(module aql
  (from where order insert update delete limit sql-literal)

  (import chicken scheme data-structures)

  (define-for-syntax (*result-fields* columns #!key quote-fields)
    (if (list? columns)
        (string-intersperse
         (map (lambda (column)
                (*quote* column quote-fields)) columns) ",")
        (*quote* columns quote-fields)))

  (define-for-syntax (*quote* field enabled)
    (let ([value (->string field)])
      (if (and (string? field) enabled)
          (string-append "\"" value "\"")
          (if (symbol? field)
              (symbol->string field)
              value))))
  
  ;; TODO: Joins
  ;; TODO: aggregation (count, all, sum, etc)
  ;; TODO: Include field renaming

  (define-syntax select
    (syntax-rules ()
      ([_ table (fields ...)] (display-blocks
                         "SELECT "
                         (if (not (null? '(fields ...)))
                             (*result-fields* '(fields ...))
                             "*")
                         " FROM "
                         (*result-fields* 'table)))))
  
  (define-syntax ->str
    (syntax-rules ()
      ([_ value] (->string 'value))))

  (define-syntax from
    (syntax-rules ()
      ([_ tables () body ...] (macrowrap
                               (select tables ())
                               body ...))
      ([_ tables (db-fields ...) body ...] (macrowrap
                                            (select tables (db-fields ...))
                                            body ...))))

  (define-syntax sql-literal
    (syntax-rules ()
      ([_ str]
       (display-blocks
	" "
	(->str str)))))
  
  (define-syntax where
    (syntax-rules ()
      ([_ forms ...] (display-blocks
                      " WHERE "
                      (where-stmt forms ...)))))
  
  (define-syntax where-subform
    (syntax-rules (?)
      ([_ (binary-operator op1 ?)] (display-blocks
                                    (*quote* op1 #t)
                                    " "
                                    (->str binary-operator)
                                    " "
                                    "?"))
      ([_ (binary-operator op1 op2)] (display-blocks
                                      (*quote* op1 #t)
                                      " "
                                      (->str binary-operator)
                                      " "
                                      (*quote* op2 #t)))
      ([_ (unary-operator op1)] (display-blocks
                                (->str unary-operator)
                                " "
                                (*quote* op1 #t)))))

  
  (define-syntax where-stmt
    (syntax-rules (?)
      ([_ (binary-operator op1 ?)] (where-subform (binary-operator op1 ?)))
      ([_ (binary-operator (form . forms) (moreforms . evenmore))] 
       (display-blocks
         (where-stmt (form . forms))
         " "
         (->str binary-operator)
         " "
         (where-stmt (moreforms . evenmore))))
      ([_ (unary-operator (form . forms))]
       (display-blocks
        (->str unary-operator)
        " "
        (where-stmt (form . forms))))
      ([_ (binary-operator op1 op2)] (where-subform (binary-operator op1 op2)))
      ([_ (unary-operator op)] (where-subform (unary-operator op)))))
  
  (define-syntax order
    (syntax-rules(by asc desc)
      ([_ by (db-fields ...) asc] (display (string-append " ORDER BY " (*result-fields* 'db-fields ...) " ASC")))
      ([_ by (db-fields ...) desc] (display (string-append " ORDER BY " (*result-fields* 'db-fields ...) " DESC")))))

  (define-syntax limit
    (syntax-rules ()
      ([_ limit] (display-blocks
                  " LIMIT "
                  limit))))

  (define-syntax macrowrap
    (syntax-rules ()
      ([_ body ...] (with-output-to-string
                      (lambda ()
                        body ...)))))

  (define-syntax update
    (syntax-rules ()
      ([_ table ((col val) ...) body ...] (update-stmt table " SET "(update-values-stmt ((col val) ...)) body ...))))

  (define-syntax update-stmt
    (syntax-rules ()
      ([_ table body ...] (macrowrap (display-blocks
                                      "UPDATE "
                                      (->str table)
                                      body ...
                                      ";")))))

  (define-syntax update-values-stmt
    (syntax-rules ()
      ([_ ((col val) ...)] (display-blocks 
                             (*result-fields* (list (string-append (->str col) " = " (*result-fields* 'val #t)) ...) #f)))))

  (define-syntax insert
    (syntax-rules ()
      ([_ table (values ...)] (insert-stmt (->string 'table) " " (insert-values (values ...))))
      ([_ table (fields ...) (values ...)] (insert-stmt (->string 'table) " (" (*result-fields* '(fields ...) #f) ") " (insert-values (values ...))))))

  (define-syntax insert-stmt
    (syntax-rules ()
      ([_ body ...] (macrowrap (display-blocks
                                "INSERT INTO "
                                body ...
                                ";")))))
                    
  (define-syntax insert-values
    (syntax-rules ()
      ([_ (values ...)] (display-blocks
                         "VALUES ("
                         (*result-fields* '(values ...) #t)
                         ")"))))

  (define-syntax display-blocks
    (syntax-rules ()
      ([_ body ...] (begin
                      (display body) ... ""))))
 

  (define-syntax delete
    (syntax-rules ()
      ([_ table body ...] (delete-stmt (->string 'table) body ...))))

  (define-syntax delete-stmt
    (syntax-rules ()
      ([_ body ...] (macrowrap (display-blocks
                                "DELETE FROM "
                                body ...
                                ";")))))

  
  )                                     ; Module