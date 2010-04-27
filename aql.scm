(module aql
  (from where order insert update delete limit)

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
  (define-syntax where
    (syntax-rules ()
      ([_ (binary-operator op1 op2) body ...] (display-blocks
                                               " WHERE "
                                               (where-handler '(binary-operator op1 op2) "")
                                               body ... ))
      ([_ (unary-operator op) body ...] (display-blocks
                                         " WHERE "
                                         (where-handler '(unary-operator op1) "")
                                         body ...))))

  (define-for-syntax (where-handler tree fragment)
    (if (null? tree)
        fragment
        (let ([operator (car tree)]
              [operand1 (cadr tree)]
              [operand2 (if (>= (length tree) 3)
                            (caddr tree)
                            #f)])
          (if (eq? operator 'quote)     ;This is ugly
              (->string operand1)
              (string-append fragment 
                             (if operand2
                                 (begin
                                   (string-append
                                    (if (list? operand1)
                                        (where-handler operand1 fragment)
                                        (eval operand1))
                                    " " (->string operator) " "
                                    (if (list? operand2)
                                        (where-handler operand2 fragment)
                                        (*quote* (eval operand2) #t))))
                                 (begin
                                   (string-append
                                    " " (->string operator) " "
                                    (if (list? operand1)
                                        (where-handler operand1 fragment)
                                        (*quote* operand1 #t))))))))))
  
  (define-syntax order
    (syntax-rules(by asc desc)
      ([_ by (db-fields ...) asc] (display (string-append " ORDER BY " (*result-fields* 'db-fields ...) " ASC ")))
      ([_ by (db-fields ...) desc] (display (string-append " ORDER BY " (*result-fields* 'db-fields ...) " DESC ")))))

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
      ([_ table ((col val) ...) body ...] (update-stmt table " SET "(update-values-stmt ((col val) ...)) " " body ...))))

  (define-syntax update-stmt
    (syntax-rules ()
      ([_ table body ...] (macrowrap (display-blocks
                                      "UPDATE "
                                      (->str table)
                                      " "
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
      ([_ table body ...] (delete-stmt (->string 'table) " " body ...))))

  (define-syntax delete-stmt
    (syntax-rules ()
      ([_ body ...] (macrowrap (display-blocks
                                "DELETE FROM "
                                body ...
                                ";")))))

  
  )                                     ; Module