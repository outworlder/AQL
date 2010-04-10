(module aql
  (from where order insert update SELECT WHERE)

  (import chicken scheme srfi-1 data-structures)

  ;; TODO: Joins
  ;; TODO: Where
  ;; TODO: Order by
  ;; TODO: LIMIT
  ;; TODO: aggregation (count, all, sum, etc)
  ;; TODO: Include field renaming
  (define (SELECT tables #!key (fields "*"))
    (display (string-append "SELECT " (*result-fields* fields) " FROM " (*result-fields* tables))))

  (define-syntax ->str
    (syntax-rules ()
      ([_ value] (->string 'value))))

  (define-syntax from
    (syntax-rules ()
      ([_ tables () body ...] (macrowrap
                               (SELECT 'tables)
                               body ...))
      ([_ tables (db-fields ...) body ...] (macrowrap
                                            (SELECT (quote tables) fields: '(db-fields ...))
                                            body ...))))

  (define-syntax where
    (syntax-rules ()
      ([_ (binary-operator field value) body ...] (begin
                                                    (WHERE '(binary-operator field value))
                                                    body ... ))
      ([_ (unary-operator value) body ...] (begin
                                             (WHERE-unary '(unary-operator value))
                                             body ...))))

  (define-syntax order
    (syntax-rules(by asc desc)
      ([_ by (db-fields ...) asc] (display (string-append " ORDER BY " (*result-fields* 'db-fields ...) " ASC ")))
      ([_ by (db-fields ...) desc] (display (string-append " ORDER BY " (*result-fields* 'db-fields ...) " DESC ")))))


  (define-syntax macrowrap
    (syntax-rules ()
      ([_ body ...] (with-output-to-string
                      (lambda ()
                        body ...)))))
  
  ;; (= "test" "blah") -> " 'test' = 'blah'
  (define (WHERE expression)
    (let ([operator (car expression)]
          [operand1 (cadr expression)]
          [operand2 (caddr expression)])
      (display (string-append " WHERE " (*quote* operand1 #t) (->string operator) (*quote* operand2 #t)))))

  (define (WHERE-unary expression)
    (let ([operator (car expression)]
          [operand (cadr expression)])
      (display (string-append " WHERE " (->string operator) (*quote* operand #t)))))

  (define (*result-fields* columns #!key quote-fields)
    (if (list? columns)
        (string-intersperse
         (map (lambda (column)
                (*quote* column quote-fields)) columns) ",")
        (*quote* columns quote-fields)))

  (define (*quote* field enabled)
    (let ([value (->string field)])
      (if (and (string? field) enabled)
          (string-append "\"" value "\"")
          value)))

  (define (alist? list)
    (and (list? list) (every pair? list)))

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
                             (*result-fields* (list (string-append (->str col) " = " (*result-fields* val #t)) ...) #f)))))

  (define-syntax insert
    (syntax-rules ()
      ([_ table (values ...)] (insert-stmt (->string 'table) " " (insert-values (values ...))))))

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
 

  )                                     ; Module