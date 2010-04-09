(module aql
  (from where SELECT WHERE)

  (import chicken scheme srfi-1 data-structures)

  ;; TODO: Joins
  ;; TODO: Where
  ;; TODO: Order by
  ;; TODO: LIMIT
  ;; TODO: aggregation (count, all, sum, etc)
  ;; TODO: Include field renaming
  (define (SELECT tables #!key (fields "*"))
    (display (string-append "SELECT " (*result-fields* fields) " FROM " (*result-fields* tables))))

  (define-syntax from
    (syntax-rules ()
      ([_ tables () body ...] (with-output-to-string
                                (lambda ()
                                  (SELECT 'tables)
                                  body ...)))
       ([_ tables (db-fields ...) body ...] (with-output-to-string
                                              (lambda ()
                                                (SELECT (quote tables) fields: '(db-fields ...))
                                                body ...)))))

  (define-syntax where
    (syntax-rules ()
      ([_ (binary-operator field value) body ...] (begin
                                                     (WHERE '(binary-operator field value))
                                                     body ... ))
      ([_ (unary-operator value) body ...] (begin
                                             (WHERE '(unary-operator value))
                                             body ...))))

  ;; (= "test" "blah") -> " 'test' = 'blah'
  (define (WHERE expression)
    (let ([operator (car expression)]
          [operand1 (cadr expression)]
          [operand2 (caddr expression)])
      (display (string-append " WHERE " (*quote* operand1 #t) (->string operator) (*quote* operand2 #t)))))

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

  ;; alist of field values or just the values
  (define (insert table values #!key fields (marks "?"))
    (let ([fields-string (if fields
                             (string-append "("
                                            (*result-fields* fields)
                                            ")")
                             "")]
          [values-string (string-append "("
                                        (if (eqv? values '?)
                                            (begin
                                              (unless (list? fields)
                                                (signal
                                                 (make-property-condition
                                                  'exn
                                                  'message
                                                  "If using markers, the list of fields must be informed.")))
                                              (*result-fields* (map (lambda (value) marks) fields)))
                                            (*result-fields* values quote-fields: #t)) ")")])
      (string-append "INSERT INTO " (->string table) " " fields-string " VALUES " values-string)))


  )                                     ; Module