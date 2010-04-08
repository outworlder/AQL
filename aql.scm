(module aql
  (SELECT INSERT)

  (import chicken scheme srfi-1 data-structures)

  ;; TODO: Joins
  ;; TODO: Where
  ;; TODO: Order by
  ;; TODO: LIMIT
  ;; TODO: aggregation (count, all, sum, etc)
  ;; TODO: Include field renaming
  (define (SELECT tables #!key (fields "*"))
    (string-append "SELECT " (*result-fields* fields) " FROM " (*result-fields* tables)))

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
  (define (INSERT table values #!key fields (marks "?"))
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