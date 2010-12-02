(module aql
  (from where order insert update delete limit sql-literal)

  (import chicken scheme data-structures)

  (define-syntax aql
    (syntax-rules ()
      ([_ sexp]
       (quote sexp))))

  (define (aql->sql sexp)
    ())

  (define (select-stmt table columns #!key where)
    (string-append "select " (columns columns) table))

  (define (where-stmt expression)
    (string-intersperse (list "where" (expression expression))))

  (define (columns col-list)
    (string-intersperse  (map (lambda (column))
                              (append "[" (->string column) "]") col-list) ","))

  (define (infix-postfix))
  
  )                                     ; Module



'(select
 ((join inner)
  (where
   (* / % + - << >> & | < <=  > >= = == != <> is not in like glob match regexp and or between like is-null is-not-null not in exists collate (cast as)))
  (order-by
   (asc desc))
  (group-by
   (having))
  (limit)
  (offset)))

;;; Examples
;;; (aql (select <table> (col1 col2 col3)))
;;; (aql (select <table> (*)))
;;; (aql (select <table> (*) (where (= id 1)))
;;; This should be transformed to:
;;; (aql->sql (aql `(select <table> (*) (where (= ,id ,1))))
;;; More:
;;; (aql (select (join 'inner (<table> key)
;;;              (join (<table2> key)))
;;; (select <table> (*)
;;;   (where (= ,id ,1)))

;;; Select takes up to 3 parameters
;;; (select table columns rest)
;;; Where takes up to 2 parameters
;;; (where <expression> rest)
