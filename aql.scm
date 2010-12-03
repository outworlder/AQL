(module aql
  (select)

  (import chicken scheme data-structures srfi-1)

  '(* / % + - << >> & | < <=  > >= = != <> is not in like glob match regexp and or between like is-null is-not-null not in exists collate (cast as))
  (define where-operators
    '((= binary)
      (* binary)
      (/ binary)
      (% binary)
      (+ binary)
      (- binary)
      (< binary)
      (> binary)
      (= binary)
      (|| binary)
      (<> binary)
      (is binary)
      (is-not binary)
      (in binary)
      (not unary)
      (glob unary function)
      (match unary function)
      (regexp unary function)
      (and binary)
      (or binary)
      (between binary)
      (like binary)
      (exists unary select)
      (collate unary function)))

  (define (operator? element)
    (let ([op (if (list? element)
                  (car element)
                  element)])
      (assoc element where-operators)))

  (define (operator->string element)
    )
  
  (define (expression expression)
    (fold (lambda (x xs)
            (if (operator? element)
                (operator->string element))) ))

  (define (has-sublists? list)
    (any list? list))
  
  (define (select table #!key columns where limit offset)
    (string-intersperse "select"
                        (column-list columns)
                        "from" table
                        (if where
                            (where where))
                        (if limit
                            (limit limit))
                        (if offset
                            (offset offset))))

  (define (where expression)
    ())

;  (and  (like 'field "%blah")
 ;       (= 'field 1)
  ;      (in 'id (select table columns: '(id))))
  

  (define (column-list columns)
    (if (eq? columns '*)
        "*"
        (string-intersperse (map (lambda (column)
                                   (string-append "[" column "]")) columns) ", ")))

  (define (consume expression)
    (fold ))
  
  
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
