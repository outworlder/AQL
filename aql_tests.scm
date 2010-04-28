(use test)
(use aql)

(test-group "select"
            (test "simple-select" "SELECT * FROM test" (from test ()))
            (test "select with fields" "SELECT field1,field2 FROM test" (from test (field1 field2)))
            (test "select with fields and simple where" "SELECT field1,field2 FROM test WHERE field1 = 1" (from test (field1 field2) (where (= 'field1 1))))
            (test "select, fields, where with and" "SELECT field1,field2 FROM test WHERE field1 = 1 and field2 > 10" (from test (field1 field2) (where (and (= 'field1 1) (> 'field2 10)))))
            (test "select, simple where, order by" "SELECT field1,field2 FROM test WHERE field1 = 1 ORDER BY field1 DESC" (from test (field1 field2) (where (= 'field1 1)) (order by (field1) desc)))
            (test "select, simple where, order by asc, limit" "SELECT field1,field2 FROM test WHERE field1 = 1 ORDER BY field1 ASC LIMIT 1" (from test (field1 field2) (where (= 'field1 1)) (order by (field1) asc) (limit 1))))

(test-group "insert"
            (test "simple insert" "INSERT INTO test VALUES (1,1);" (insert test (1 1)))
            (test "insert with named fields" "INSERT INTO test (field1,field2) VALUES (1,1);" (insert test (field1 field2) (1 1))))

(test-group "delete"                    ; Whitespace issues
            (test "delete everything" "DELETE FROM test;" (delete test))
            (test "delete, simple where" "DELETE FROM test WHERE field1 = 1;" (delete test (where (= 'field1 1)))))

(test-group "update"                    ; Whitespace issues
            (test "update everything, 1 field" "UPDATE test SET field1 = 1;" (update test ((field1 1))))
            (test "update everything, more than 1 field" "UPDATE test SET field1 = 1, field2 = 2;" (update test ((field1 1) (field2 2))))
            (test "update 1 row, 1 field" "UPDATE test SET field2 = 1 WHERE field1 = 1;" (update test ((field1 1)) (where (= 'field1 1)))))