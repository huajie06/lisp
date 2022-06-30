"
(eq 'a 'b) is false. 
(eq 'a 'a) is true. 
(eq 3 3) might be true or false, depending on the implementation. 
(eq 3 3.0) is false. 
(eq 3.0 3.0) might be true or false, depending on the implementation. 
(eq #c(3 -4) #c(3 -4)) 
might be true or false, depending on the implementation. 
(eq #c(3 -4.0) #c(3 -4)) is false. 
(eq (cons 'a 'b) (cons 'a 'c)) is false. 
(eq (cons 'a 'b) (cons 'a 'b)) is false. 
(eq '(a . b) '(a . b)) might be true or false. 
(progn (setq x (cons 'a 'b)) (eq x x)) is true. 
(progn (setq x '(a . b)) (eq x x)) is true. 
(eq #\A #\A) might be true or false, depending on the implementation. 
(eq "Foo" "Foo") might be true or false. 
(eq "Foo" (copy-seq "Foo")) is false. 
(eq "FOO" "foo") is false.


(eql 'a 'b) is false. 
(eql 'a 'a) is true. 
(eql 3 3) is true. 
(eql 3 3.0) is false. 
(eql 3.0 3.0) is true. 
(eql #c(3 -4) #c(3 -4)) is true. 
(eql #c(3 -4.0) #c(3 -4)) is false. 
(eql (cons 'a 'b) (cons 'a 'c)) is false. 
(eql (cons 'a 'b) (cons 'a 'b)) is false. 
(eql '(a . b) '(a . b)) might be true or false. 
(progn (setq x (cons 'a 'b)) (eql x x)) is true. 
(progn (setq x '(a . b)) (eql x x)) is true. 
(eql #\A #\A) is true. 
(eql "Foo" "Foo") might be true or false. 
(eql "Foo" (copy-seq "Foo")) is false. 
(eql "FOO" "foo") is false.


(equal 'a 'b) is false. 
(equal 'a 'a) is true. 
(equal 3 3) is true. 
(equal 3 3.0) is false. 
(equal 3.0 3.0) is true. 
(equal #c(3 -4) #c(3 -4)) is true. 
(equal #c(3 -4.0) #c(3 -4)) is false. 
(equal (cons 'a 'b) (cons 'a 'c)) is false. 
(equal (cons 'a 'b) (cons 'a 'b)) is true. 
(equal '(a . b) '(a . b)) is true. 
(progn (setq x (cons 'a 'b)) (equal x x)) is true. 
(progn (setq x '(a . b)) (equal x x)) is true. 
(equal #\A #\A) is true. 
(equal "Foo" "Foo") is true. 
(equal "Foo" (copy-seq "Foo")) is true. 
(equal "FOO" "foo") is false.


(equalp 'a 'b) is false. 
(equalp 'a 'a) is true. 
(equalp 3 3) is true. 
(equalp 3 3.0) is true. 
(equalp 3.0 3.0) is true. 
(equalp #c(3 -4) #c(3 -4)) is true. 
(equalp #c(3 -4.0) #c(3 -4)) is true. 
(equalp (cons 'a 'b) (cons 'a 'c)) is false. 
(equalp (cons 'a 'b) (cons 'a 'b)) is true. 
(equalp '(a . b) '(a . b)) is true. 
(progn (setq x (cons 'a 'b)) (equalp x x)) is true. 
(progn (setq x '(a . b)) (equalp x x)) is true. 
(equalp #\A #\A) is true. 
(equalp "Foo" "Foo") is true. 
(equalp "Foo" (copy-seq "Foo")) is true. 
(equalp "FOO" "foo") is true.
"
