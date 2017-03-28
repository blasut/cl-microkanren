(in-package #:cl-microkanren)

(setq prove:*default-test-function* #'equalp)

(subtest "walk"
  (subtest "nested references"
    ;; walk should get the actual value of the lvar if it exists
    (is (walk (lvar 0)
              (ext-s (lvar 0) (lvar 1) (ext-s (lvar 1) 7 nil)))
        7)

    (is (walk (lvar 0)
              (ext-s (lvar 0) (lvar 1)
                     (ext-s (lvar 1) (lvar 2)
                            (ext-s (lvar 2) (lvar 3) nil))))

        (lvar 3))

    (is (walk (lvar 0)
              (ext-s (lvar 0) (lvar 1)
                     (ext-s (lvar 1) 5
                            (ext-s (lvar 2) (lvar 3) nil))))
        5))

  (subtest "should treat nil as values"
    (is (walk '() (ext-s (lvar 0) '() nil))
        '())

    (is (walk (lvar 1) (ext-s (lvar 0) (lvar 1)
                              (ext-s (lvar 1) '() nil)))
        '())

    (is (walk (lvar 1) nil)
        '())

    (is (walk (lvar 0) (ext-s (lvar 0) '() nil))
        '()))

  (subtest "things that are not lvars should be returned"
    (is (walk 5 (ext-s (lvar 0) 5 nil))
        5)

    (is (walk '(123) (ext-s (lvar 0) 5 nil))
        '(123))

    (is (walk 5 nil)
        5)))


(subtest "unify"
  (subtest "unify should return false instead of nil when false"
    (is (unify 1 2 empty-state)
        'false)

    (is (unify 1 2 nil)
        'false)

    (is (unify 1 1 empty-state)
        empty-state)

    (is (unify #(0) 6 '((#(0) . 5)))
        'false)

    (is (unify 1 1 '())
        '())))

(subtest "=="
  (subtest "should return empty list for no match"
    (is (call/empty-state (call/fresh (lambda (b) (conj (== b 5) (== b 6)))))
        '())

    (is (call/empty-state (call/fresh (lambda (b) (conj (== b '())
                                                        (== b '())))))
        '((((#(0))) . 1)))

    (UNIFY #(0) '(#(1) #(2)) '((#(2) 1 2) (#(1))))

    (UNIFY '(#(3) . #(5)) '(1 2) '((#(1) #(3) . #(4))))

    ;; this run:
    (run 1 (q) (fresh (x)
                      (== `(a c ,x r n) q)
                      (== x '(1 2 3))))
    (UNIFY (A C #(1) R N) #(0) NIL)
    (WALK (A C #(1) R N) NIL)
    (EXT-S #(0) (A C #(1) R N) NIL)

    (UNIFY #(1) (1 2 3) ((#(0) A C #(1) R N)))
    (WALK #(1) ((#(0) A C #(1) R N)))
    (WALK (1 2 3) ((#(0) A C #(1) R N)))

    (EXT-S #(1) (1 2 3) ((#(0) A C #(1) R N)))

    
    ;; this run:
    (run 1 (q) (fresh (x y)
                      (== `(a c ,x r n) q)
                      (== `(m j a ,y) x)
                      (== y '(1 2 3))))

    (== (A C #(1) R N) #(0))
    (UNIFY (A C #(1) R N) #(0) NIL)

    (== (M J A #(2)) #(1))
    (UNIFY (M J A #(2)) #(1) ((#(0) A C #(1) R N)))


    ;; this run:
    (is (unify '(A B #(1)) '(A B #(0)) NIL)
        '((#(1) . #(0))))

    ;; the value retuned is _.0
    ;; it is because #(1) is pointing to #(0)
    ;; #(0) is pointning to nothing
    ;; which means it is unbound
    (run 1 (q) (fresh (x)
                      (== `(a b ,x) `(a b ,q))))

    (== (A B #(1)) (A B #(0)))
    (UNIFY (A B #(1)) (A B #(0)) NIL)
    (WALK (A B #(1)) NIL)
    (WALK (A B #(0)) NIL)

    ;; both U & V are pairs in this scenario
    (UNIFY A A NIL)               ;; returns NIL == empty-state
    (UNIFY (B #(1)) (B #(0)) NIL) ;; returns nil
    ;; because u and v ar pairs, we walk again with the car
    (UNIFY B B NIL)
    ;; ... and the cdr
    (UNIFY (#(1)) (#(0)) NIL)
    ;; which are pairs...
    (UNIFY #(1) #(0) NIL)
    ;; they are both lvars but not equal
    (LVAR=? #(1) #(0))
    ;; so extend the state, pointing #(1) to #(0)
    (EXT-S #(1) #(0) NIL)
    ;; then check the CDR
    (UNIFY NIL NIL ((#(1) . #(0))))
    ;; this returns the state unchanged
    ;; because (eqv? nil nil) == T
    

    ;; this run
    (run 1 (q) (fresh (x y)
                      (== q x)
                      (== `(a b ,x) `(a b c)) ))

    
    (print "end")))

(finalize)

