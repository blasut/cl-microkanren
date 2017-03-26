

;;;; tests from the repo
(defmacro test-check (title tested-expression expected-result)
  `(progn
     (format t "Testing ~s\n" ,title)
     (let* ((expected ,expected-result)
            (produced ,tested-expression))
       (or (equalp expected produced)
           (format nil "Failed: ~a~%Expected: ~a~%Computed: ~a~%" 'tested-expression expected produced)))))


(defun appendo (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== `(,a . ,d) l)
            (call/fresh
             (lambda (res)
               (conj
                (== `(,a . ,res) out)
                (lambda (s/c)
                  (lambda ()
                    (funcall (appendo d s res) s/c)))))))))))))

(defun call-appendo ()
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (appendo l s out)
               (== `(,l ,s ,out) q)))))))))))

(defun ground-appendo ()
  (appendo '(a) '(b) '(a b)))

(defmacro test-run (form)
  `(funcall ,form empty-state)) 

(test-check "second-set t1"
  (let (($ (test-run (call/fresh (lambda (q) (== q 5))))))
    (car $))
  '(((#(0) . 5)) . 1))

(test-check "second-set t2"
  (let (($ (test-run (call/fresh (lambda (q) (== q 5))))))
    (cdr $))
  '())

(test-check "second-set t3"
  (let (($ (test-run a-and-b)))
    (car $))
  '(((#(1) . 5) (#(0) . 7)) . 2))

(test-check "second-set t4"
  (let (($ (test-run a-and-b)))
    (car (cdr $)))
  '(((#(1) . 6) (#(0) . 7)) . 2))

(test-check "second-set t5"
  (let (($ (test-run a-and-b)))
    (cdr (cdr $)))
  '())

(test-check "who cares"
  (let (($ (test-run (call/fresh (lambda (q) (fives q))))))
    (take 1 $))
  '((((#(0) . 5)) . 1)))

(test-check "ground appendo"
  (car (funcall (funcall (ground-appendo) empty-state)))
  '(((#(2) b) (#(1)) (#(0) . a)) . 3))

(test-check "appendo"
  (funcall (call-appendo) empty-state)
  '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4)
    (((#(0) #(1) #(2) #(3)) (#(2) . #(6)) (#(5)) (#(3) #(4) . #(6)) (#(1) #(4) . #(5))) . 7)))


;;;; playground

;; example query in scheme
;; (define empty-state ' (() . 0))
;; ((call/fresh (λ (q) (≡ q 5))) empty-state)
;; => ((((#(0) . 5)) . 1))
(defparameter empty-state '(() . 0))
(funcall (call/fresh (lambda (q) (== q 5))) empty-state)
;; => ((((#(0) . 5)) . 1)) 

(let* ((a (lvar "a"))
       (b (lvar "b"))
       (s empty-state))
  (funcall (conj
            (== a 1)
            (== a b))
         s))

(funcall a-and-b empty-state)
;; result from call:
;; ((((#(1) . 5) (#(0) . 7)) . 2) (((#(1) . 6) (#(0) . 7)) . 2))
;; in paper:
;; ((((#(1) . 5) (#(0) . 7)) . 2) (((#(1) . 6) (#(0) . 7)) . 2))


(funcall
  (conj
    (call/fresh (lambda (a) (disj (== a 7) (== a 1))))
    (call/fresh (lambda (b) (disj (== b 5) (== b 6)))))
  empty-state)
