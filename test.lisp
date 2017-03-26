(in-package #:cl-microkanren)

;;;; tests from the repo
(defmacro test-check (title tested-expression expected-result)
  `(progn
     (format t "Testing ~s\n" ,title)
     (let* ((expected ,expected-result)
            (produced ,tested-expression))
       (or (equalp expected produced)
           (format nil "Failed: ~a~%Expected: ~a~%Computed: ~a~%" 'tested-expression expected produced)))))

(defparameter a-and-b
  (conj
    (call/fresh (lambda (a) (== a 7)))
    (call/fresh (lambda (b) (disj (== b 5) (== b 6))))))

(defun fives (x)
  (disj
   (== x 5)      
   (lambda (a/c)
      (lambda ()
        (funcall (fives x) a/c)))))


(test-check "second-set t1"
  (let (($ (call/empty-state (call/fresh (lambda (q) (== q 5))))))
    (car $))
  '(((#(0) . 5)) . 1))

(test-check "second-set t2"
  (let (($ (call/empty-state (call/fresh (lambda (q) (== q 5))))))
    (cdr $))
  '())

(test-check "second-set t3"
  (let (($ (call/empty-state a-and-b)))
    (car $))
  '(((#(1) . 5) (#(0) . 7)) . 2))

(test-check "second-set t4"
  (let (($ (call/empty-state a-and-b)))
    (car (cdr $)))
  '(((#(1) . 6) (#(0) . 7)) . 2))

(test-check "second-set t5"
  (let (($ (call/empty-state a-and-b)))
    (cdr (cdr $)))
  '())

(test-check "who cares"
  (let (($ (call/empty-state (call/fresh (lambda (q) (fives q))))))
    (take 1 $))
  '((((#(0) . 5)) . 1)))

(test-check "super simple"
  (let (($ (call/empty-state (call/fresh (lambda(q) (== q 5))))))
    (take 1 $))
  '((((#(0) . 5)) . 1)))

(test-check "appendo"
  (run* (q x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5)))
  '((NIL (1 2 3 4 5)) ((1) (2 3 4 5)) ((1 2) (3 4 5)) ((1 2 3) (4 5))
           ((1 2 3 4) (5)) ((1 2 3 4 5) NIL)))

(test-check "acorn"
  (run* (q) (== '(a c o r n) q))
  '((A C O R N)))

(defun fives (x) (disj (== 5 x) (zzz (fives x))))

(defun test-recursion ()
  (let ((res (call/empty-state (call/fresh (lambda(q) (fives q))))))
      (assert (equalp '(((#(0) . 5)) . 1) (car res)))
      (assert (functionp (cdr res)))))

(test-recursion)

(defun fives+ (x) (disj+ (== 5 x) (fives+ x)))
(defun sixes+ (x) (disj+ (== 6 x) (sixes+ x)))
(defun fives-and-sixes+ (x)
  (disj+ (conj+ (fives+ x) (== x 5)))
  (sixes+ x))

;; treat '() as truthy
(assert (scm-and '() 'a 'b 'c) 'c)
(assert (scm-and '() 'a 'b 'false 'c) 'b)

(assert (scm-orr '() 'a 'b 'false 'c) 'a)
(assert (scm-orr 'false 'false '() 'false 'c) '())
(assert (scm-if 'false 'a 'b) 'false)
(assert (scm-if '() 'a 'b) 'a)

(defun scm-and (&rest args)
  (print args)
  (loop :for el :in args
     :for i :from 0
     :collecting el :into forms
     :if (equal el 'false) :do (return (elt forms (1- i)))
     :finally (return (elt forms i))))
                  
(equal 'b (scm-and '() 'a 'b 'false 'c))
(equal 'c (scm-and '() 'a 'b 'c))
