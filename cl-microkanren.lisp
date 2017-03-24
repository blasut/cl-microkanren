;;;; cl-microkanren.lisp

(in-package #:cl-microkanren)

;; helpers
(defun pair? (v) (consp v))

(defun eqv? (x y) (eql x y))

(defun null? (x) (null x))

(let ((f (lambda (x) (= x 3)))
      (list '((3 a) (1 b) (4 c) (3 b))))
  (loop :for cons :in list
     :when (funcall f (car cons)) :do (return cons)))

(let ((f (lambda (x) (= x 3)))
      (list '((3 a) (1 b) (4 c) (3 b))))
  (assp f list))

(defun assp (f list)
  (unless (= 0 (length list))
    (loop :for cons :in list
     :when (funcall f (car cons)) :do (return cons))))
  
(defun my-aref (v i)
  (if (= 0 (length v))  
      nil
      (aref v i)))

;;; "cl-microkanren" goes here. Hacks and glory await!

(defun var (c) (vector c))
(defun var? (x) (vectorp x))
(defun var=? (x1 x2) (eql (my-aref x1 0) (my-aref x2 0)))

;; u == var(?)
;; s == substiution-map?
(defun walk (u s)
  (let ((pr (and (var? u)
                 (assp (lambda (v) (var=? u v)) s))))
    (if pr
        (walk (cdr pr) s)
        u)))
  
(defun ext-s (x v s)
  `((,x . ,v) . ,s))

(defun == (u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(defun unit (s/c) (cons s/c mzero))
(defvar mzero '())

(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (t (and (eqv? u v) s)))))

(defun call/fresh (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      (funcall (funcall f (var c)) `(,(car s/c) . ,(+ c 1))))))

(defun disj (g1 g2)
  (lambda (s/c)
    (mplus (funcall g1 s/c) (funcall g2 s/c))))

(defun conj (g1 g2)
  (lambda (s/c)
    (bind (funcall g1 s/c) g2)))

(defun mplus ($1 $2)
  (cond
    ((null? $1) $2)
    ((functionp $1) (lambda () (mplus $2 (funcall $1))))
    (t (cons (car $1) (mplus (cdr $1) $2)))))

(defun bind ($ g)
  (cond
    ((null? $) mzero)
    ((functionp $) (lambda () (bind (funcall $) g)))
    (t (mplus (funcall g (car $)) (bind (cdr $) g)))))

;; macros for nicer use, and more minikamren style

(defmacro fresh (syms &body body)
  `(let (,@(mapcar (lambda (x)
                     `(,x (var ,(symbol-name x))))
                   syms))
     (conj+ ,@body)))

(defmacro Zzz (g)
  `(lambda (s/c)
     (lambda ()
       (funcall ,g s/c))))
  
(funcall (Zzz (== 1 2))
  empty-state)

(funcall (funcall
           (LAMBDA (S/C)
            (LAMBDA () (FUNCALL (== 1 2) S/C)))
           empty-state))



;;;; playground

;; example query in scheme
;; (define empty-state ' (() . 0))
;; ((call/fresh (λ (q) (≡ q 5))) empty-state)
;; => ((((#(0) . 5)) . 1))
(defparameter empty-state '(() . 0))
(funcall (call/fresh (lambda (q) (== q 5))) empty-state)
;; => ((((#(0) . 5)) . 1)) 

(let* ((a (var "a"))
       (b (var "b"))
       (s empty-state))
  (funcall (conj
            (== a 1)
            (== a b))
         s))

(defparameter a-and-b
  (conj
    (call/fresh (lambda (a) (== a 7)))
    (call/fresh (lambda (b) (disj (== b 5) (== b 6))))))

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


(funcall
 (fresh (a b)
    (== a 42)
    (== b a))
 empty-state)  
