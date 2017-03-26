;;;; cl-microkanren.lisp

(in-package #:cl-microkanren)

;; helpers
(defun pair? (v) (consp v))
(defun eqv? (x y) (eql x y))
(defun null? (x) (null x))

;;; "cl-microkanren" goes here. Hacks and glory await!

(defun lvar (c) (vector c))
(defun lvar? (c) (vectorp c))
(defun lvar=? (x1 x2) (eql (aref x1 0) (aref x2 0)))

;; u == lvar(?)
;; s == substiution-map?
(defun walk (u s)
  (if (and (lvar? u)
           (pair? s)
           (position u s :key #'car :test #'equalp))
     (walk (cdr (elt s (position u s :key #'car :test #'equalp))) u)
     u))

(defun ext-s (x v s)
  `((,x . ,v) . ,s))

(defparameter mzero '())
(defun unit (s/c) (cons s/c mzero))

(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (lvar? u) (lvar? v) (lvar=? u v)) s)
      ((lvar? u) (ext-s u v s))
      ((lvar? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (if (null s) ;; even if empty-list, keep checking. Shouldnt infinite-loop because of cdr
           (unify (cdr u) (cdr v) s)  
           (and s (unify (cdr u) (cdr v) s)))))
      (t (and (eqv? u v) s)))))

(defun call/fresh (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      (funcall (funcall f (lvar c)) `(,(car s/c) . ,(+ c 1))))))

(defun == (u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

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

(defun disj (g1 g2)
  (lambda (s/c)
    (mplus (funcall g1 s/c) (funcall g2 s/c))))

(defun conj (g1 g2)
  (lambda (s/c)
    (bind (funcall g1 s/c) g2)))

;;; "cl-microkanren" stops here
