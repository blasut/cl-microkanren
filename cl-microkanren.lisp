;;;; cl-microkanren.lisp

(in-package #:cl-microkanren)

;; helpers
(defun pair? (v)
  (if (consp v)
      t
      'false))
(defun eqv? (x y) (eql x y))
(defun null? (x) (null x))

(defun assp (pred alist)
  (if (null? alist)
      'false
      (loop :for cons :in alist
         :if (not (equal 'false (funcall pred (car cons)))) :do (return cons)
         :finally (return 'false))))
  
(defun my-aref (v i)
  (if (= 0 (length v))  
      nil
      (aref v i)))

(defmacro scm-if (term then else)
  `(if (equal ,term 'false)
      ,else
      ,then))

(defmacro scm-and (&rest goals)
  (match goals
    ((cons (quote nil) ()) `t)
    ((cons (quote (quote nil)) ()) `t)
    ((cons (place g) '())  `(scm-if ,g ,g 'false))
    ((cons (place g) rest) `(scm-if ,g (scm-and ,@rest) 'false))))

;;; "cl-microkanren" goes here. Hacks and glory await!

(defun lvar (c) (vector c))
(defun lvar? (c) (if (vectorp c) t 'false))
(defun lvar=? (x1 x2)
  (scm-and
    (lvar? x1) (lvar? x2) (eql (aref x1 0) (aref x2 0))))

;; u == lvar(?)
;; s == substiution-map?
(defun walk (u s)
  (let ((pr (scm-and (lvar? u) (assp (lambda (v) (lvar=? u v)) s))))
    (scm-if pr
        (walk (cdr pr) s)
        u)))
  
(defun ext-s (x v s)
  `((,x . ,v) . ,s))

(defvar mzero '())
(defun unit (s/c) (cons s/c mzero))

(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((not (equal 'false (scm-and (lvar? u) (lvar? v) (lvar=? u v)))) s)
      ((lvar? u) (ext-s u v s))
      ((lvar? v) (ext-s v u s))
      ((not (equal 'false (scm-and (pair? u) (pair? v))))
       (let ((s (unify (car u) (car v) s)))
         (scm-and s (unify (cdr u) (cdr v) s))))
      (t (scm-and (eqv? u v) s)))))

(defun call/fresh (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      (funcall (funcall f (lvar c)) `(,(car s/c) . ,(+ c 1))))))

(defun == (u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (scm-if s (unit `(,s . ,(cdr s/c))) mzero))))

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
