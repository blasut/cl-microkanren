;;;; cl-microkanren.lisp

(in-package #:cl-microkanren)

;; helpers
(defun pair? (v) (consp v))

(defun eqv? (x y) (eql x y))

(defun null? (x) (null x))

(defun assp (f list)
  (unless (= 0 (length list))
    (loop :for cons :in list
     :when (funcall f (car cons)) :do (return cons))))
  
(defun my-aref (v i)
  (if (= 0 (length v))  
      nil
      (aref v i)))

;;; "cl-microkanren" goes here. Hacks and glory await!

(defun lvar (c) (vector c))
(defun lvar? (x) (vectorp x))
(defun lvar=? (x1 x2) (eql (my-aref x1 0) (my-aref x2 0)))

;; u == lvar(?)
;; s == substiution-map?
(defun walk (u s)
  (let ((pr (and (lvar? u)
                 (assp (lambda (v) (lvar=? u v)) s))))
    (if pr
        (walk (cdr pr) s)
        u)))
  
(defun ext-s (x v s)
  `((,x . ,v) . ,s))

(defun unit (s/c) (cons s/c mzero))
(defvar mzero '())

(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (lvar? u) (lvar? v) (lvar=? u v)) s)
      ((lvar? u) (ext-s u v s))
      ((lvar? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (t (and (eqv? u v) s)))))

(defun call/fresh (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      (funcall (funcall f (lvar c)) `(,(car s/c) . ,(+ c 1))))))

(defun == (u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

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

;;; "cl-microkanren" stops here
