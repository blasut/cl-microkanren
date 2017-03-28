;;;; cl-microkanren.lisp

(in-package #:cl-microkanren)

;; helpers
(defun pair? (v) (consp v))
(defun eqv? (x y) (eql x y))
(defun null? (x) (null x))
(defun my-pos (u s) (position u s :key #'car :test #'equalp))

;;; "cl-microkanren" goes here. Hacks and glory await!

(defun lvar (c) (vector c))
(defun lvar? (c) (vectorp c))
(defun lvar=? (x1 x2) (eql (aref x1 0) (aref x2 0)))

;; u == lvar(?)
;; s == substiution-map?
(defun walk (u s)
  (if (and (lvar? u)
           ;; unles the subs-map is a pair, we dont need to check it
           (pair? s)
           ;; my-pos makes sure we return even if the u is ()
           ;; () is falsey in CL
           (my-pos u s))
      ;; recurse the sub-map, to find out nested references
      ;; nested refs = triangular substition
      (walk (cdr (elt s (my-pos u s))) s)
      u))

;; adds an lvar and the value v to the substition map.
;; (EXT-S #(0) 7 NIL) => ((#(0) . 7)))
;; in cl we dont print nil, actual list:
;; (EXT-S #(0) 7 NIL) => ((#(0) . 7)) . ())
(defun ext-s (lvar v s)
  `((,lvar . ,v) . ,s))

(defparameter mzero '())
(defun unit (s/c) (cons s/c mzero))

(defun unify (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ;; when u and are lvars, we check it they are the same var
      ((and (lvar? u) (lvar? v) (lvar=? u v)) s)
      ;; here we know that u v and not the _same_ lvars
      ;; so we want to add the values to the sub-map
      ((lvar? u) (ext-s u v s))
      ;; as we can see here, the sub map is supporting triangular subs. that means that an lvar can point to another lvar
      ;; which can point to another lvar and so on and so on
      ((lvar? v) (ext-s v u s))
       
      ;; if both are pairs === conses we split have to
      ;; recusively walk the pairs until we have a single val
      ;; that can be checked
      ;; this happens when ? 
      ((and (pair? u) (pair? v))
       ;; we start with checkin the first element u & v
       ;; if the s1 not false,
       ;; we can check the cdr and return the result
       ;; this is an ugly "and"
       ;; both s1 and 's2' have to be true
       (let ((s1 (unify (car u) (car v) s)))
         (if (not (eql s1 'false))
           (unify (cdr u) (cdr v) s1)  
           'false)))

      ;; if the terms are eql and the state is not false,
      ;; we want to return the state
      ;; this happens when for example we compare
      ;; u = 5 and v = 5
      ;; eqv? returns false for strings
      ((and (eqv? u v) (not (eql s 'false))) s)

      ;; when u & v can't be unified we return false
      (t 'false))))

(defun call/fresh (f)
  (lambda (s/c)
    ;; state is a list where the CDR is the count of the number of used lvars 
    ;; the default empty-state starts at 0.
    ;; '(()                                .         0)  <= default empty-state param
    ;;   |
    ;; empty-starting state                       no current lvars, doesn't need lvars to set this number though
    ;; no lvars bound to anything                 this is arbitrary, could be any positive-integer
    ;; we can pass in state:
    ;; (((#(0) . 7)) . 0)
    ;; this means the lvar "0" is bound to 7
    (let ((c (cdr s/c))) 
      (funcall (funcall f (lvar c)) `(,(car s/c) . ,(+ c 1))))))

(defun == (u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if (not (eql s 'false))
          (unit `(,s . ,(cdr s/c)))
          mzero))))

;; page 5
(defun mplus ($1 $2)
  (cond
    ((null? $1) $2)
    ;; trampoline binary
    ((functionp $1) (lambda () (mplus $2 (funcall $1))))
    (t (cons (car $1) (mplus (cdr $1) $2)))))

(defun bind ($ g)
  (cond
    ((null? $) mzero)
    ((functionp $) (lambda () (bind (funcall $) g)))
    (t (mplus (funcall g (car $)) (bind (cdr $) g)))))

;; how to visualise a search?
(defun disj (g1 g2)
  (lambda (s/c)
    (mplus (funcall g1 s/c) (funcall g2 s/c))))

;; how to visualise a search?
(defun conj (g1 g2)
  (lambda (s/c)
    (bind (funcall g1 s/c) g2)))

;;; "cl-microkanren" stops here
