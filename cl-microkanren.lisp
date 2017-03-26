;;;; cl-microkanren.lisp

(in-package #:cl-microkanren)

;; https://github.com/jasonhemann/microKanren/blob/master/miniKanren-wrappers.scm
;; https://mullr.github.io/micrologic/literate.html

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

;;; "cl-microkanren" goes stops here

;;; below are helper macros and a playground area

(defparameter empty-state '(() . 0))

(defun pull (st)
  (if (functionp st)
      (pull (funcall st))
      st))

(defun take (n st)
  (if (= 0 n) '()
    (let ((st (pull st)))
      (cond
        ((null? st) '())
        (t (cons (car st) (take (- n 1) (cdr st))))))))

(defun take-all (st)
  (let ((next (pull st)))
    (if (null? next) 
        '()
        (cons (car next) (take-all (cdr next))))))

(defun call/empty-state (g)
  (funcall g empty-state))

(defun reify-s (v s)
  (let ((v (walk v s)))
    (cond
     ((lvar? v)
      (let ((n (reify-name (length s))))
        (cons `(,v . ,n) s)))
     ((pair? v)
      (reify-s (cdr v) (reify-s (car v) s)))
     (t s))))

(defun reify-name (n)
  (make-symbol (concatenate 'string "_." (write-to-string n))))

(defun reify-state/1st-var (s/c)
  (let ((v (walk* (lvar 0) (car s/c))))
     (walk* v (reify-s v '()))))

(defun mK-reify (s/c)
  (mapcar (lambda (s)
            (reify-state/1st-var s))
          s/c))

(defun walk* (v s)
  (let ((v (walk v s)))
    (cond
      ((lvar? v) v)
      ((pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s)))
      (t v))))

(defmacro zzz (g) 
  `(lambda(s/c) (lambda() (funcall ,g s/c))))

(defmacro conj+ (&rest goals)
  (match goals
      ((guard x (null (cdr x))) `(zzz ,@x))
      ((cons (place g) rest) `(conj (zzz ,g) (conj+ ,@rest)))))

(defmacro disj+ (&rest goals)
  (match goals
      ((guard x (null (cdr x))) `(zzz ,@x))
      ((cons (place g) rest) `(disj (zzz ,g) (disj+ ,@rest)))))

(defmacro fresh (&rest e)
  (cond
    ((null? (car e)) `(conj+ ,@(cdr e)))
    (t `(call/fresh (lambda (,(car (car e)))
                      (fresh ,(cdr (car e)) ,@(cdr e)))))))

(defmacro conde (&rest goals)
  `(disj+ (conj+ ,@(first goals))
          (conj+ ,@(second goals))))

(defun appendo (l s out)
 (conde
   ((== '() l) (== s out))
   ((fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res)))))

(defmacro run (num &rest goals)
  `(mK-reify
    (take ,num
          (call/empty-state
           (fresh ,@goals))))) 

(defmacro run* (&rest goals)
  `(mK-reify
    (take-all
     (call/empty-state
      (fresh ,@goals)))))


;;;;;;;;;;; playground

(run 2 (q)
      (== 1 q)
      (== 1 1))

(run 10 (a b)
      (== a b))

(run 5 (a b)
     (conde ((== a b) (== b 1))
            ((== a b))))

(run* (q) (== '(a c o r n) q))

(run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))

(run* (q x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5)))


(macroexpand-1 '(conde ((== a b) (== b c)) ((== b c) (== d e))))

(take-all
 (call/empty-state
  (call/fresh
   (lambda (a)
     (disj (== a 7) (== a 1))))))
