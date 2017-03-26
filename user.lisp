(in-package #:cl-microkanren)

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


























