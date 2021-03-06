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
  (funcall g '(() . 0)))

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
  (mapcar (lambda (s) (reify-state/1st-var s)) s/c))

(defun walk* (v s)
  (let ((v (walk v s)))
    (cond
      ((lvar? v) v)
      ((pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s)))
      (t v))))

(defmacro zzz (g) 
  `(lambda (s/c) (lambda() (funcall ,g s/c))))

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

(defun conso (first rest out)
  (if (lvar? rest)
    (== `(,first . ,rest) out)
    (== (cons first rest) out)))

(run* (q) (conso 1 '(2 3) q))

(run* (q) (conso 1 q '(1 2 3)))

(run* (q) (conso 1 `(2 ,q) '(1 2 3)))

(run* (q) (fresh (a b) (conso a `(2 ,b) '(1 2 3)) (== q (list a b))))
;; sets a & b to match the list and then sets the result to q

(run* (q x y)
   (== `(,x ,y) q) (appendo x y '(1 2 3 4 5)))

;;;;;;;;;;; streams playground

(call/empty-state
 (conj (call/fresh (lambda (a) (== a 7)))
       (call/fresh (lambda (b) (disj (== b 5)
                                     (== b 6))))))
(defun fives (x)
  "example of inverse-n-delay"
  (disj (== x 5) (lambda (s/c) (lambda () (funcall (fives x) s/c)))))

(defun sixes (x)
  "example of inverse-n-delay"
  (disj (== x 6) (lambda (s/c) (lambda () (funcall (sixes x) s/c)))))
           
(defun fives-and-sixes ()
  (call/fresh (lambda (x) (disj (fives x) (sixes x)))))



;; this call returns an list with a answer and the cdr of the state-map is a closure
;; this is how infinitive streams are represented
;; probably "immature"?
(funcall (fives-and-sixes) empty-state)

(take 1 (funcall (fives-and-sixes) empty-state))




;;;;;;;;;;; playground

(macroexpand-1 '(conde ((== a b) (== b c)) ((== b c) (== d e))))

(take-all
 (call/empty-state
  (call/fresh
   (lambda (a)
     (disj (== a 7) (== a 1))))))


(take-all
 (call/empty-state
  (call/fresh
   (lambda (a)
     (conj (== a 7)
           (== a 1))))))



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

(run* (q)
  (== q (list 1 2 3)))

(run* (q)
  (fresh (a b c)
         (disj+
          (== q 1)
          (== q 2)
          (== q 3))))

(run* (q)
      (fresh (x y)
             (appendo x y '(1 2))
             (== q (list x y))))                      























