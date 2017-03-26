(in-package #:cl-microkanren)

;; macros for nicer use, and more minikamren style


(macroexpand-1 '(fresh (a q) (== q 5) (== a q)))

(defmacro my-run (syms &body forms)
  `(funcall ,(fresh-helper syms forms) empty-state)) 

(macroexpand-1 '(my-run (a b) (== a b) (== a 5)))

(my-run (a b)
  (== a b)
  (== a '((1 2 x 4 5))))

(funcall
 (fresh (a q)
   (== q 5)
   (== a q))
 empty-state)


(funcall
 (fresh (a b)
   (== a b)
   (== b a))
 empty-state)

(macroexpand '(conj+ (== q 5)))

(macroexpand '(conj+ (== q 5) (== q 1)))


(defmacro Zzz (g)
  `(lambda (s/c)
       (funcall ,g s/c)))
  



(funcall
 (fresh (a b)
    (== a 42)
    (== b a))
 empty-state)  


(funcall
 (fresh (a b c d)
   (conj
     (== a 42)
     (== b a))
   (conj
     (== a c)
     (== c d)))
 empty-state)  


(match '((== 1 2) (== 1 5))
  ((cons a))
  ((cons a b) (+ a b)))

(macroexpand-1 '(conj+ (== 1 2) (== 3 3) (== 4 4) (== 5 5) (== 6 6)))

(funcall
 (conj+ (== 1 2) (== 3 3) (== 4 4) (== 5 5) (== 6 6))
 empty-state)
