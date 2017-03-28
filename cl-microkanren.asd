;;;; cl-microkanren.asd

(asdf:defsystem #:cl-microkanren
  :description "Describe cl-microkanren here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:blasut-utils #:prove #:optima)
  :components ((:file "package")
               (:file "cl-microkanren")))

