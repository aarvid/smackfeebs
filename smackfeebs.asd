(defpackage :smack-feebs-system
  (:use :cl :asdf))

(in-package :smack-feebs-system)

(defsystem smackfeebs
  :description "Planet of the Feebs - SmackLisp Version."
  :version "0.1"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "none - public domain"
  :depends-on (#:alexandria #:smacklisp)        
  :components
   ((:cl-source-file "package")
    (:cl-source-file "maze-layouts"  :depends-on ("package"))
    (:cl-source-file "parameters"  :depends-on ("package"))
    (:cl-source-file "planet"  :depends-on ("package" "maze-layouts" "parameters"))
    (:cl-source-file "things"  :depends-on ("package" "planet"))
    (:cl-source-file "vision"  :depends-on ("package" "planet" "things"))
    (:cl-source-file "print"  :depends-on ("package" "planet"))
    (:cl-source-file "history" :depends-on ("package" "planet" "things"))
    (:cl-source-file "smackenv"
     :depends-on ("package" "planet" "things" "vision" "print"))
    (:cl-source-file "play"
     :depends-on ("package" "planet" "things" "vision" "smackenv" "history"))))


