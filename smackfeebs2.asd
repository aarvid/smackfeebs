(defpackage :smackfeebs-system
  (:use :cl :asdf))

(in-package :smackfeebs-system)

(defsystem smackfeebs2
  :description "Planet of the Feebs - SmackLisp Version."
  :version "0.1"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria #:smacklisp #:feebs)        
  :components
   ((:cl-source-file "spackage")
    (:cl-source-file "smeebs"  :depends-on ("spackage"))
    ))

