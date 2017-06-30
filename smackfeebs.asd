

(defsystem "smackfeebs"
  :description "Planet of the Feebs - SmackLisp Version."
  :version "0.1"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on ("alexandria" "smacklisp" "feebs")        
  :components
   ((:cl-source-file "package")
    (:cl-source-file "smeebs"  :depends-on ("package"))
    ))

