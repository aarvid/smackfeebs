
;;; based on Planet of the Feebs (no license)

;;; About Planet of the Feebs:
;; Designed by Scott Fahlman
;; Written by Skef Wholey.
;; Modified by Scott Fahlman.
;; Modified by Dan Kuokka.
;; Modified by Jim Healy.
;;
;; also using ideas from The Feebs War  Gustavo Henrique Milaré GPLv3

(in-package :cl-user)

(defpackage  :smackfeebs
  (:nicknames :smeebs)
  (:use :common-lisp
        :smacklisp
        :alexandria)
  (:shadow
   :load-file)
  (:export
   #:load-file
  ))


