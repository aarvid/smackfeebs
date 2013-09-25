
;;; based on Planet of the Feebs (no license)

;;; About Planet of the Feebs:
;; Designed by Scott Fahlman
;; Written by Skef Wholey.
;; Modified by Scott Fahlman.
;; Modified by Dan Kuokka.
;; Modified by Jim Healy.
;;
;; also using ideas from The Feebs War  Gustavo Henrique Milaré GPLv3


(defpackage  :smackfeebs
  (:nicknames :smeebs)
  (:use :common-lisp
        :smacklisp
        :alexandria)
  (:export
   ;; parameters.lisp
   #:list-universe-parameter-settings
   ;; planet.lisp
   #:planet
   #:cycle
   #:game-number
   #:game-zero-timestamp
   #:maze-layout
   #:playing-feebs
   #:fireballs
   #:carcasses
   #:get-feeb-parm
   #:planet-archives
   #:archives
   #:planet-name
   #:defined-feebs
   ;; things.lisp
   #:feeb
   #:make-feeb
   #:feeb-lisp-env
   #:thing-coordinates
   #:thing-heading
   #:thing-id
   #:feeb-name
   #:deadp
   #:planet-mushrooms
   #:feeb-energy
   #:feeb-score
   #:feeb-kills
   #:feeb-deaths
   ;;vision.lisp
   #:forward-dx
   #:forward-dy
   ;; smackenv.lisp
   #:*active-feeb*
   #:*planets*
   #:make-planet-name
   #:unique-feeb-name-p
   #:make-feeb-name
   #:feeb-leave-planet
   #:create-planet-with-feeb
   ;; play.lisp
   #:initialize-simulation
   #:play
   ;; history.lisp
   #:playing-feebs->alist
   ))


