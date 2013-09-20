;;; -*- Common Lisp -*-

(in-package :smackfeebs)

;;; Directions

(deftype direction ()
  `(integer 0 3))

(defconstant north 0)
(defconstant east  1)
(defconstant south 2)
(defconstant west  3)


(defclass universe-parameter ()
  (
   (name :accessor parameter-name :initarg :name)
   (value :accessor parameter-value :initarg :value)
   (type :accessor parameter-type :initarg :type)
   (documentation :accessor parameter-documentation :initarg :doc)))
    
(defparameter *parameters* (make-hash-table :test 'eq))


(defun def-universe-parm (name value type &optional doc)
  (unless (typep value type)
    (error "value ~a not expected of expected type ~a" value type))
  (if-let ((parm (gethash name *parameters*)))
    (progn
      (warn "Change parameter ~a to ~a: ~%parameter already existed with value ~a." name value (parameter-value parm))
      (setf (parameter-value parm) value)
      (setf (parameter-type parm) type)
      (setf (parameter-documentation parm) doc))
    (setf (gethash name *parameters*)
          (make-instance 'universe-parameter
                         :name name
                         :value value
                         :type type
                         :doc doc)))
  name)

(defun get-universe-parm (name)
  (if-let ((parm (gethash name *parameters*)))
    (parameter-value parm)
    (error "parameter ~a unknown" name)))

(defun change-universe-parm ( name value)
  (if-let ((parm (gethash name *parameters*)))
    (let ((type (parameter-type parm)))
      (unless (typep value type)
        (error "value ~a not expected of expected type ~a" value type))  
      (setf (parameter-value (gethash name *parameters*)) value))
    (error "parameter ~a unknown" name)))
       

(defun uni-param-type (name)
  (if-let ((parm (gethash name *parameters*)))
    (parameter-type parm)
    (error "parameter ~a unknown" name)))

  
(defmethod documentation (name (type (eql 'feeb-parameter)))
  (if-let ((parm (gethash name *parameters*)))
    (parameter-documentation parm)
    (error "parameter ~a unknown" name)))

(defun list-universe-parameter-settings ()
  (let (params)
    (maphash #'(lambda (key value)
                 (push (list key (parameter-value value) (parameter-type value) (parameter-documentation value)) params))
             *parameters*)
    params))


;; general game parameters

(def-universe-parm :feeb-capacity 10
  'unsigned-byte
  "Maximum number of feebs in the maze")

(def-universe-parm :game-length 100
  'unsigned-byte
  "Number of cycles in the simulation")

(def-universe-parm  :slow-feeb-noop-switch t
  'boolean
  "If non-null, each feeb has a chance of having its orders aborted in
proportion to the time it takes to produce them.")

(def-universe-parm  :slow-feeb-noop-factor .25
  'real
  "If :slow-feeb-noop-switch is non-null, a feeb's orders will be aborted
with probability equal to the product of this factor times the time taken
by this feeb divided by the total time taken by all feebs this turn.")

(def-universe-parm  :feeb-timeout-seconds 5
  t
  "If non-null, the number of seconds each feeb's brain to execute. Used to
abort infinite loops or excessively long calculations.  Not to be confused
with :slow-feeb-noop-switch/factor.")

;; scoring
(def-universe-parm :points-for-being-killed -2
  'integer
  "Added to one's score for being killed.")

(def-universe-parm :points-for-starving -1
  'integer
  "Added to one's score for being starving to death.")

(def-universe-parm :points-for-killing 2
  'integer
  "Added to one's score for killing an opponent.")

(def-universe-parm :points-for-eating 0
  'integer
  "Added to one's score for eating a mushroom.")

;; characteristics of the maze:
(def-universe-parm :mushrooms-per-cycle 3
  'unsigned-byte
  "Maximum number of mushrooms created each cycle.")

(def-universe-parm :mushroom-capacity 10
  'unsigned-byte
  "Maximum number of mushrooms in the maze (also limited by mushroom sites).")

(def-universe-parm :mushroom-at-start nil
  'boolean
  "The maze starts simulation with mushrooms ")

(def-universe-parm :complement-with-system-feebs t
  'boolean
  "Game is filled to capacity with system feebs")


;; energies

(def-universe-parm :starting-energy-mean 50
  'unsigned-byte
  "The average amount of energy a feeb will start with.")

(def-universe-parm :starting-energy-standard-deviation 10
  'unsigned-byte
  "Deviation from the mean for the amount of energy a feeb will start with.")

(def-universe-parm :maximum-energy 100
  'unsigned-byte
  "The most energy a feeb can accumulate.")

(def-universe-parm :mushroom-energy 50
  'integer
  "Energy gained by feeding on a mushroom.")

(def-universe-parm :carcass-energy 30
  'integer
  "Energy gained by feeding on a carcass.")

(def-universe-parm :flame-energy 10
  'integer
  "Energy lost when throwing a flame.")

;; carcasses



(def-universe-parm :carcass-lifetime-minimum 3
  'integer
  "Minimum number of turns a carcass will hang around.")

(def-universe-parm :carcass-lifetime-mean 5
  'integer
  "The average number of turns a carcass will hang around.")

(def-universe-parm :carcass-lifetime-standard-deviation 1
  'real
  "Deviation from the mean for number of turns a carcass will hang around.")

;; fireballs

(def-universe-parm  :fireball-lifetime-mean 5
  'unsigned-byte
  "The average lifetime of a fireball, that is how many turns until it dissipates. ")

(def-universe-parm  :fireball-lifetime-standard-deviation 2
  'real
  "Deviation from the mean for the lifetime of a firebal.")



(def-universe-parm  :fireball-reflection-probability 1/2
  'rational
  "Chance that a fireball will reflect when it hits a wall.")



(def-universe-parm  :flame-recovery-mean 3
  'unsigned-byte
  "The average number of turns a feeb need to regain its ability to flame after flaming once.")

(def-universe-parm  :flame-recovery-standard-deviation 1
  'real
  "Deviation from the mean Chance a feeb will regain its ability to flame each turn after flaming once.")
