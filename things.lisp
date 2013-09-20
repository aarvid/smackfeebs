(in-package :smackfeebs)

(let ((counter 0))
  (defun generate-thing-id ()
    (incf counter)))

(defclass thing ()
  ((coordinates :accessor thing-coordinates :initarg :coordinates :initform nil)
   (id :reader thing-id  :initform (generate-thing-id))
   (planet :accessor planet :initarg :planet :initform nil)))


(defun thingp (o) (typep o 'thing))

(defgeneric thing-x-pos (thing))
(defgeneric thing-y-pos (thing))
(defmethod  thing-x-pos ((thing thing))
  (first (thing-coordinates thing)))
(defmethod  thing-y-pos ((thing thing))
  (second (thing-coordinates thing)))
(defgeneric print-maze-char (thing)
  )

(defmethod print-maze-char ((thing t))
  #\Space)
(defmethod print-maze-char ((thing thing))
  #\?)

(defmethod print-maze-char ((s symbol))
  (if (eq s :carcass) 
      (code-char #x2620)
      #\Space))

(defmethod print-object ((object thing) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (coordinates) object
      (format stream "[~d,~d]" (first coordinates) (second coordinates)))))

(defgeneric place-thing (thing location))
(defmethod place-thing ((thing thing) location)
  (setf (thing-coordinates thing) location)
  (push thing (aref (planet-maze (planet thing))
                    (thing-x-pos thing)
                    (thing-y-pos thing))))

(defgeneric remove-thing (thing))

(defmethod remove-thing ((thing thing))
  (when (thing-coordinates thing)
    (removef (aref (planet-maze (planet thing))
                   (thing-x-pos thing)
                   (thing-y-pos thing))
             thing)
    (setf (thing-coordinates thing) nil)))


(defclass mobile-thing (thing)
  ((heading :accessor thing-heading :initarg :heading :initform north)))

(defmethod print-object ((object mobile-thing) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (coordinates heading) object
      (format stream "[~d,~d,~d]"
              (first coordinates)
              (second coordinates)
              (print-heading heading)))))




(defclass fireball (mobile-thing)
  ((shooter :accessor fireball-shooter :initarg :shooter :initform nil)
   (newp :accessor fireball-new-p  :initform t)
   (lifetime-cycles :accessor lifetime-cycles :initform nil :initarg :cycles))
  )

(defun fireballp (o) (typep o 'fireball))

(defmethod print-object ((object fireball) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (shooter coordinates heading) object
      (format stream "~d [~d,~d,~d]" shooter (first coordinates) (second coordinates) (print-heading heading)))))


#|(defmethod print-maze-char ((f fireball))
  (case (thing-heading f)
    (0 #\^)
    (1 #\>)
    (2 #\v)
    (3 #\<)))|#

(defmethod print-maze-char ((f fireball))
 (code-char
  (ecase (thing-heading f)
     (0 #x2191)    ;;#\^
     (1 #x2192)    ;;#\>
     (2 #x2193)    ;;#\v
     (3 #x2190)))) ;;#\<



(defclass food (thing)
  ((calories :accessor calories :initarg :calories :initform nil))
  )

(defclass mushroom (food)
  ((holder :accessor holder :initarg :owner :initform nil))
  )

(defun mushroomp (o) (typep o 'mushroom))

(defmethod print-object ((object mushroom) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (holder coordinates heading) object
      (format stream "~d [~d,~d]" holder (first coordinates) (second coordinates)))))

(defmethod print-maze-char ((x mushroom))
  (code-char #x2618)) ;; flower 2698  heart 2767 shamrock 2618

(defclass carcass (food)
  ((corpse-of :accessor corpse-of :initarg :corpse-of :initform nil)
   (lifetime-cycles :accessor lifetime-cycles :initform nil)))


(defmethod print-object ((object carcass) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (corpse-of coordinates heading) object
      (format stream "~d [~d,~d]" corpse-of (first coordinates) (second coordinates)))))

(defun carcassp (o) (typep o 'carcass))

(defmethod print-maze-char ((x carcass))
  (code-char #x2620))

(defclass feeb (mobile-thing)
  (
   (name       :accessor feeb-name     :initarg :name)
   (system-feeb :accessor feeb-system-p  :initarg :system-feeb :initform nil)
   (energy     :accessor feeb-energy   :initform 0)
   (score      :accessor feeb-score    :initform 0)  
   (kills      :accessor feeb-kills    :initform 0)
   (deaths     :accessor feeb-deaths   :initform 0)
   (lisp-env   :accessor feeb-lisp-env :initform (create-lisp-environment))

   (last-move  :accessor feeb-last-move :initform :noop)
   (last-error  :accessor feeb-last-error :initform :noop)
   (move-time :accessor feeb-move-time :initform 0)
   (move-aborted  :accessor feeb-move-aborted :initform nil)
   (peeking    :accessor feeb-peeking  :initform nil)
   (ready-to-fire :accessor ready-to-fire :initform t)
   (deadp :accessor deadp :initform nil)
   (restore-firepower-cycles :accessor restore-firepower-cycles :initform nil)
   (turns-dead :accessor turns-dead :initform 0)
   
   (current-square :accessor current-square  :initform nil)
   (right-square :accessor right-square  :initform nil)
   (left-square :accessor left-square  :initform nil)
   (rear-square :accessor rear-square  :initform nil)
   (vision-ahead :accessor vision-ahead  :initform nil)
   (vision-left :accessor vision-left  :initform nil)
   (vision-right :accessor vision-right  :initform nil)
   (line-of-sight :accessor line-of-sight :initform 0)
   )
  )

(defun feebp (o) (typep o 'feeb))

(defmethod print-object ((object feeb) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name coordinates heading) object
      (format stream "~d [~d,~d,~d]" name (first coordinates) (second coordinates) (print-heading heading)))))

(defmethod print-maze-char ((f feeb))
 (code-char
  (ecase (thing-heading f)
     (0 #x25b4)    ;;#\^
     (1 #x25b8)    ;;#\>
     (2 #x25be)    ;;#\v
     (3 #x25c2)))) ;;#\<

(defgeneric make-feeb (planet name))
(defmethod make-feeb ((planet planet) name)
  (let* ((feeb (make-instance 'feeb
                              :planet planet
                              :name name)))
    (push feeb (defined-feebs planet))
    (place-feeb-in-maze planet feeb)
    feeb))

(defmethod make-feeb ((planet t) name)
  (let* ((feeb (make-instance 'feeb
                              :planet nil
                              :name name
                              :lisp-env )))
    feeb))

(defgeneric place-feeb-in-maze (planet feeb))
(defmethod place-feeb-in-maze ((planet planet) (feeb feeb))
  (let* ((pnt (random-open-entry-point planet)))
    (setf (thing-coordinates feeb)  pnt
          (thing-heading feeb) (planet-random planet 4)
          (feeb-score feeb) 0
          (feeb-energy feeb)
          (round (planet-gaussian-random
                  planet
                  (get-feeb-parm planet :starting-energy-mean)
                  (get-feeb-parm planet :starting-energy-standard-deviation)
                  1
                  (get-feeb-parm planet :maximum-energy)))
          (feeb-last-move feeb) :noop)
    (if pnt
        (push feeb (aref (planet-maze planet) (first pnt) (second pnt)))
        (push feeb (dead-feebs planet)))
    (setf (deadp feeb) (not pnt))
    (push feeb (playing-feebs planet))
    feeb))

(defgeneric create-system-feeb (planet number))
(defmethod create-system-feeb ((planet planet) number)
  (let ((feeb (make-instance 'feeb
                             :planet planet
                             :name (format nil "system-~d" number)
                             :system-feeb t)))
    (place-feeb-in-maze planet feeb)
    (let ((smacklisp:*smack-symbols* (feeb-lisp-env feeb)))
      (smacklisp:load-file "smackfeebs/autobrain.smack"))))  ;; !!! fix this.


(defgeneric bump-system-feeb (planet))
(defmethod bump-system-feeb ((planet planet))
  (remove-feeb planet
               (random-elt (remove-if-not #'feeb-system-p (playing-feebs planet)))))
                             

(defgeneric remove-feeb (planet feeb))
(defmethod remove-feeb ((planet planet) (feeb feeb))
  (remove-thing feeb)
  (removef (playing-feebs planet) feeb)
  (unless (feeb-system-p feeb)
   (removef (defined-feebs planet) feeb))
  (setf (planet feeb) nil))

(defgeneric planet-mushrooms (planet))
(defmethod planet-mushrooms ((planet planet))
  (let ((shrooms nil))
    (dolist (s (mushroom-sites planet) shrooms)
      (appendf shrooms
               (remove-if-not #'mushroomp  (aref (planet-maze planet)
                                                 (first s)
                                                 (second s)))))))
