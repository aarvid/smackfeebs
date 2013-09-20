(in-package :smackfeebs)

;;; Vision calculation.

;;; Some macros for directional arithmetic.

(defmacro left-of (facing)
  `(mod (+ ,facing 3) 4))

(defmacro right-of (facing)
  `(mod (+ ,facing 1) 4))

(defmacro behind (facing)
  `(mod (+ ,facing 2) 4))

;;; These guys tell us offsets given an orientation.

;; (0 1 0 -1)
(defmacro forward-dx (facing)
  `(- (rem (- ,facing 2) 2)))

;; (-1 0 1 0)
(defmacro forward-dy (facing)
  `(rem (- (1+ ,facing) 2) 2))

(defmacro left-dx (facing)
  `(forward-dx (left-of ,facing)))

(defmacro left-dy (facing)
  `(forward-dy (left-of ,facing)))

(defmacro right-dx (facing)
  `(forward-dx (right-of ,facing)))

(defmacro right-dy (facing)
  `(forward-dy (right-of ,facing)))

(defmacro behind-dx (facing)
  `(forward-dx (behind ,facing)))

(defmacro behind-dy (facing)
  `(forward-dy (behind ,facing)))


;;; These image structures are used to represent feebs and fireballs in
;;; the sensory displays of other feebs.

(defstruct (feeb-image
	    (:print-function print-feeb-image)
	    (:constructor make-feeb-image (name heading)))
  name
  heading)

(defun print-feeb-image (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Feeb-Image of ~S facing ~S>"
	  (feeb-image-name structure)
	  (print-direction (feeb-image-heading structure))))


(defstruct (fireball-image
	    (:print-function print-fireball-image)
	    (:constructor make-fireball-image (shooter direction)))
  shooter
  direction)

(defun print-fireball-image (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Fireball moving ~S>"
	  (print-direction (fireball-image-direction structure))))

(defgeneric square-vision (feeb x y))

(defmethod square-vision ((feeb feeb) x y)
  (let* ((maze (planet-maze (planet feeb))))
    (mapcar
     #'(lambda (x)
         (cond ((feebp x)
                (make-feeb-image (feeb-name x)
                                 (thing-heading x)))
               ((fireballp x)
                (make-fireball-image (feeb-name (fireball-shooter x))
                                     (thing-heading x)))
               ((mushroomp x) :mushroom)
               ((carcassp x) :carcass)
               ((eq x :rock) :rock)
               (t :unknown)))
     (aref maze x y))))


(defgeneric side-vision (feeb x y heading))

(defmethod side-vision ((feeb feeb) x y heading)
  (let* ((maze (planet-maze (planet feeb)))
         (heading (thing-heading feeb)))
    (dolist (x (aref maze x y) nil)
      (cond ((and (typep x 'feeb)
                  (feeb-peeking x)
                  (= heading (thing-heading x)))
             (return :peeking))
            ((eq x :rock) (return :rock))))))

(defgeneric compute-feeb-vision (feeb))

(defmethod compute-feeb-vision ((feeb feeb))
  (let* ((maze (planet-maze (planet feeb)))
         (pnt  (thing-coordinates feeb))
         (heading (thing-heading feeb))
         (x (first pnt))
         (y (second pnt))
         (vision-dx 0)
         (vision-dy 0)
         (v-ahead nil)
         (v-left nil)
         (v-right nil))
    (setf (current-square feeb)
	  (square-vision feeb x y))
    (setf (right-square feeb)
	  (square-vision feeb
                         (+ x (right-dx heading))
                         (+ y (right-dy heading))))
    (setf (left-square feeb)
	  (square-vision feeb
                         (+ x (left-dx heading))
                         (+ y (left-dy heading))))
    (setf (rear-square feeb)
	  (square-vision feeb
                         (+ x (behind-dx heading))
                         (+ y (behind-dy heading))))
    ;; The vision vector starts in the square the feeb is facing.
    (setq x (+ x (forward-dx heading)))
    (setq y (+ y (forward-dy heading)))
    ;; Figure out which direction to scan in.
    (case (feeb-peeking feeb)
      (nil)
      (:left (setq heading (left-of heading)))
      (:right (setq heading (right-of heading))))
    (setq vision-dx (forward-dx heading))
    (setq vision-dy (forward-dy heading))
    (unless (and (= vision-dx 0)
                 (= vision-dy 0))
      (do* ((x x (+ x vision-dx))
            (y y (+ y vision-dy))
            (left-wall-x (+ x (left-dx heading)) (+ left-wall-x vision-dx))
            (left-wall-y (+ y (left-dy heading)) (+ left-wall-y vision-dy))
            (right-wall-x (+ x (right-dx heading)) (+ right-wall-x vision-dx))
            (right-wall-y (+ y (right-dy heading)) (+ right-wall-y vision-dy))
            (index 0 (1+ index)))
           ((member :rock (aref maze x y))
            (setf (line-of-sight feeb) index))
        (push (square-vision feeb x y) v-ahead)
        (push (side-vision feeb left-wall-x left-wall-y (right-of heading))
              v-left)
        (push (side-vision feeb right-wall-x right-wall-y (left-of heading))
              v-right)))
    (setf (vision-ahead feeb)
          (apply #'vector (reverse v-ahead)))
    (setf (vision-left feeb)
          (apply #'vector (reverse v-left)))
    (setf (vision-right feeb)
          (apply #'vector (reverse v-right)))))

(defgeneric compute-vision (planet))
(defmethod compute-vision ((planet planet))
  (dolist (f (playing-feebs planet) t)
    (unless (deadp f)
     (compute-feeb-vision f))))

(defgeneric get-forward-square (thing))

(defmethod get-forward-square ((thing mobile-thing))
  (let* ((dir (thing-heading  thing))
         (x (+ (forward-dx dir) (thing-x-pos thing)))
         (y (+ (forward-dy dir) (thing-y-pos thing))))
    (aref (planet-maze (planet thing)) x y)))

(defgeneric get-current-square (thing))

(defmethod get-current-square ((thing thing))
  (aref (planet-maze (planet thing))
        (thing-x-pos thing)
        (thing-y-pos thing)))

(defgeneric move-forward (thing))
(defmethod move-forward ((thing mobile-thing))
  (let* ((dir (thing-heading  thing))
         (pnt (copy-list (thing-coordinates thing))))
    (remove-thing thing)
    (incf (first pnt) (forward-dx dir))
    (incf (second pnt) (forward-dy dir))
    (place-thing thing pnt)))

(defgeneric turn-around (thing))
(defmethod turn-around ((thing mobile-thing))
  (setf (thing-heading thing)
        (behind (thing-heading thing))))

(defgeneric turn-right (thing))
(defmethod turn-right ((thing mobile-thing))
  (setf (thing-heading thing)
        (right-of (thing-heading thing))))

(defgeneric turn-left (thing))
(defmethod turn-left ((thing mobile-thing))
  (setf (thing-heading thing)
        (left-of (thing-heading thing))))
