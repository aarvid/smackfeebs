(in-package :smackfeebs)

(defclass planet ()
    (
     (parameters :reader parameters :initform (make-hash-table :test 'eq))
     (name :accessor planet-name :initform nil :initarg :name)

     (maze-layout :accessor maze-layout :initform nil)
     (maze :accessor planet-maze :initform nil)

     (play-is-active :accessor play-is-active :initform nil)
     (defined-feebs :accessor defined-feebs :initform nil)
     (playing-feebs :accessor playing-feebs :initform nil)
     (feebs :accessor feebs :initform nil)
     (dead-feebs :accessor dead-feebs :initform nil)
     (fireballs :accessor fireballs :initform nil)
     (mushroom-sites :accessor mushroom-sites :initform nil)
     (carcasses :accessor carcasses :initform nil)
     (entry-points :accessor entry-points :initform nil)
     (number-of-mushroom-sites :accessor number-of-mushroom-sites :initform 0)
     (number-of-entry-points :accessor number-of-entry-points :initform 0)
     (game-zero-timestamp :accessor game-zero-timestamp :initform nil)
     (game-number :accessor game-number :initform 0)     
     (cycle :accessor cycle :initform 0)
     (game-active :accessor game-active :initform nil)
     (total-time :accessor total-time :initform 0)
     (random-state :initform (make-random-state t)
                   :initarg :random-state
                   :accessor planet-random-state)
     (private :accessor planet-private :initarg :private)
     (archives :accessor planet-archives
               :initform nil)))


(defmethod initialize-instance :after ((planet planet) &key)
  (planet-initialize planet)
  (load-text-maze planet *maze-default*))


;; planet parameters
(defgeneric get-feeb-parm (planet  name ))
(defmethod get-feeb-parm ((planet planet) (name symbol))
  (if-let ((parm (gethash name (parameters planet))))
    (car parm)
    (error "parameter ~a unknown" name)))

(defgeneric def-feeb-parm (planet universe-parameter))
(defmethod def-feeb-parm ((planet planet) universe-parameter)
  (let ((hparm (parameters planet))
        (name (parameter-name universe-parameter)))
    (setf (gethash name hparm)
          (cons (parameter-value universe-parameter) universe-parameter))
    name))

(defgeneric change-feeb-parm (planet name value))
(defmethod change-feeb-parm ((planet planet) name value)
  (unless (game-active planet)
    (if-let ((parm (gethash name (parameters planet))))
       (let ((type (parameter-type (cdr parm))))
         (unless (typep value type)
           (error "value ~a not expected of expected type ~a" value type))  
         (setf (car (gethash name (parameters planet))) value))
       (error "parameter ~a unknown" name))))

(defgeneric list-parameter-settings (planet))
(defmethod list-parameter-settings ((planet planet))
  (let (params)
    (maphash #'(lambda (key value)
                 (push (list key (car value) (parameter-type (cdr value))
                             (parameter-documentation (cdr value)))
                       params))
             (parameters planet))
    params))



(defgeneric planet-initialize (planet))
(defmethod planet-initialize ((planet planet))
  (maphash #'(lambda (key value)
               (declare (ignore key))
               (def-feeb-parm planet value))
           *parameters*)
  (setf (planet-archives planet)
        (make-array (1+ (get-feeb-parm planet :game-length))
                    :adjustable t :fill-pointer 0)))

(defgeneric planet-capacity (planet))
(defmethod planet-capacity ((planet planet))
  (min (get-feeb-parm planet :feeb-capacity)
       (number-of-entry-points planet)))



(defun calc-text-maze-size (maze)
  (let ((h 0)
        (w (length (car maze))))
    (dolist (s maze)
      (incf h)
      (if (/= (length s) w)
          (error "Not all the strings in ~a have the same size." maze))    )
    (values h w)))

(defgeneric load-text-maze (planet maze))
(defmethod load-text-maze ((planet planet) maze)
  (multiple-value-bind (height width) (calc-text-maze-size maze)
    (setf (planet-maze planet) (make-array (list width height))
          (maze-layout planet) (make-array (list width height))
          (entry-points planet) nil
          (mushroom-sites planet) nil)
    (do ((rows maze (cdr rows))
         (y 0 (1+ y))) 
        ((null rows))
      (let ((str (car rows)))
        (dotimes (x (length str))
          (let ((spot (case (schar str x)
                        (#\X :rock)
                        (#\* :mushroom-place)
                        (#\e :feeb-entry-place)
                        (#\space nil)
                        (t (error "Bad thing in maze spec: ~C. at x: ~a  y: ~a"
                                  (schar str x) x y)))))
            (setf (aref (planet-maze planet) x y) (case spot
                                                    (:rock (list spot))
                                                    (t nil))
                  (aref  (maze-layout planet) x y) spot)
            (case spot
              (:mushroom-place
               (push (list x y) (mushroom-sites planet)))
              (:feeb-entry-place
               (push (list x y) (entry-points planet))))))))
    (setf  (number-of-mushroom-sites planet) (length (mushroom-sites planet))
           (number-of-entry-points planet) (length (entry-points planet)))
    t))




(defgeneric planet-random (planet n))
(defmethod planet-random ((planet planet) n)
     (random n (planet-random-state planet)))

(defgeneric planet-chance (planet ratio))
(defmethod planet-chance ((planet planet) (ratio rational))
  (< (planet-random planet (denominator ratio))
     (numerator ratio)))


;; stolen from alexandria and modified because alexandria has a bug.
(defun my-gaussian-random (&optional min max)
  "Returns two gaussian random double floats as the primary and secondary value,
optionally constrained by MIN and MAX. Gaussian random numbers form a standard
normal distribution around 0.0d0."
  (macrolet ((valid (x) `(<= (or min ,x) ,x (or max ,x)) ))
    (labels ((gauss ()
               (loop
                 for x1 = (- (random 2.0d0) 1.0d0)
                 for x2 = (- (random 2.0d0) 1.0d0)
                 for w = (+ (expt x1 2) (expt x2 2))
                 when (< w 1.0d0)
                   do (let ((v (sqrt (/ (* -2.0d0 (log w)) w))))
                        (return (values (* x1 v) (* x2 v))))))
             (guard (x)
               (unless (valid x) 
                 (tagbody
                  :retry
                    (multiple-value-bind (x1 x2) (gauss)
                      (when (valid x1)
                        (setf x x1)
                        (go :done))
                      (when (valid x2)
                        (setf x x2)
                        (go :done))
                      (go :retry))
                  :done))
               x))
      (multiple-value-bind (g1 g2) (gauss)
        (values (guard g1) (guard g2))))))

(let (_saved_gauss_)
  (defun simple-gaussian-random (&optional min max)
    (labels ((gauss ()
               (loop
                 for x1 = (- (random 2.0d0) 1.0d0)
                 for x2 = (- (random 2.0d0) 1.0d0)
                 for w = (+ (expt x1 2) (expt x2 2))
                 when (< w 1.0d0)
                   do (let ((v (sqrt (/ (* -2.0d0 (log w)) w))))
                        (return (list (* x1 v) (* x2 v)))))))
      (loop
        (unless _saved_gauss_
          (setf _saved_gauss_ (gauss)))
        (let ((x (pop _saved_gauss_)))
          (when (<= (or min x) x (or max x))
            (return x)))))))

(defgeneric planet-gaussian-random (planet mu sigma &optional min max))

(defmethod planet-gaussian-random ((planet planet) mu sigma &optional min max)
  (macrolet ((deparm (p) `(when (and ,p (symbolp ,p))
                            (setf ,p (get-feeb-parm planet ,p)))))
    (deparm mu)
    (deparm sigma)
    (deparm min)
    (deparm max))
  (let ((*random-state* (planet-random-state planet))
          (min0 (if min (/ (- min mu) sigma)))
          (max0 (if max (/ (- max mu) sigma))))
      (+ mu (* sigma (simple-gaussian-random min0 max0)))))
#|(multiple-value-bind (g1 g2) (my-gaussian-random min0 max0)
        (values (+ mu (* sigma g1))
                (+ mu (* sigma g2))))|#
