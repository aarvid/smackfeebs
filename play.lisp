(in-package :smackfeebs)

(defmacro with-play-active ((planet) &body body)
  (let ((save-active (gensym)))
    `(let ((,save-active (play-is-active ,planet)))
       (setf (play-is-active ,planet) t)
       (unwind-protect
          (progn ,@body)
         (setf (play-is-active ,planet) ,save-active)))))

(defgeneric initialize-simulation (planet &key layout))
(defmethod initialize-simulation ((planet planet) &key layout)
  (when layout
    (load-text-maze planet layout))
  (setf (cycle planet) 0)
  (clear-maze planet)
  (initialize-playing-feebs planet)
  (compute-vision planet)
  (setf (game-zero-timestamp planet) (get-internal-real-time))
  (incf (game-number planet))
  (setf (fill-pointer (planet-archives planet)) 0)
  (archive-planet-cycle planet))


(defgeneric clear-maze (planet))
(defmethod clear-maze ((planet planet))
  (setf (playing-feebs planet) nil)
  (setf (carcasses planet) nil)
  (setf (fireballs planet) nil)
  (let ((dim (array-dimensions (planet-maze planet))))
    (dotimes (x (car dim))
      (dotimes (y (cadr dim))
        (setf (aref (planet-maze planet) x y)
              (remove-if #'thingp
                         (aref (planet-maze planet) x y)))))))


(defgeneric initialize-playing-feebs (planet))
(defmethod initialize-playing-feebs ((planet planet))
  (let ((count 0)
        (capacity (planet-capacity planet)))
    (setf (playing-feebs planet) nil)
    (dolist (feeb (defined-feebs planet))
      (when (< count capacity)
        (incf count)
        (place-feeb-in-maze planet feeb)))
    (when (and (get-feeb-parm planet :complement-with-system-feebs)
               (< count capacity))
      (dotimes (i (- capacity count))
        (create-system-feeb planet i)))))

(defgeneric point-occupied-p (planet point))
(defmethod point-occupied-p ((planet planet) point)
  (let ((spot (aref (planet-maze planet ) (first point) (second point))))
    (member-if #'feebp spot)))

(defgeneric random-open-entry-point (planet))
(defmethod random-open-entry-point ((planet planet))
  (let ((open-points (remove-if (lambda (p) (point-occupied-p planet p))
                                (entry-points planet))))
    (when open-points
      (nth (planet-random planet (length open-points)) open-points))))


(defgeneric grow-mushrooms (planet &key allp))
(defmethod grow-mushrooms ((planet planet) &key allp)
  (flet ((site-mushroom-p (s) (member-if #'mushroomp
                                         (aref (planet-maze planet)
                                               (first s)
                                               (second s)))))
    (let* ((capacity (get-feeb-parm planet :mushroom-capacity))
           (calories (get-feeb-parm planet :mushroom-energy))
           (per-cycle (if allp
                          most-positive-fixnum
                          (get-feeb-parm planet :mushrooms-per-cycle)))
           (sites (mushroom-sites planet))
           (mushrooms (count-if #'site-mushroom-p sites))
           (empty-sites (shuffle (remove-if #'site-mushroom-p sites)))
           (num (min (if allp most-positive-fixnum  per-cycle)
                     (length empty-sites)
                     (- capacity mushrooms))))
      (loop for s in empty-sites
            repeat num
            do (place-thing (make-instance 'mushroom
                                           :planet planet
                                           :calories calories)
                            s))
      num)))



(defgeneric decay-carcasses (planet))
(defmethod decay-carcasses ((planet planet) )
  (let ((deleted nil)
        (life-min (get-feeb-parm planet :carcass-lifetime-minimum))
        (life-mean (get-feeb-parm planet :carcass-lifetime-mean))
        (life-sdev (get-feeb-parm planet :carcass-lifetime-standard-deviation)))
    (dolist (c (carcasses planet))
      (unless (lifetime-cycles c)
        (setf (lifetime-cycles c)
              (round (planet-gaussian-random planet life-mean life-sdev life-min))))
      (when (> 0 (decf (lifetime-cycles c)))
        (push c deleted)
        (remove-thing c)
        (setf (corpse-of c) nil)))
    (setf (carcasses planet) (nset-difference (carcasses planet) deleted))
    (length deleted)))

(defgeneric reincarnate-feebs (planet))

(defmethod reincarnate-feebs ((planet planet))
  (let ((ressurected nil)
        (energy-mean (get-feeb-parm planet :starting-energy-mean))
        (energy-sdev (get-feeb-parm planet :starting-energy-standard-deviation))
        (energy-max (get-feeb-parm planet :maximum-energy)))
    (dolist (feeb (reverse (dead-feebs planet)))
      (unless (member-if (lambda (c) (eq (corpse-of c) feeb) )
                         (carcasses planet))
        (when-let ((pnt (random-open-entry-point planet)))
          (place-thing feeb pnt)
          (setf (deadp feeb) nil)
          (setf (thing-heading feeb) (planet-random planet 4))
          (setf (ready-to-fire feeb) t)
          (setf (feeb-energy feeb)
                (round (planet-gaussian-random planet energy-mean energy-sdev 1 energy-max )))
          (setf (feeb-last-move feeb) :dead)
          (push feeb ressurected))))
    (setf (dead-feebs planet) (nset-difference (dead-feebs planet) ressurected))
    (length ressurected)))

(defgeneric kill-feeb (feeb &optional killer))

(defmethod kill-feeb ((feeb feeb) &optional (killer feeb))
  (unless (deadp feeb)
    (let* ((pnt (thing-coordinates feeb))
           (planet (planet feeb))
           (carcass (make-instance 'carcass
                                   :corpse-of feeb
                                   :planet planet
                                   :calories (get-feeb-parm planet :carcass-energy))))
      (remove-thing feeb)
      (push feeb (dead-feebs planet))
      (setf (deadp feeb) t)
      (push carcass (carcasses planet))
      (place-thing carcass pnt)
      (incf (feeb-deaths feeb))
      (incf (feeb-score feeb)
            (get-feeb-parm planet (if killer
                                      :points-for-being-killed
                                      :points-for-starving)))
      (when (and killer
                 (not (eq killer feeb)))
        (incf (feeb-kills killer))
        (incf (feeb-score killer)
              (get-feeb-parm planet :points-for-killing))))))

(defgeneric move-fireballs (planet))

(defmethod move-fireballs ((planet planet))
  (let ((deleted nil)
        (life-mean (get-feeb-parm planet :fireball-lifetime-mean))
        (life-sdev (get-feeb-parm planet :fireball-lifetime-standard-deviation))
        (reflection-prob (get-feeb-parm planet :fireball-reflection-probability)))
    (dolist (f (fireballs planet))
      (when (fireball-new-p f)
          (setf (fireball-new-p f) nil)
          (place-thing f (thing-coordinates f)))
      (unless (lifetime-cycles f)
        (setf (lifetime-cycles f)
              (round (planet-gaussian-random planet life-mean life-sdev 0))))
      (let ((rockp (member :rock (get-forward-square f))))
        (if (or (> 0 (decf (lifetime-cycles f)))
                (and rockp (planet-chance planet reflection-prob)))
            (push f deleted)
            (progn
             (if rockp
                 (turn-around f)
                 (move-forward f))
             (dolist (thing (get-current-square f))
               (cond ((mushroomp thing)
                      (remove-thing thing))
                     ((carcassp thing)
                      (setf (calories thing) 0))
                     ((feebp thing)
                      (kill-feeb thing (fireball-shooter f))
                      (push f deleted))))))
        (when (eq f (car deleted))
          (remove-thing f))))
    (setf (fireballs planet) (nset-difference (fireballs planet) deleted))
    (values (length (fireballs planet)) (length deleted))))


(defgeneric starve-feebs (planet))

(defmethod starve-feebs ((planet planet))
  (let ((starved 0))
    (dolist (f (playing-feebs planet))
      (unless (deadp f)
        (when (<= (decf (feeb-energy f)) 0)
          (kill-feeb f)
          (incf starved))))
    starved))

(defgeneric feebs-restore-firepower (planet))

(defmethod feebs-restore-firepower ((planet planet))
  (let ((restored 0))
    (dolist (f (playing-feebs planet))
      (unless (or (deadp f)
                  (ready-to-fire f))
        (when (< (decf (restore-firepower-cycles f)) 0)
          (setf (ready-to-fire f) t)
          (incf restored))))
    restored))

(defgeneric calculate-feeb-move (feeb))
(defmethod calculate-feeb-move ((feeb feeb))
  (let ((smacklisp:*smack-symbols* (feeb-lisp-env feeb ))
        (*active-feeb* feeb)
        (timeout (get-feeb-parm (planet feeb) :feeb-timeout-seconds)))
    (handler-case
        (smacklisp::interp-toplevel '(smacklisp::brain) :timeout timeout)
      (simple-condition (condition)
        (setf (feeb-last-error feeb)
              (apply 'format nil
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))
        :error)
      (serious-condition (condition)
        (setf (feeb-last-error feeb)
              (format nil "~A" condition))
        :error))))

(defgeneric test-feeb-brain (feeb))
(defmethod test-feeb-brain ((feeb feeb))
  (let ((smacklisp:*smack-symbols* (feeb-lisp-env feeb ))
        (*active-feeb* feeb)
        (timeout (get-feeb-parm (planet feeb) :feeb-timeout-seconds)))
    (with-play-active ((planet feeb))
      (smacklisp::interp-toplevel '(smacklisp::brain) :timeout timeout))))

(defgeneric execute-feeb-move (feeb action))
(defmethod execute-feeb-move ((feeb feeb) action)
  (let ((planet (planet feeb)))
   (case action
     (:turn-left
      (turn-left feeb))      
     (:turn-right
      (turn-right feeb))
     (:turn-around
      (turn-around feeb))
     (:move-forward
      (unless (member :rock (get-forward-square feeb))
        (move-forward feeb)
        (when-let ((ball (car (member-if #'fireballp (get-current-square feeb)))))
          (kill-feeb feeb (fireball-shooter ball)))))
     (:flame
      (when (ready-to-fire feeb)
        (let ((cycles (round (planet-gaussian-random
                              planet
                              :fireball-lifetime-mean
                              :fireball-lifetime-standard-deviation
                              0))))
         (push (make-instance 'fireball :planet planet 
                                        :shooter feeb
                                        :heading (thing-heading feeb)
                                        :coordinates (thing-coordinates feeb)
                                        :cycles cycles)
               (fireballs planet)))
        (decf (feeb-energy feeb)
              (get-feeb-parm planet :flame-energy))
        (setf (ready-to-fire feeb) nil)
        (setf (restore-firepower-cycles feeb)
              (round (planet-gaussian-random planet
                                             :flame-recovery-mean
                                             :flame-recovery-standard-deviation
                                             0)))))
     (:eat-mushroom
      (when-let ((shroom (car (member-if #'mushroomp (get-current-square feeb)))))
        (setf (feeb-energy feeb)
              (min (+ (feeb-energy feeb)
                      (calories shroom))
                   (get-feeb-parm planet :maximum-energy)))
        (remove-thing shroom)))
     (:eat-carcass
      (when-let ((carcass (car (member-if #'carcassp (get-current-square feeb)))))
        (setf (feeb-energy feeb)
              (min (+ (feeb-energy feeb)
                      (calories carcass))
                   (get-feeb-parm planet :maximum-energy)))
        (setf (calories carcass) 0)
        (remove-thing carcass)))
     (:peek-left
      (unless (member :rock (get-forward-square feeb))
        (setf (feeb-peeking feeb) :left)))
     (:peek-right
      (unless (member :rock (get-forward-square feeb))
        (setf (feeb-peeking feeb) :right)))
     ((:noop :error :wait)))))

(defgeneric move-feebs (planet))

(defmethod move-feebs ((planet planet))
  ;; Collect all the feebs' moves, keeping track of the time each one takes.
  (let ((total-time 0)
        (slow-feeb-noop-switch (get-feeb-parm planet :slow-feeb-noop-switch))
        (slow-feeb-noop-factor (get-feeb-parm planet :slow-feeb-noop-factor)))
    (dolist (feeb (playing-feebs planet))
      (unless (deadp feeb)
	(let ((time (get-internal-run-time)))
	  (setf (feeb-last-move feeb)
                (or (feeb-special-move feeb)
                    (calculate-feeb-move feeb)))
	  (setq time (- (get-internal-run-time) time))
	  (unless (eq :error (feeb-last-move feeb))
            (incf total-time time))
	  (setf (feeb-move-time feeb) time))))
    (setf (total-time planet) total-time)
    ;; Do all the feebs' moves, or perhaps abort the move according
    ;; to the time taken by the feeb.
    (dolist (feeb (playing-feebs planet))
      (setf (feeb-peeking feeb) nil)
      (unless (or (deadp feeb)
                  (eq :error (feeb-last-move feeb)))
        (setf (feeb-move-aborted feeb)
              (and slow-feeb-noop-switch
                   (> total-time 0)
                   (< (random 1.0)
                      (* slow-feeb-noop-factor
                         (/ (feeb-move-time feeb) total-time)))))
	(unless (feeb-move-aborted feeb)
          (execute-feeb-move feeb (feeb-last-move feeb)))))
    t))


(defgeneric play-one-cycle (planet))
(defmethod play-one-cycle ((planet planet))
  (with-play-active (planet)
    (grow-mushrooms planet)
    (decay-carcasses planet)
    (reincarnate-feebs planet)
    (move-fireballs planet)
    (starve-feebs planet)
    (feebs-restore-firepower planet)
    (compute-vision planet)
    (move-feebs planet)
    (compute-vision planet)
    (incf (cycle planet))
    (archive-planet-cycle planet)))


(defgeneric planet-inconsistencies (planet))
(defmethod  planet-inconsistencies ((planet planet))
  (let* ((mz (planet-maze planet) )
         (dims (array-dimensions mz))
         (errors nil))
    (dotimes (x (car dims))
      (dotimes (y (cadr dims))
        (dolist (thing (aref mz x y))
          (when (and (thingp thing)
                     (not (tree-equal (list x y) (thing-coordinates thing))))
            (push (list x y thing) errors)))))
    (dolist (f (append (playing-feebs planet) (fireballs planet)))
      (when (and (thing-coordinates f)
                 (not (and (fireballp f)
                           (fireball-new-p f )))
                 (not (member f (aref mz (first (thing-coordinates f))
                                      (second (thing-coordinates f))))))
        (push f errors)))
    errors))

(defgeneric play (planet &key print-maze error-stop))

(defmethod play ((planet planet) &key print-maze error-stop)
  (with-play-active (planet)
    (let ((game-length (get-feeb-parm planet :game-length)))
      (initialize-simulation planet)
      (dotimes (i game-length)
        (play-one-cycle planet)
        (when (and error-stop
                   (member-if (lambda (f) (eq :error (feeb-last-move f)))
                              (playing-feebs planet)))
          (return (values)))
        (when print-maze
          (print-maze planet))
        (when-let ((errors (planet-inconsistencies planet)))
          (return errors))))))
