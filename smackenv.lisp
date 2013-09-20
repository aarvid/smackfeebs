(in-package :smackfeebs)
(defparameter *active-feeb* nil)
(defparameter *planets* nil)


(defun make-planet-name (base-name)
  (do* ((n  0 (1+ n))
        (name (format nil "~a-~a" base-name n)
              (format nil "~a-~a" base-name n)))
       ((not (member name *planets* :key #'planet-name :test #'string=))
        name)))

(defun unique-feeb-name-p (name)
  (not (find-if (lambda (pl)
                  (member name (defined-feebs pl)
                          :key #'feeb-name :test #'string=))
                *planets*)))

(defun make-feeb-name (base-name)
  (do* ((n  0 (1+ n))
        (name (format nil "~a-~a" base-name n)
              (format nil "~a-~a" base-name n)))
       ((unique-feeb-name-p name)
        name)))

(defun create-planet-with-feeb (planet-name feeb &key private)
  (let ((pl (make-instance 'planet :name planet-name :private private)))
    (setf (planet feeb) pl)
    (push feeb (defined-feebs pl))
    (initialize-simulation pl)
    (push pl *planets*)
    pl))

(defun feeb-leave-planet (planet feeb)
  (remove-feeb planet feeb)
  (when (= 0 (length (defined-feebs planet)))
    (removef *planets* planet))  )

(defun ensure-planet ()
  (let ((pl (planet *active-feeb*)))
    (if (typep pl 'planet)
        pl
        (error "Feeb is not a member of a planet!"))))

(defgeneric feeb-repl (feeb))
(defmethod feeb-repl ((feeb feeb))
  (let ((*active-feeb* feeb)
        (smacklisp:*smack-symbols* (feeb-lisp-env feeb)))
    (smacklisp:smack)))

(defun api-feeb-heading ()
  (thing-heading *active-feeb*))


(defun api-feeb-name ()
  (feeb-name  *active-feeb*))

(defun api-set-feeb-name (name)
  (when-let (pl (planet *active-feeb*))
    (when (member name (defined-feebs pl) :key #'feeb-name :test #'string=)
      (error "The Planet already has a feeb with this name."))
    (when (play-is-active pl)
      (error "Cannot change name within an active game!")))
  (setf (feeb-name *active-feeb*) name))

(defun api-coordinates ()
  (copy-list (thing-coordinates *active-feeb*)))


(defun api-planet-name ()
  (planet-name  (ensure-planet)))



(defun api-feeb-peeking ()
  (feeb-peeking *active-feeb*))

(defun api-feeb-energy ()
  (feeb-energy *active-feeb*))

(defun api-line-of-sight ()
  (line-of-sight *active-feeb*))

(defun api-feeb-score ()
  (feeb-score *active-feeb*))

(defun api-feeb-kills ()
  (feeb-kills *active-feeb*))

(defun api-feeb-last-move ()
  (feeb-last-move *active-feeb*))

(defun api-move-aborted-p ()
  (feeb-move-aborted *active-feeb*))

(defun api-ready-to-fire-p ()
  (ready-to-fire *active-feeb*))


(defun api-current-square ()
  (current-square *active-feeb*))

(defun api-rear-square ()
  (rear-square  *active-feeb*))

(defun api-left-square ()
  (left-square  *active-feeb*))

(defun api-right-square ()
  (right-square  *active-feeb*))


(defun api-vision-ahead ()
  (vision-ahead  *active-feeb*))

(defun api-vision-right ()
  (vision-right  *active-feeb*))

(defun api-vision-left ()
  (vision-left  *active-feeb*))

(defun api-default-brain ()
  :noop)

(defun api-get-parm (parm)
  (get-feeb-parm (ensure-planet) parm))

(defun api-set-parm (parm value)
  (let ((planet (ensure-planet)))
    (if (play-is-active planet)
        (error "Cannot change parameters within an active game!")
        (change-feeb-parm planet parm value))))

(defun api-feeb-image-p (image)
  (feeb-image-p image))

(defun api-feeb-image-name (image)
  (feeb-image-name image))

(defun api-feeb-image-heading (image)
  (feeb-image-heading image))

(defun api-fireball-image-p (image)
  (fireball-image-p image))

(defun api-fireball-image-shooter (image)
  (fireball-image-shooter image))

(defun api-fireball-image-direction (image)
  (fireball-image-direction image))

(defun api-print-maze ()
  (let ((planet (ensure-planet)))
    (if (play-is-active planet)
        (error "Cannot print-maze within an active game!")
        (print-maze planet))))

(defun api-print-scoreboard ()
  (let ((planet (ensure-planet)))
    (if (play-is-active planet)
        (error "Cannot print-scorecard within an active game!")
        (print-scoreboard planet))))

(defun api-test-feeb-brain ()
  (if (play-is-active (ensure-planet))
      (error "Cannot test-feeb-move within an active game!")
      (test-feeb-brain *active-feeb*)))

(defun api-list-parameter-settings ()
  (mapcar (lambda (x) (list (first x) (second x)))
          (list-parameter-settings (ensure-planet))))

(defun api-play-cycle (&optional (count 1))
  (let ((planet (ensure-planet)))
    (when (play-is-active planet)
      (error "Cannot call play-cycle within an active game!"))
    (dotimes (i count (cycle planet))
      (play-one-cycle planet))))

(defun api-play-game ()
  (let ((planet (ensure-planet)))
    (when (play-is-active planet)
      (error "Cannot call play-cycle within an active game!"))
    (play planet)
    (cycle planet)))

(defun api-play-restart ()
  (let ((planet (ensure-planet)))
    (when (play-is-active planet)
      (error "Cannot call play-cycle within an active game!"))
    (initialize-simulation planet)
    (cycle planet)))

(defun api-maze-layouts ()
  (when (play-is-active (ensure-planet))
    (error "Cannot call maze-layouts within an active game!"))
  (mapcar (lambda (sym) (intern (symbol-name sym) :smacklisp))
          *maze-layouts*))

(defun api-change-maze-layout (layout)
  (unless (typep layout 'symbol)
    (error "Parameter layout has to be a symbol!"))
  (let ((planet (ensure-planet)))
    (when (play-is-active planet)
      (error "Cannot call play-cycle within an active game!"))
    (if-let ((new-layout
              (car (member-if (lambda (lo) (string= (symbol-name layout)
                                                    (symbol-name lo)))
                              *maze-layouts*))))
            (when (load-text-maze planet (symbol-value new-layout))
              (initialize-simulation planet)
              t)
            (error "Parameter layout has to be a symbol returned by (maze-layouts)!"))))

(defun api-planet-list ()
  (mapcar #'planet-name *planets*))

(defun api-planet-feebs (&optional planet-name)
  (if-let ((pl (if planet-name
                   (find planet-name *planets* :key #'planet-name :test #'string=)
                   (ensure-planet))))
    (mapcar #'feeb-name (defined-feebs pl))
    (error "There does not exists a planet with this name: ~a" planet-name)))



(defun api-planet-create (name &key private)
  (when (planet *active-feeb*)
    (error "Feeb is already a member of a planet. Use (planet-leave) first."))
  (when (member name *planets* :key #'planet-name :test #'string=)
    (error "Already exists a planet with this name: ~a" name))
  (create-planet-with-feeb name *active-feeb* :private private)
  name)

(defun api-planet-join (name)
  (when (planet *active-feeb*)
    (error "Feeb is already a member of a planet. Use (planet-leave) first."))
  (if-let ((pl (find name *planets* :key #'planet-name :test #'string=)))
    (let ((capacity (planet-capacity pl)))
      (when (planet-private pl)
        (error "Planet ~a is private. You cannot join." name))
      (when (>= (length (defined-feebs pl)) capacity)
        (error "Planet ~a is already at capacity. You cannot join." name))
      (when (member (feeb-name *active-feeb*) (defined-feebs pl)
                    :key #'feeb-name :test #'string=)
        (error "Planet ~a already has a feeb with your name. You cannot join." name))
      (when (>= (length (playing-feebs pl)) capacity)
        (bump-system-feeb pl))
      (setf (planet *active-feeb*) pl)
      (push *active-feeb* (defined-feebs pl))
      (place-feeb-in-maze pl *active-feeb*)
      name)
    (error "There does not exists a planet with this name: ~a" name)))


(defun api-planet-leave ()
    (let ((planet (ensure-planet)))
      (when (play-is-active planet)
        (error "Cannot leave an active planet!"))
      (feeb-leave-planet planet *active-feeb*)
      t))

(defparameter *feeb-functions*
  '(
    (smacklisp::planet-name api-planet-name)    
    (smacklisp::feeb-heading api-feeb-heading)
    (smacklisp::feeb-name api-feeb-name)
    (smacklisp::set-feeb-name api-set-feeb-name)
    (smacklisp::coordinates api-coordinates)
    (smacklisp::feeb-peeking api-feeb-peeking)
    (smacklisp::feeb-energy api-feeb-energy)
    (smacklisp::line-of-sight api-line-of-sight)        
    (smacklisp::feeb-score api-feeb-score)
    (smacklisp::feeb-kills api-feeb-kills)
    (smacklisp::feeb-last-move api-feeb-last-move)
    (smacklisp::move-aborted-p api-move-aborted-p)
    (smacklisp::ready-to-fire-p api-ready-to-fire-p)
    (smacklisp::current-square api-current-square)
    (smacklisp::rear-square api-rear-square)
    (smacklisp::left-square api-left-square)
    (smacklisp::right-square api-right-square)
    (smacklisp::vision-ahead api-vision-ahead)
    (smacklisp::vision-right api-vision-right)
    (smacklisp::vision-left api-vision-left)
    (smacklisp::feeb-image-p api-feeb-image-p)
    (smacklisp::feeb-image-name api-feeb-image-name)
    (smacklisp::feeb-image-heading api-feeb-image-heading)
    (smacklisp::fireball-image-p api-fireball-image-p)
    (smacklisp::fireball-image-shooter api-fireball-image-shooter)
    (smacklisp::fireball-image-direction api-fireball-image-direction)
    (smacklisp::brain api-default-brain)
    (smacklisp::get-parm api-get-parm)
    (smacklisp::set-parm api-set-parm)
    (smacklisp::print-maze api-print-maze)
    (smacklisp::print-scoreboard api-print-scoreboard)
    (smacklisp::test-feeb-brain api-test-feeb-brain)
    (smacklisp::list-parameter-settings api-list-parameter-settings)
    (smacklisp::play-cycle api-play-cycle)
    (smacklisp::play-restart api-play-restart)
    (smacklisp::play-game api-play-game)
    (smacklisp::maze-layouts api-maze-layouts)
    (smacklisp::change-maze-layout api-change-maze-layout)
    (smacklisp::planet-list api-planet-list) 
    (smacklisp::planet-feebs api-planet-feebs) 
    (smacklisp::planet-create api-planet-create)
    (smacklisp::planet-join api-planet-join) 
    (smacklisp::planet-leave api-planet-leave)
    ))


(defun create-lisp-environment ()
  (let* ((env (make-hash-table))
         (smacklisp:*smack-symbols* env))
    (smacklisp:init-smack-interp)
    (mapc #'smacklisp::link-smack-cl-function *feeb-functions*)
    env))
