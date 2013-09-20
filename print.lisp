(in-package :smackfeebs)


(defun print-heading (heading)
  (case heading
    (0 #\^)
    (1 #\>)
    (2 #\v)
    (3 #\<)))


(defun print-direction (dir)
  (case dir
    (0 #\N)
    (1 #\E)
    (2 #\S)
    (3 #\W)))

;; for new function print-map-square
(defun print-direction-arrow (dir)
  (case dir
    (0 #\^)
    (1 #\>)
    (2 #\v)
    (3 #\<)))


(defgeneric print-layout (planet))
(defmethod print-layout ((planet planet))
  (let* ((maze (maze-layout planet))
         (dim  (array-dimensions maze)))
    (dotimes (y (cadr dim))
      (dotimes (x (car dim))
        (princ
         (string
          (case (aref maze x y)
            (:rock (print-rock maze x y))
            (:feeb-entry-place #\^)
            (:mushroom-place #\*)
            ((nil) #\Space)
            (t #\?)))))
      (terpri))))


(defgeneric print-maze (planet))

(defmethod print-maze ((planet planet))
  (let* ((maze (planet-maze planet))
         (dim  (array-dimensions maze)))
    (princ #\space)
    (dotimes (x (car dim))
      (princ (mod x 10)))
    (terpri)
    (dotimes (y (cadr dim))
      (princ (mod y 10))
      (dotimes (x (car dim))
        (princ
         (string
          (if (rockp maze x y)
              (print-rock maze x y)
              (print-maze-char (car (aref maze x y)))))))
      (princ (mod y 10))
      (terpri))
    (princ #\space)
    (dotimes (x (car dim))
      (princ (mod x 10)))
    (terpri)))

(defun rockp (maze x y)
  (let ((dim  (array-dimensions maze)))
    (when (and (<= 0 y)
               (< y (cadr dim))
               (<= 0 x)
               (< x (car dim)))
      (eq (ensure-car (aref maze x y))
          :rock))))

(defun print-rock (maze x y) 
  (let ((left (rockp maze (1- x) y))
        (right (rockp maze (1+ x) y))
        (above (rockp maze x (1- y)))
        (below (rockp maze x (1+ y))))
    (code-char
     (cond ((and above below left right)
            #x256C)
           ((and above below left )
            #x2563)
           ((and above below right)
            #x2560)
           ((and above below )
            #x2551)
           ((and above left right)
            #x2569)
           ((and above left )
            #x255D)
           ((and above right)
            #x255A)
           ((and above)
            #x2551)
           ((and below left right)
            #x2566)
           ((and below left)
            #x2557)
           ((and below right)
            #x2554)
           ((and below)
            #x2551)
           ((and left right)
            #x2550)
           ((and left)
            #x2550)
           ((and right)
            #x2550)
           (t #x256c)))))

(defgeneric print-maze-simple (planet))
(defmethod print-maze-simple ((planet planet))
  (let* ((maze (planet-maze planet))
         (dim  (array-dimensions maze)))
    (flet ((rockp (x y)
             (when (and (<= 0 y)
                        (< y (cadr dim))
                        (<= 0 x)
                        (< x (car dim)))
               (eq (car (aref maze x y))
                   :rock))))
     (dotimes (y (cadr dim))
       (dotimes (x (car dim))
         (princ
          (string
           (if (rockp x y)
               #\+
               #\Space))))
       (terpri)))))

(defun print-maze-test (maze)
  (let ((pl (make-instance 'planet)))
    (load-text-maze pl  maze)
    (print-maze pl)))

(defgeneric print-scoreboard (planet))
(defmethod print-scoreboard ((planet planet))
  (format t
          "~10a ~6@a ~6@a ~6@a ~6@a ~a~%"
          "Name"
          "Score" 
          "Energy"
          "Kills" 
          "Deaths"
          "Coordinates")
  (dolist (fb (sort (playing-feebs planet)
                    #'> :key #'feeb-score))
    (format t
            "~10a ~6d ~6d ~6d ~6d ~a~%"
            (feeb-name fb)
            (feeb-score fb)
            (feeb-energy fb)
            (feeb-kills fb)
            (feeb-deaths fb)
            (thing-coordinates fb)))
  nil)
