(in-package :smackfeebs)





(defun chain-slot-value (object slots)
  (slot-value (if (sequence-of-length-p slots 1)
                  object
                  (chain-slot-value object (cdr slots)))
              (car slots)))

(defun object-to-alist (object slots)
  (mapcar (lambda (slot) (cons (format-symbol 'smackfeebs "~{~a~^-~}"
                                              (reverse (ensure-list slot)))
                               (chain-slot-value object (ensure-list slot))))
          slots))


(defgeneric playing-feebs->alist (planet))
(defmethod playing-feebs->alist ((planet planet))
 (mapcar (rcurry #'object-to-alist
                 '(id name deadp energy score
                   kills deaths
                   heading coordinates))
         (playing-feebs planet)))

(defgeneric archive-planet-cycle (planet))
(defmethod archive-planet-cycle ((planet planet))
  (with-slots (archives cycle playing-feebs fireballs carcasses
               game-number) planet
    (when (<= (length archives) cycle)
      (let ((mushrooms (planet-mushrooms planet)))
        (vector-push-extend
         (list (cons 'cycle cycle)
               (cons 'game-number game-number)
               (cons 'feebs (playing-feebs->alist planet))
               (cons 'fireballs (mapcar (rcurry #'object-to-alist
                                                '(id (name shooter) heading coordinates))
                                        fireballs))
               (cons 'mushrooms (mapcar (rcurry #'object-to-alist
                                                '(id calories coordinates))
                                        mushrooms))
               (cons 'carcasses (mapcar (rcurry #'object-to-alist
                                                '(id (name corpse-of) coordinates))
                                        carcasses)))
         archives)))
    cycle))
