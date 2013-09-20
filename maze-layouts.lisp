;;; -*- Common Lisp -*-

(in-package :smackfeebs)


;;; The mazes were
;;; Created by Jim Healy, July 1987.
;;;
;;; **************************************************
;;;  Maze guidelines:
;;;    X represents a wall.
;;;    * represents a mushroom patch.
;;;    e is a feeb entry point.
;;;
;;;  The maze should be a rectangle bounded by walls
;;;  in each side.
;;;  These mazes are all 32x32, but you may build
;;;  a maze of any size you wish.
;;; **************************************************

(defparameter *maze-layouts* nil)

(defmacro defmaze (var val)
  `(progn
     (defparameter ,var ,val)
     (pushnew (quote ,var) *maze-layouts*)))

;; default maze
(defmaze *maze-default*
  '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    "Xe   *        XXXXXXX XXXXXXXXXX"
    "XXXXX XXXXXXX XXXXXXX    * XXXXX"
    "XXXXX XXXXXXX XXXXXXX XXX XXXXXX"
    "XXXXX XXX XXX  XXXXXXeXXX XXXXXX"
    "XXXXX XXX XXXX XXXXXXXXXX XXXXXX"
    "XXXXX XXX XXXX XX XXXXXXX XXXXXX"
    "XXXXX    *  XX XX XXXXXXX XXXXXX"
    "XXXXX XXXX XXX XX*    *   XXXXXX"
    "XX   *XXXX XXX XX XXXX XXXXXXXXX"
    "XX XX XXXXeXXX XX XXXX XXXXXXXXX"
    "XX XX XXXX XXX   * *  *        X"
    "XX XX XXXX XXXXXXXX XXXXXXXXXXeX"
    "XXeXX XXXX XXXXXXXX XXXXXXXXXX X"
    "XX XX     *   *     XXXXXXXX   X"
    "XX XXXXXXXXXXX XXXX XXXXXXXX XXX"
    "XX eXXXXXXXXXX  XXXe  XXXXXX XXX"
    "XX XXXXXXXXXXXXe XXXXXXXXXXX XXX"
    "XX*  XXX XXXXXXX  XXXXXXXXXX XXX"
    "XX X  XX XXXXXXXX eXXXXXXXXX XXX"
    "XX XX  X XXXXXXXXX  XXXXXXXX XXX"
    "X  XXX  *    XXXXXX*        *  X"
    "X XXXXXX XXX XXXXXX XXXXXXXXXX X"
    "X XXXXXX XXX XXXXXX X        X X"
    "X XXXXXX XXX XXXXXX X XXXXXX X X"
    "X    *     *     XX X X  *eX X X"
    "XXXXX XXXX XXXXXXXX X XXX XX X X"
    "XXXXX XXXX XXXXX   *X XXX XX X X"
    "XXXXX XXXX XXXXX XX X    e   X X"
    "XXXXX XXXX     e XX XXX*XXXXXX X"
    "XXXXX XXXXXXXXXXXXX            X"
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))

;;; Maze1 has a good number of dead ends and little nooks.
(defmaze *maze-deadends*
  '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    "XXX     *eXXXX      *e      ** X"
    "XXX XXXXX XXXX XXXXXXXXXXXX XX X"
    "XXX   XXX      XXXXXX       X  X"
    "XXXXX XXXXXXX XXXXXXX XXXXXXX XX"
    "X *       XXX XX *    XeXX    XX"
    "X XXXXXXX XXX XXXXXXX X XXX XXXX"
    "X  XXXXXX XXX XX      X      *XX"
    "X XXXXXXX XXX XXXXXXX XXXXXXXXXX"
    "X XXXXXXX XXX*     e  XXXXXX XXX"
    "X         XXXXX XXXXXXXXX  * XXX"
    "X XXXXX XXXXXX     XXXXXX XX  XX"
    "X eXXXX XXXXXX XXX  XXXXX XX XXX"
    "X XXXXX*       XXXXe XXXX XX  XX"
    "X XXXXX XXXXXX XXXXX  XXX XXX XX"
    "X eXXXX    e   XXXXXX *XX XX  XX"
    "X XXXXX XXXXXX XXXXXXX  X XXeXXX"
    "X   XXX        XXXXXXXX   XX  XX"
    "X XXXXX XXXXXX XXXXXXXXXX XXXXXX"
    "X XXXXX      * XXXXX          XX"
    "X*  XXX XXXXXX XXXXX XXXXXX X XX"
    "X XXXXX e      XXXXX X  e   X XX"
    "X XX XX XXXXXX XXXXX X XXXXXX XX"
    "X         *XXX XXXXX       *  XX"
    "X XX XX XXXXXX XXXXXXXXXX XXXXXX"
    "X XXXXX XXXXXX   *   *        XX"
    "X   XXX XXXXXXXXXXXXXXXXXXXXX XX"
    "X XXXX    X    X   eX    X    XX"
    "X  XXX XX X XX X XX X XX X XX XX"
    "X XXXX XX X XX X XX X XX*X XX XX"
    "X e *  XX   XX * XX   XX   XXeXX"
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))

;;; Maze2 doesn't have any really long corridors.

(defmaze *maze-short-runs*
  '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    "X  eXXXXX * X   XXXXXX X e XXXXX"
    "X XXXX    X X XXXX   X   X XXXXX"
    "X  XX  XXXX   XXXX X*  X X XXXXX"
    "XX XX XXX   XXXX   XX XX X     X"
    "XX e  XXX XXXXXXX XX  XXXXXXXX*X"
    "XXXX XXX    XXXXX X e XXX   XX X"
    "XXXX XXX XXXXXXXX   XXXX  X    X"
    "XX * XX   XXe  XXXXXXXXXX XX XXX"
    "XX XXXX X XX X   XXX XXXXX   XXX"
    "XX        XX XXX X   XX    XXXXX"
    "XXXXX XXX   *XXX   X    XXXXXXXX"
    "XX*   XXXXXX  XXXX XXXX XXXXXXXX"
    "XXXXX XX XXXX XXXXXXXXX XXXXXXXX"
    "XXXXX  e XXXX   *XXXXXX   eXXXXX"
    "XXXXXXXX XXXXXXX XXXXXXXXX XXXXX"
    "XXXXXX     XXXXX  eXXXXX   XXXXX"
    "XXXXXX XXX XXXXXXX XXXXX XXXXXXX"
    "XX     XXX X   XXX XX    X    XX"
    "XX XXX   XXXXX XX   XX XXX XX XX"
    "XX XXXXX  *X   XX X XX XXXXXX*XX"
    "X    XXXXX   XXXX X      XX   XX"
    "X XX XXXXXXX   XXXXX*X X Xe XXXX"
    "X    XXXX  e X XXXXX*XX  XX XXXX"
    "X XX XXXXXX XX   XXX*XXX     XXX"
    "XXXX  eXXX  XXXX XX  XXXXX X   X"
    "XXXXXX XXXXXXXXX XX XXXX   XXX X"
    "XXX *  X X    XX    XXXX XXX X X"
    "XX  XXXX X XX XXXX XXX   X  e  X"
    "XX XX  *   X *   X XXXX XX XXX*X"
    "XX    XXX XXX XX  eXXX     XXX*X"
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))

;;; Maze3 has the minimum number of mushroom sites, most
;;; of which are between a rock and a hard place.  Those
;;; poor feebs!

(defmaze *maze-desert*
  '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    "X    e  XXXXXX XXXXXXX*XXXXXXXXX"
    "X X XXX XXXXX e   XXXX XXX e   X"
    "X   XXX XXXXXX XX XX   XXX XXX X"
    "XXX XXX XXXXXX XX XX X X   e   X"
    "Xe  XXX*XXXX*  XX XXeX X XXXXX X"
    "X X XXX XXXXXX XX XX X   XXXXX X"
    "X   XXX XXXXXX XX*   XXXXXXXXX X"
    "X XXXXX XX e   XX XXXXXXXX XXX X"
    "X X XXX XXX XXXXX XXXXXX   XXX X"
    "Xe  XXX      XXXX XXXX   X X   X"
    "XXX XXXX XXXXXXXX  X   XXX   XXX"
    "XXX  eXX XXXXXXXXX   XXXXX XXXXX"
    "XXXXX XXXXXXXXXXXX XXXXXX    XXX"
    "XXXXX     *    XX eXX XXX XX XXX"
    "XX*XXXX XXXXXX XX XXX XXX XX XXX"
    "XX X    XXXXX  X  XXX    eXX XXX"
    "X    XXXXXXXX XX XXXX XXX XX XXX"
    "X XXXXeXXXXXX    XXXX XXX XX XXX"
    "X      XX*XXXXX XXXXXXXXX    XXX"
    "XXXXXX    XXX   XXXX  XXXXXX XXX"
    "XXXXXXXXX XXX XXXXXX XXXXXX  XXX"
    "XXX       XX  e          eX XXXX"
    "XX  XXXXX    XXXX XXXX XXXX XXXX"
    "XX XXXXX  XX XXXX XXXX XXXX   XX"
    "XX eXXXX XX  XXXX XXXX XXXXXX XX"
    "XXX XX   XXX     *        XXX XX"
    "XX  XX XXXX* XXXX XXXX XXXXXX XX"
    "XXX  X XXXXX XXXX XXXX X      XX"
    "XXXX    e    XXXX XXXX X XX X  X"
    "XXXXXXXXXXXX     *e       X e XX"
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))


;;; Maze4 is symmetric about the vertical axis.  (Wow...)

(defmaze *maze-symmetric*
  '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    "X*        eXXXXXXXXXXe        *X"
    "X XXXXXXXX            XXXXXXXX X"
    "X XX   XXXXXXX XX XXXXXXX   XX X"
    "X    XeXXXXXXX XX XXXXXXXeX    X"
    "XX X XXXXXXX  eXXe  XXXXXXX X XX"
    "XX X XXXXXXX XXXXXX XXXXXXX X XX"
    "XX * XXXXXXX XXXXXX XXXXXXX * XX"
    "XX X XXXe              eXXX X XX"
    "XX X XXX XXXXXXXXXXXXXX XXX X XX"
    "XX e XXX    XXXXXXXX    XXX e XX"
    "XX X XXXXXX XXXXXXXX XXXXXX X XX"
    "XX X XXXX   XXXXXXXX   XXXX X XX"
    "XX   XXXX XXXe   eXXXX XXXX   XX"
    "XXX XXXXX XXX XXX XXXX XXXXX XXX"
    "XXX XXXXX XXX      XXX XXXXX XXX"
    "X*  XXXXX     XXXX     XXXXX  *X"
    "X XXXXX XX XX  **  XX XX XXXXX X"
    "X XXXXX XX XX XXXX XX XX XXXXX X"
    "X XXX e XX XX XXXX XX XX e XXX X"
    "X XXXXX XX    XXXX    XX XXXXX X"
    "X XXXXX XXXXX XXXX XXXXX XXXXX X"
    "X     X XXXXX XXXX XXXXX X     X"
    "XXXXX  *                *  XXXXX"
    "XXXXX XXXXXXXX XX XXXXXXXX XXXXX"
    "XXXXX XXXXXXXX XX XXXXXXXX XXXXX"
    "XXXXX    XXXXX XX XXXXX    XXXXX"
    "XXXX  XX  XXXXeXXeXXXX  XX  XXXX"
    "XXX  XXXX  XXX XX XXX  XXXX  XXX"
    "XXX XXXXXX XXX XX XXX XXXXXX XXX"
    "XX*       e    XX    e       *XX"
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))

;;; Maze5 has a lot of long corridors good for feeb showdowns.
;;; Furthermore, all the feeb entry-points are in the corridors.
;;; You can run but you can't hide!

(defmaze *maze-showdown*
  '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    "X              e           e   X"
    "X XXXXXXX*XXXXXXXXXXXX XXXXXXX X"
    "X               e              X"
    "X X XXXXX XXXXXXXXXXXX XXXXX X X"
    "X              *               X"
    "X X XX XX XXXXXXXXXXXX XX XX X X"
    "X X XX XX XXXXXXXXXXXX XXeXX X X"
    "X X XX XX    *         XX XX X X"
    "XeX XX XX XXXXXXXXXXXX XX XX X X"
    "X X XX XX XXXXXXXXXXXX XX XX X X"
    "X X XX XX          e   XX XXeX X"
    "X X XXeXX XXXXXXXXXXXX XX XX X X"
    "X X XX XX XXXXXXXXXXXX XX XX XeX"
    "X*X XX XX              XX XX X X"
    "X X XX XX XXXXXXXXXXXX XX XX X X"
    "X XeXX XX XXXXXXXXXXXX*XX XX X X"
    "X X XX XX  *           XX XX*X X"
    "X X XX XX XXXXXXXXXXXX XX XX X X"
    "X X XX XX XXXXXXXXXXXX XX XX X X"
    "X X XX XX  e           XX XX*X X"
    "X X XX*XX XXXXXXXXXXXX XX XX X X"
    "X X XX XX XXXXXXXXXXXX XX XX X X"
    "X X XX XX              XX XXeX X"
    "X X XX XX XXXXXXXXXXXX XX XX X X"
    "X X XX XX XXXXXXXXXXXX XX XX X X"
    "X             e                X"
    "X*X XXXXX XXXXXXXXXXXX XXXXX X*X"
    "X             e       *        X"
    "X XXXXXXX XXXXXXXXXXXX XXXXXXX X"
    "X     e        *               X"
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
