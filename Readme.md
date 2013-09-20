__Planet of the Feebs__ is a game based on the original version [available here](http://www.cliki.net/Planet%20of%20the%20Feebs). Quoting the Cliki: "__Planet of the Feebs__ was originally developed at the beginning of the nineties[?] by Scott Falhman, Skef Wholey and others at Carnegie-Mellon University, and it was placed in the public domain." This game also takes inspiration from [The Feebs War](http://common-lisp.net/project/the-feebs-war/) by Gustavo Henrique Milaré.  This version is purely repl/web based and uses a separate [smacklisp](https://github.com/aarvid/SmackLisp) interpreter for each feeb. That is, each feeb is programmed in smacklisp with a separate programming space. The game itself still runs in Common Lisp. The web version is at [smacklisp.com](http://smacklisp.com). 

The game is rather simple. A feeb needs to traverse the maze searching and killing other feebs.  The feeb also needs to search for mushrooms or carcasses to eat to maintain its energy.  If energy runs out, the feeb starves and dies. If the feeb is flamed,  he dies.  The carcass of the feeb will last for a random number of turns, and then the feeb will be reincarnated at a random entry point. Each turn cost the feeb energy and flaming costs energy.  After a feeb flames, he will need to recover before flaming again.

The user will need to write a function named "brain" to control his feeb.  The "brain" function must have this name and needs to return a keyword symbol from the list: __:turn-left__, __:turn-right__, __:turn-around__, __:move-forward__, __:flame__, __:eat-mushroom__, __:eat-carcass__, __:peek-left__, __:peek-right__,  __:noop__, __:wait__.  The feeb has his own smacklisp environment and can define whatever helper functions or global variables he wishes.  The feeb has no direct access to the game or access to the smacklisp environment of other feebs.  Access to the game and maze environment is done through API functions defined in the {feeb-ref "" "" [SmackFeebs API Reference](http://www.smacklisp.com/reference/feeb/) in the SmackRef tab. 

After defining his brain, the user can play the game by calling the function  "play-game" from the _repl_.  The user can also test his brain with the functions "play-cycle", "play-restart", "test-feeb-brain".  Also all API functions are available to the user through the _repl_.

The advanced user can modify the game by changing parameters and changing the maze layout. Use the function "set-parm". The parameters  are also defined in the API. "maze-layouts" lists the mazes available and "change-maze-layout"} allows the user to change the maze.

You need to create "planet-create" or join "planet-join" a planet.  Use "set-feeb-name" to personalize your name.  

It is advisable to create scripts that you can load. 



The following is a brain used by the system feebs:

    ;; smacklisp code for auto-brain
    (defun feeb-in-sight ()
      (find-if (lambda (sq) (member-if #'feeb-image-p sq))
               (vision-ahead)))

    (defun fireball-in-sight ()
      (find-if (lambda (sq) (member-if #'fireball-image-p sq))
               (vision-ahead)))

    (defun brain ()
      (let ((here (current-square)))
        (cond ((and (member :mushroom here)
                    (< (feeb-energy)
                       (get-parm :maximum-energy)))
               :eat-mushroom)
              ((and (member :carcass here)
                    (< (feeb-energy)
                       (get-parm :maximum-energy)))
               :eat-carcass)
              ((and (ready-to-fire-p)
                    (> (- (feeb-energy)
                          (get-parm :flame-energy))
                       10)
                    (feeb-in-sight))
               :flame)
              ((and (not (member :rock (left-square)))
                    (not (eq :turn-right (feeb-last-move)))
                    (not (find-if #'fireball-image-p (left-square)))
                    (or (member :mushroom (left-square))
                        (> 0.3 (random 1.0))))
               :turn-left)
              ((and (not (member :rock (right-square)))
                    (not (eq :turn-left (feeb-last-move)))
                    (not (find-if #'fireball-image-p (right-square)))
                    (or (member :mushroom (right-square))
                        (> 0.3 (random 1.0))))
               :turn-right)
              ((and (> (length (vision-ahead)) 0)
                    (not (fireball-in-sight)))
               :move-forward)
              ((not (member :rock (rear-square)))
               :turn-around)
              ((not (member :rock (right-square)))
               :turn-right)
              ((not (member :rock (left-square)))
               :turn-left)
              (t :wait))))
