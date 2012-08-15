(define-module (game main-menu)
               #:export (run))

(use-modules (ncurses curses)
             (srfi srfi-26)
             (ice-9 match))

(use-modules (util debug)
             (util reader)
             (util colors)
             (ninja menu))

(define (draw-main-menu scr)
  (let* ((theme (make-ninja-menu-color-theme
                  0  #| foreground |#
                  0  #| background |#
                  15 #| title      |#
                  15 #| prompt     |#
                  15 #| enabled    |#
                  8  #| disabled   |#
                  7  #| short      |#
                  8  #| bracket    |#
                  ))
         (menu (make-ninja-menu "Ducking ninja!" theme))) 
    (add-item! menu "New Game" #\n 'new-game #t) 
    (add-item! menu "Options" #\o 'show-options #t) 
    (add-item! menu "Color test" #\c 'color-test #t) 
    (add-item! menu "Help" #\h 'show-help #f)
    (add-blank! menu) 
    (add-item! menu "Quit" #\q 'quit #t) 
    (let redraw-menu ((state #f))
      (clear scr)
      (touchwin scr)
      (refresh scr)
      (debug "Setup menu\n") 
      (let redraw-loop
        ((mx   (getmaxx scr)) 
         (my   (getmaxy scr))) 
        (match (catch 'window-resize
                 (λ () (do-menu menu scr (- my 2) (- mx 2) 1 1))
                 (λ (key . args)
                    (redraw-loop (getmaxx scr) (getmaxy scr))))
          ('new-game 
           ((@ (game game-screen) start-game) scr)
           (redraw-menu state))
          ('show-options 
           (addstr scr "Do the option screen")
           (get-char scr)
           (redraw-menu state) )
          ('color-test 
           ((@ (util colors) draw-test-screen) scr)
           (redraw-menu state) )
          ('show-help 
           (addstr scr "There is no help-screen")
           (get-char scr)
           (redraw-menu state) )
          ('quit (addstr scr "I'm leaving now!!"))
          (_ #f))))))

(define (run stdscr)
  (debug "This is where we setup the game\n")
  ((@ (game start-screen) draw) stdscr) 
  (set-color! "bold yellow" COLOR_YELLOW COLOR_BLACK A_BOLD) 
  (set-color! "silly color" COLOR_BLUE COLOR_YELLOW A_BOLD)
  (set-color! "puke" COLOR_GREEN 11)
  (draw-main-menu stdscr)
  (let ((ch (get-char stdscr)))
    (if (eq? ch #\q)
        (throw 'game-done "Nothing to do here!"))))
