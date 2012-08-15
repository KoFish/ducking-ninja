(define-module (game game-screen)
               #:export (start-game))

(use-modules (ncurses curses))

(use-modules (util debug)
             (util reader)
             (util colors))

(define (start-game scr)
  (clear scr)
  (touchwin scr)
  (refresh scr)
  (addstr scr "So lets do some game all up in here!" #:y 0 #:x 0)
  (get-char scr))
