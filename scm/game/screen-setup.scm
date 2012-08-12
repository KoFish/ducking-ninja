(define-module (game screen-setup)
               #:export (setup
                         destroy))

(use-modules (ncurses curses)
             (util colors)
             (util debug))

(define setup #f)
(define destroy #f)

(let ((stdscr (initscr)))
  (set! setup (λ ()
                 (with-throw-handler #t
                   (λ ()
                      (setup-colors!)
                      (add-color! "menu title" COLOR_YELLOW COLOR_BLACK A_BOLD) 
                      (add-color! "menu option" COLOR_GREEN COLOR_BLACK) 
                      (add-color! "menu disabled" 8 COLOR_BLACK))
                   (λ (key . args)
                      (destroy))) 
                 (raw!)
                 (keypad! stdscr #t)
                 (noecho!)
                 stdscr))
  (set! destroy (λ ()
                   (endwin))))
