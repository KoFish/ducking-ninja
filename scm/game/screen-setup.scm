(define-module (game screen-setup)
               #:export (setup
                         destroy))

(use-modules (ncurses curses)
             (util debug))

(define setup #f)
(define destroy #f)

(let ((stdscr (initscr)))
  (set! setup (λ ()
                 (raw!)
                 (keypad! stdscr #t)
                 (noecho!)
                 stdscr))
  (set! destroy (λ ()
                   (endwin))))
