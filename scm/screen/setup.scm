(define-module 
  (screen setup)
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
                 (start-color!)
                 (debug "color-pairs ~a\ncolors ~a\n" (color-pairs) (colors))
                 stdscr))
  (set! destroy (λ () (endwin))))
