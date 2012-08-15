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
                      (setup-colors!))
                   (λ (key . args)
                      (destroy))) 
                 (raw!)
                 (keypad! stdscr #t)
                 (noecho!)
                 stdscr))
  (set! destroy (λ ()
                   (endwin))))
