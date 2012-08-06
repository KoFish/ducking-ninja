(define-module (game start-screen)
               #:export (draw))

(use-modules (ncurses curses)
             (util reader)
             (util debug))

(define welcome-message "Lets start this up")
(define i 0)

(define (draw scr)
  (let ((as (add-async 300 (λ ()
                              (addstr scr (format #f "[ ~a ]" i) #:x 0 #:y 0)
                              (set! i (1+ i))))))
    (let redraw-loop ((mx (getmaxx scr))
                      (my (getmaxy scr)))
      (catch 'window-resize
        (λ ()
           (erase scr)
           (addstr scr welcome-message #:x (- (floor/ mx 2) (floor/ (string-length welcome-message) 2)) #:y (floor/ my 2)) 
           (get-char scr)
           (drop-async as))
        (λ (key . args)
           (redraw-loop (getmaxx scr) (getmaxy scr)))))))
