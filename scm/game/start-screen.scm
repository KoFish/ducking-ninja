(define-module (game start-screen)
               #:export (draw))

(use-modules (ncurses curses)
             (util reader)
             (util colors)
             (util debug))

(define welcome-message (color-string "%c4;Lets%c %bstart%b %c1this%c %c2up%c"))
(define i 0)
(define idle-animation (map-in-order color-string
                         (list 
                           "%c3%bo%b%c        "
                           "%c3o%bo%b%c       "
                           "%c3-o%bo%b%c      "
                           " %c3-o%bo%b%c     "
                           "  %c3-o%bo%b%c    "
                           "   %c3-o%bo%b%c   "
                           "    %c3-o%bo%b%c  "
                           "     %c3-o%bo%b%c "
                           "      %c3-o%bo%b%c"
                           "       %c3-%bo%b%c"
                           "        %c3%bo%b%c"
                           "       %c3%bo%bo%c"
                           "      %c3%bo%bo-%c"
                           "     %c3%bo%bo-%c "
                           "    %c3%bo%bo-%c  "
                           "   %c3%bo%bo-%c   "
                           "  %c3%bo%bo-%c    "
                           " %c3%bo%bo-%c     "
                           "%c3%bo%bo-%c      "
                           "%c3%bo%b-%c       "
                           )))

(define (draw scr)
  (let ((as (add-async 300 (λ ()
                              (addchstr scr (color-string "[ %i%c4;~a%c%i ]" i) #:x 0 #:y 0)
                              (addch scr (invis #\X) #:x (1- (getmaxx scr)) #:y (1- (getmaxy scr)))
                              (set! i (1+ i))))))
    (let redraw-loop ((mx (getmaxx scr))
                      (my (getmaxy scr)))
      (catch 'window-resize
        (λ ()
           (erase scr)
           (let ((x (floor/ mx 2))
                 (y (floor/ my 2))
                 (msg-length (length welcome-message)))
             (addchstr scr welcome-message #:x (- x (floor/ msg-length 2)) #:y y) 
           (get-char scr (λ (c)
                            (let ((c (or c 0)))
                              (let ((i (floor-remainder c (length idle-animation))))
                                (addchstr scr (list-ref idle-animation i) #:x (- x 4) #:y (+ y 2))
                                (addch scr (invis #\X) #:x (1- mx) #:y (1- my))
                                (1+ i)))) 200)
           (drop-async as)))
        (λ (key . args)
           (redraw-loop (getmaxx scr) (getmaxy scr)))))))
