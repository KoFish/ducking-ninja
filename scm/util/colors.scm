(define-module (util colors)
               #:export (setup-colors!
                         get-color
                         add-color!
                         color-string
                         with-color
                         draw-test-screen))

(use-modules (ice-9 vlist)
             (ice-9 match)
             (ice-9 receive)
             (srfi srfi-1)
             (ice-9 i18n)
             (ncurses curses))

(use-modules (util debug)
             (util reader))

(define setup-colors! #f)
(define get-color #f)
(define add-color! #f)
(define fold-colors #f)
(define get-nr-of-colors #f)

(define-syntax-rule (with-color scr name e e* ...)
  (cond ((get-color name) 
         => (λ (col)
               (attr-on! scr col)
               e
               e* ...
               (attr-off! scr col)))
        (else (debug "Could not set color\n") e e* ...)))

(define (color-string s . rest)
  (let parse ((s (string->list (apply format #f s rest)))
              (modifiers '())
              (color-stack '())) 
    (let ((cons-c (λ (c r)
                     (let ((composed (if (null? modifiers)
                                         normal 
                                         (apply compose modifiers))))
                       (cons (if (null? color-stack)
                                 (composed c)
                                 (color (car color-stack) (composed c))) 
                             (parse r modifiers color-stack)))))
          (parse-with (λ (mod r) (parse r (if (find (λ (x) (eq? x mod)) modifiers)
                                              (delete mod modifiers)
                                              (append modifiers (cons mod '())))
                                        color-stack))))
      (if (null? s) '()
          (receive (c r) (car+cdr s)
            (cond ((char=? c #\%)
                   (receive (c r) (car+cdr r)
                     (cond ((char=? c #\b) (parse-with bold-on r))
                           ((char=? c #\i) (parse-with inverse-on r))
                           ((char=? c #\d) (parse-with dim-on r))
                           ((char=? c #\f) (parse-with blink-on r))
                           ((char=? c #\u) (parse-with underline-on r))
                           ((char=? c #\c) (receive (col r) (span char-numeric? r)
                                             (let ((col (locale-string->integer (list->string col))))
                                               (parse (if (and (not (null? r)) (char=? (car r) #\;)) (cdr r) r) modifiers (if col
                                                                      (cons col color-stack)
                                                                      (cdr color-stack))))))
                           (else (cons-c c r)))))
                  (else (cons-c c r))))))))

(let ((%color-vlist vlist-null)
      (pair-counter #f)
      (default-colors (list
                        (list "default" COLOR_WHITE COLOR_BLACK)
                        (list "inverse" COLOR_BLACK COLOR_WHITE)
                        (list "red" COLOR_RED COLOR_BLACK))))
  (set! setup-colors! 
    (λ ()
       (if (eq? %color-vlist vlist-null)
           (begin 
             (debug "Setup colors\n")
             (start-color!)
             (debug " Colors available: ~a\n" (colors))
             (debug " Color pairs available: ~a\n" (color-pairs))
             (debug "  - Setup default color-pairs..\n")
             (for-each (λ (i) (init-pair! i i 0) 
                          (debug "   - Color ~a\n" i)) 
                       (iota 16 1)) 
             (debug "  - Setup named color-pairs..\n")
             ;(set! pair-counter (color-pairs))
             (set! pair-counter 255) 
             (map (λ (c) (apply add-color! c)) default-colors)))))

  (set! get-color (λ (name) 
                     (cond ((vhash-assv (string-hash name) %color-vlist)
                            => (λ (c) (cddr c)))
                           (else #f))))

  (set! fold-colors (λ (ƒ init)
                      (vhash-fold ƒ init %color-vlist)))

  (set! get-nr-of-colors (λ () (- #|(color-pairs)|# 255 pair-counter))) 

  (set! add-color! (λ (name fg bg . attrs)
                      (let ((i pair-counter))
                        (init-pair! i fg bg)
                        (set! pair-counter (1- pair-counter)) 
                        (set! %color-vlist (vhash-consv (string-hash name) (cons name (logior (color-pair i) (apply logior attrs))) %color-vlist))))))

(define (draw-test-screen scr)
  (let redraw-loop ((mx (getmaxx scr))
                    (my (getmaxy scr)))
    (addstr scr "Print color test" #:x 0 #:y 0)
    (debug "Nr. of defined color pairs: ~a\n" (get-nr-of-colors))
    (for-each (λ (i)
                 (addchstr scr (color-string "%c1normal %ddim%d %bbold%b %iinverse%i %fflash%f %uunderline%u%c") #:y 1 #:x 0)
                 (addstr scr (format #f "~a" i) #:y (+ 2 i) #:x 0)
                 (addchstr scr (color-string "%c~aXX%dXX%d%bXX%b%iXX%i%fXX%f%uXX%u" i) #:y (+ 2 i) #:x 5)) 
              (iota (- my (+ 4 (get-nr-of-colors))))) 
    (fold-colors (λ (name-hash name-value y)
                    (let ((name (car name-value))
                          (value (cdr name-value))) 
                      (attr-on! scr value)
                      (addstr scr (format #f "~s" name) #:y y #:x 0) 
                      (attr-off! scr value) 
                      (1- y))) (- my 2))
    (get-char scr))) 
