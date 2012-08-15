(define-module (util colors)
               #:export (setup-colors!
                         get-color
                         set-color!
                         get-pair!
                         make-attr!
                         color-string
                         with-attr
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
(define set-color! #f)
(define get-pair! #f)
(define make-attr! #f)
(define fold-colors #f)
(define get-nr-of-colors #f)
(define get-nr-of-pairs #f)

(let ((%color-dict vlist-null)
      (%pairs-dict vlist-null)
      (pair-counter 1)
      (default-colors (list
                        (list "default" COLOR_WHITE COLOR_BLACK)
                        (list "inverse" COLOR_BLACK COLOR_WHITE)
                        (list "red" COLOR_RED COLOR_BLACK))))

  (set! get-pair!
    (λ (fg bg)
       "Look up if there are any pairs defined with this particular
       color combination, otherwise just initialize a pair and return
       the new pair."
       (cond ((vhash-assoc (cons fg bg) %pairs-dict)
              => (λ (c) (cdr c)))
             (else (let ((i pair-counter))
                     (init-pair! i fg bg)
                     (set! pair-counter (1+ i))
                     (set! %pairs-dict (vhash-cons (cons fg bg) i %pairs-dict))
                     i)))))

    (set! make-attr!
      (λ (fg bg . attrs)
         (logior (color-pair (get-pair! fg bg)) (apply logior attrs))))

    (set! setup-colors!
      (λ ()
         (if (eq? %pairs-dict vlist-null)
             (begin
               (debug "Setup colors\n")
               (start-color!)
               (debug " Colors available: ~a\n" (colors))
               (debug " Color pairs available: ~a\n" (color-pairs))
               (debug "  - Setup default color-pairs..\n")
               (for-each (λ (i)
                            (get-pair! i 0)
                            (debug "   - Color ~a ~a\n" i (color-content i)))
                         (iota 16 1))
               (debug "  - Setup named color-pairs..\n")
               (map (λ (c) (debug "   - ~a ~a ~a ~a\n" (car c) (cadr c) (caddr c) (cddr c)) (apply set-color! c)) default-colors)))))

    (set! get-color (λ (name)
                       (cond ((vhash-assv (string-hash name) %color-dict)
                              => (λ (c) (cddr c)))
                             (else #f))))

    (set! fold-colors (λ (ƒ init)
                         (vhash-fold ƒ init %color-dict)))

    (set! get-nr-of-pairs (λ () (1- pair-counter)))
    (set! get-nr-of-colors (λ () (vhash-fold (λ (k v r) (1+ r)) 0 %color-dict)))

    (set! set-color! (λ (name fg bg . attrs)
                        (let ((pair (get-pair! fg bg)))
                          (set! %color-dict (vhash-consv (string-hash name)
                                                         (cons name (logior (color-pair pair) (apply logior attrs)))
                                                         (vhash-delete (string-hash name) %color-dict)))))))

(define-syntax-rule (with-attr scr attr e e* ...)
                    (let ((a attr))
                      (attr-on! scr a)
                      e e* ...
                      (attr-off! scr a)))

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
                           ((char=? c #\c)
                            (receive (col r) (span char-numeric? r)
                              (let ((col (locale-string->integer (list->string col))))
                                (parse (if (and (not (null? r)) (char=? (car r) #\;))
                                           (cdr r) r)
                                       modifiers
                                       (if col (cons col color-stack) (cdr color-stack))))))
                           (else (cons-c c r)))))
                  (else (cons-c c r))))))))

(define (draw-test-screen scr)
  (let redraw-loop ((mx (getmaxx scr))
                    (my (getmaxy scr)))
    (erase scr)
    (refresh scr)
    (attr-on! scr A_BOLD)
    (addstr scr "Print color test" #:x 0 #:y 0)
    (attr-off! scr A_BOLD)
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
    (catch 'window-resize
      (λ ()
         (get-char scr))
      (λ (key . handle)
         (redraw-loop (getmaxx scr)
                      (getmaxy scr))))))
