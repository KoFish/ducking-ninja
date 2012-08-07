(define-module (util colors)
               #:export (setup-colors!
                         get-color
                         color-string
                         with-color))

(use-modules (ice-9 vlist)
             (ice-9 match)
             (srfi srfi-1)
             (ice-9 i18n)
             (ncurses curses))

(use-modules (util debug))

(define setup-colors! #f)
(define get-color #f)

(define-syntax-rule (with-color scr name e e* ...)
  (cond ((get-color name) 
         => (λ (col)
               (attr-on! scr col)
               e
               e* ...
               (attr-off! scr col)))
        (else (debug "Could not set color\n") e e* ...)))

(define (color-string s)
  (let parse ((s (string->list s))
              (bld #f) (col #f) (bli #f) (inv #f) (und #f))
    (if (not (null? s))
        (let ((c (car s))
              (r (cdr s)))
          (match c
            ((? (λ (x) (char=? x #\etx #| Ctrl-C |#)))
             (cond ((char-numeric? (car r))
                    (let* ((sp (span char-numeric? r))
                           (nr (locale-string->integer
                                 (list->string sp)))
                           (r (drop r (length sp))))
                      (parse (if (char=? (car r) #\\) (cdr r) r)
                             bld nr bli inv und)))
                   (else (parse r bld #f bli inv und))))
            ((? (λ (x) (char=? x #\stx #| Ctrl-B |#)))
             (parse r (not bld) col bli inv und))
            ((? (λ (x) (char=? x #\can #| Ctrl-X |#)))
             (parse r bld col bli (not inv) und))
            ((? (λ (x) (char=? x #\nak #| Ctrl-U |#)))
             (parse r bld col bli inv (not und)))
            (_ (let ((ch c))
                 (if bld (set! ch (bold-on ch)))
                 (if col (set! ch (color col ch)))
                 (if bli (set! ch (blink-on ch)))
                 (if inv (set! ch (inverse-on ch)))
                 (if und (set! ch (underline-on ch)))
                 (cons (if (char? ch) (normal ch) ch)
                       (parse r bld col bli inv und))))))
        '())))

(let ((%color-vlist vlist-null)
      (color-table (list
                     (list "menu title" COLOR_YELLOW COLOR_BLACK A_BOLD)
                     (list "menu option" COLOR_WHITE COLOR_BLACK)
                     (list "menu disabled" 8 COLOR_BLACK))))
  (set! setup-colors! 
    (λ ()
       (start-color!)
       (for-each (λ (i) (init-pair! i i 0)) 
                 (iota 16 1))
       (set! %color-vlist 
         (alist->vhash
           (map 
             (λ (i c)
                (let ((name (car c))
                      (fg (cadr c))
                      (bg (caddr c))
                      (attrs (cddr c)))  
                  (init-pair! i fg bg)
                  `(,(string-hash name) . ,(apply logior (append attrs (list (color-pair i)))))))
             (iota (length color-table) (- (color-pairs) (length color-table)))
             color-table) hashv))))

  (set! get-color (λ (name) 
                     (cond ((vhash-assv (string-hash name) %color-vlist)
                            => (λ (c) (cdr c)))
                           (else #f)))))
