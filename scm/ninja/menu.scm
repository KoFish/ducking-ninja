(define-module (ninja menu)
               #:export (<ninja-menu>
                          <ninja-menu-item>
                          make-ninja-menu
                          make-ninja-multi-menu
                          make-ninja-menu-item
                          ninja-menu-item?
                          default-menu-color-theme
                          get-title
                          multiselect?
                          get-menu
                          get-name
                          get-short
                          get-action
                          is-allowed?
                          set-allowed!
                          add-item!
                          add-blank!
                          draw-menu
                          do-menu))

(use-modules (oop goops)
             (ncurses curses)
             (srfi srfi-9) ; define-record-type
             (ice-9 optargs))

(use-modules (util reader)
             (util debug)
             (util colors))

(define-class <ninja-menu> ()
  (title #:getter get-title #:init-keyword #:title)
  (items #:accessor items #:init-value '())
  (colors #:getter get-color-theme #:init-keyword #:colors)
  (prompt #:getter get-prompt #:setter set-prompt! #:init-value "Choice: "))

(define (ninja-menu? obj) (is-a? obj <ninja-menu>))

(define-class <ninja-ms-menu> (<ninja-menu>)
  (selected #:accessor selected #:init-value '()))

(define (ninja-ms-menu? obj) (is-a? obj <ninja-ms-menu>))

(define (make-ninja-menu title colors) 
  (make <ninja-menu> #:title title #:colors colors))
(define (make-ninja-ms-menu title colors) 
  (make <ninja-ms-menu> #:title title #:colors colors))

(define-record-type <ninja-menu-item>
  (make-ninja-menu-item menu name short action allowed)
  ninja-menu-item?
  (menu get-menu)
  (name get-name)
  (short get-short)
  (action get-action)
  (allowed is-allowed? set-allowed!))

(define-record-type <ninja-menu-color-theme>
  (make-ninja-menu-color-theme fg bg title prompt option-enabled option-disabled short short-bracket)
  ninja-menu-color-theme?
  (fg get-foreground-color)
  (bg get-background-color)
  (title get-title-color)
  (prompt get-prompt-color)
  (option-enabled get-option-enabled-color)
  (option-disabled get-option-disabled-color)
  (short get-short-color)
  (short-bracket get-bracket-color))

(define (default-menu-color-theme)
  (let ((white 7) 
        (bold-white 15) 
        (gray  8) 
        (black 0)) 
    (make-ninja-menu-color-theme white      #| fg       |#
                                 black      #| bg       |#
                                 bold-white #| title    |#
                                 bold-white #| prompt   |#
                                 white      #| enabled  |#
                                 gray       #| disabled |#
                                 bold-white #| short    |#
                                 gray       #| brackets |#
                                 ))) 

(define (get-option-color theme enabled)
  (if enabled 
      (get-option-enabled-color theme)
      (get-option-disabled-color theme)))

(define-method (add-item! (menu <ninja-menu>) name short action allowed)
  (let ((item (make-ninja-menu-item 
                menu name short (or action (cond (name (string->symbol name))
                                                                     (else #f))) allowed)))
    (set! (items menu) (append (items menu) (cons item '())))))

(define-method (add-blank! (menu <ninja-menu>))
  (add-item! menu #f #f #f #f))

(define-method (draw-menu-generic (menu <ninja-menu>) (draw-item <procedure>) scr)
  (let ((mx (getmaxx scr))
        (my (1- (getmaxy scr)))
        (theme (or (get-color-theme menu) (default-menu-color-theme))))
    (clear scr)
    (refresh scr)
    (with-attr scr (make-attr! (get-title-color theme) (get-background-color theme) A_BOLD)
               (addstr scr (get-title menu) #:x 0 #:y 0)) 
    (let draw-item-loop ((items-left (items menu)) (y 1))
      (if (and (not (null? items-left)) (> (- my 2) y))
          (let ((item (car items-left))
                (rest (cdr items-left)))
            (if (get-short item) (draw-item item y 0))
            (draw-item-loop rest (1+ y)))
          y))))

(define-method (draw-menu (menu <ninja-menu>) scr)
  (let ((theme (get-color-theme menu)))
    (let ((bracket (get-bracket-color theme))
          (short (get-short-color theme))
          (enabled (get-option-enabled-color theme))
          (disabled (get-option-disabled-color theme)))
      (let ((draw-item 
              (λ (item y x)
                 (addchstr scr (color-string 
                                 " %c~a[%c~a;~a%c~a]%c %c~a;~a"
                                 bracket
                                 short
                                 (get-short item)
                                 bracket
                                 (if (is-allowed? item) enabled disabled) 
                                 (get-name item)) 
                           #:y y #:x x))))
        (draw-menu-generic menu draw-item scr)))))

(define-method (draw-menu (menu <ninja-ms-menu>) scr)
  (let ((draw-item 
          (λ (item x y)
             (addstr scr 
                     (format #f " [~a] ~:[+~;-~]~:[*~: ~]~a" 
                             (get-short item) 
                             (memq (get-short item) (selected menu))
                             (is-allowed? item)
                             (get-name item)
                             'stuff) #:y y #:x x))))
    (draw-menu-generic menu draw-item scr)))

(define-method (do-menu (menu <ninja-menu>) pscr h w y x)
  (let ((scr (derwin pscr h w y x))
        (theme (get-color-theme menu)))
    (let* ((ey (draw-menu menu scr))
           (ppx 0) (ppy (1+ ey))
           (pcx (+ 0 (string-length (get-prompt menu)))) (pcy (1+ ey))
           (flash (λ (timeout s)
                     (addstr scr s #:y (1+ ppy) #:x ppx)
                     (add-async timeout 
                                (λ () 
                                   (hline scr (normal #\Space) (string-length s) 
                                               #:y (1+ ppy) 
                                               #:x ppx)
                                   (move scr pcy pcx) 
                                   (addchstr scr (normal "   ")) #| This is based on bad heuristics |#  
                                   #f #| Don't repeat this function |#)))))
      (with-attr scr (make-attr! (get-prompt-color theme) (get-background-color theme))
                 (addstr scr (get-prompt menu) #:y ppy #:x ppx))
      (let get-char-loop ((ch (get-char scr)))
        (addstr scr (format #f "~a" ch) #:y pcy #:x pcx)
        (let ((items (filter (λ (item) (eq? ch (get-short item))) (items menu))))
          (cond ((null? items)
                 (flash 400 "That is not an option")
                 (get-char-loop (get-char scr)))
                ((false-if-exception (car items))
                 => (λ (item)
                       (if (is-allowed? item)
                           (begin
                             (clear scr)
                             (delwin scr)
                             (touchwin pscr)
                             (refresh pscr)
                             (get-action item))       
                           (begin 
                             (flash 400 "This option is disabled")
                             (get-char-loop (get-char scr))))))))))))
