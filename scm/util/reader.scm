(define-module (util reader)
               #:export (
                         get-char
                         add-async
                         drop-async
                         tick-async))

(use-modules (ncurses curses)
             (oop goops)
             (srfi srfi-1) ; partition
             (srfi srfi-9) ; define-record-type
             (ice-9 match)
             (ice-9 receive))

(use-modules (util debug))

(define-record-type <async-thunk>
  (make-async-thunk timeout thunk)
  async-thunk?
  (timeout get-timeout)
  (thunk get-thunk))

(define %async-thunks (make-fluid '()))

(define (add-async timeout thunk)
  (let ((thunks (fluid-ref %async-thunks))
        (sym (gensym "async"))
        (time (quotient (tms:clock (times)) 1000000)))
    (fluid-set! %async-thunks (cons (cons (cons sym (+ time timeout)) (make-async-thunk timeout thunk)) thunks))
    sym))

(define (drop-async sym)
  (let ((thunks (fluid-ref %async-thunks)))
    (fluid-set! %async-thunks (filter (λ (x) (not (eq? sym (caar x)))) thunks))))


(define (tick-async)
  (let ((thunks (fluid-ref %async-thunks))
        (time (quotient (tms:clock (times)) 1000000)))
    (receive (thunks expired) (partition (λ (x) (< time (cdar x))) thunks)
      (let ((p? (λ (x) (or x #f)))
            (run-async (λ (async)
                          (let ((sym (caar async))
                                (async-thunk (cdr async)))
                            (catch #t 
                              (λ ()
                                 (if (apply (get-thunk async-thunk) '())
                                     (cons (cons sym (+ time (get-timeout async-thunk))) async-thunk) 
                                     #f))
                              (λ (key . args)
                                 (debug "Couldn't run async handler ~a\n" sym)
                                 #f))))))
        (fluid-set! %async-thunks (append thunks (filter p? (map run-async expired)))))))) 

(define (get-char-timeout scr timeout)
  (timeout! scr timeout)
  (let ((ch (getch scr)))
    (timeout! scr -1)
    ch))

(define (throttel-key scr key timeout)
  (let throttel-loop ((ch (get-char-timeout scr timeout)))
    (match ch
      (#f 
       (tick-async)
       key)
      ((? (λ (x) (eq? x key))) 
       (throttel-loop (get-char-timeout scr timeout)))
      (_ (ungetch ch)))))


(define (get-char scr . ƒ) 
  (let ((timeout (if (> (length ƒ) 1) (cadr ƒ) 300))
        (ƒ (if (null? ƒ) #f (car ƒ))))
    (timeout! scr timeout)
    (let getch-loop ((ch (get-char-timeout scr timeout))
                     (idle-value #f))
      (match ch
        ((? char?) 
         (tick-async)
         ch)
        (#f
         (tick-async)
         (getch-loop (get-char-timeout scr timeout) (if ƒ (ƒ idle-value))))
        ((? (λ (k) (eq? k KEY_RESIZE)))
         (throttel-key scr ch 300)
         (throw 'window-resize))
        (_ (throw 'unknown-key (format #f "Unknown key pressed [~a]" ch)))))))
