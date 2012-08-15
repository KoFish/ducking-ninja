(define-module (util debug)
               #:export (debug))

(define (debug str . rest)
  (apply format (current-error-port) str rest)
  (force-output (current-error-port)))
