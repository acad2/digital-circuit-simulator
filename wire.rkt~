#lang racket
(require scheme/mpair)

;;;;;;;;This module consists of definition of wire objects;;;;;;;
(define (make-wire)
  (let*([signal-val 0]
        [action-procedures '()])
    (define (set-signal! new-val)
      (if (not (= signal-val new-val))
          (begin
            (set! signal-val new-val)
            (call-each action-procedures))
          'done))
    (define (accept-action-proc! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc));;there has to be an explicit call to newly added proc as if it were not present
             ;;then updatation of the signal will not call this particualr proc as it was not present
             ;;in the action procedure during the setting of the signal.
    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal-val]
            [(eq? m 'set-signal!) set-signal!]
            [(eq? m 'add-action!) accept-action-proc!]
            [else (error "Unknown request ---- WIRE" m)]))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-val)
  ((wire 'set-signal!) new-val))
(define (add-action! wire proc)
  ((wire 'add-action!) proc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;