(require racket/mpair)
;;;;;;;;;;;;scheduler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scheduler%
  (class object%

    (init-field (current-time 0))
    (init-field (last-event-time 0))
    (init-field (schedule (make-vector 200 '())))

    (super-new)

    (define/public (add-to-schedule! time proc)
      (begin
        (vector-set! schedule time (mappend! (vector-ref schedule time) (mlist proc)))
        (cond ((> time last-event-time) (set! last-event-time time)))))
    (define/public (serviced?)
      (> current-time last-event-time))
    (define/public (increment-time!)
      (set! current-time (+ 1 current-time)))
    (define/public (next-item)
      (vector-ref schedule current-time))
    (define/public (get-time)
      current-time)))

;;;;shcedule handling functions;;;;;;;;;
(define (add-to-queue-after! time proc)
  (send the-schedule add-to-schedule! (+ time (send the-schedule get-time)) proc))
(define (execute! action-list)
  (cond ((not (null? action-list))
         (begin
           ((mcar action-list))
           (execute! (mcdr action-list))))))
(define (propagate schedule)
  (cond [(send the-schedule serviced?) 'done]
        [(let ([action-list (send the-schedule next-item)])
           (begin
             (execute! action-list)
             (send the-schedule increment-time!)
             (propagate schedule)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-schedule (make-object scheduler%))