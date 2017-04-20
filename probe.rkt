;;;;;;;;;;;definition of the probe to monitor the trace;;;;;;;;;;
(define probe%
  (class object%

    (init-field wire)

    (super-new)

    (send wire add-action! (lambda ()
                             (display "time: ") (display (send the-schedule get-time))
                             (display "   ")
                             (display "wire: ") (display (send wire get-name))
                             (display "   ")
                             (display "new-value: ") (display (send wire get-signal))
                             (display "\n")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;