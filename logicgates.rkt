;;;;;;;;;;;;;;;;;;basic logics and gates;;;;;;;;;;;;;;;;;;;;;;;;;
;;some logics
(define (logical-not val)
  (cond ((eq? val 1) 0)
        ((eq? val 'udefined) 'undefined)
        (else 1)))
(define (logical-or val1 val2)
  (cond [(or (eq? val1 1) (eq? val2 1)) 1]
        [(or (eq? val1 'undefined) (eq? val2 'undefined)) 'undefined]
        [else 0]))
(define (logical-and val1 val2)
  (logical-not (logical-or (logical-not val1) (logical-not val2))))

;;definition of a gate
(define inverter%
  (class object%

    (init-field inp)
    (init-field out)

    (define inverter-delay 1)

    (super-new) ;done before the function definition

    (define (invert)
      (let((new-val (logical-not (send inp get-signal)))) ;send is used to acess the functions of objects of another class
        (add-to-queue-after! inverter-delay
                            (lambda ()
                              (send out set-signal! new-val)))))
    (send inp add-action! invert)
    'ok))

(define or-gate%
  (class object%

    (init-field inp1)
    (init-field inp2)
    (init-field out)

    (define or-delay 2)
    
    (super-new)

    (define (take-or)
      (let((new-val (logical-or (send inp1 get-signal) (send inp2 get-signal))))
        (add-to-queue-after! or-delay  (lambda ()
                                         (send out set-signal! new-val)))))
    (send inp1 add-action! take-or)
    (send inp2 add-action! take-or)))

(define and-gate%
  (class object%

    (init-field inp1)
    (init-field inp2)
    (init-field out)

    (define and-delay 2)

    (super-new)

    (define (take-and)
      (let((new-val (logical-and (send inp1 get-signal) (send inp2 get-signal))))
        (add-to-queue-after! and-delay (lambda ()
                                        (send out set-signal! new-val)))))
    (send inp1 add-action! take-and)
    (send inp2 add-action! take-and)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;