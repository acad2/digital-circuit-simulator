#lang racket
(require scheme/mpair)

;;First we need to creaate some primitive function boxes
;;which later can be combined to create complex circuits


;;basic logical operations whichwill be useful later on
(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "Invalid Input")]))
(define (logical-and s1 s2)
  (cond [(and (= s1 1) (= s2 1)) 1]
        [(and (= s1 1) (= s2 0)) 0]
        [(and (= s1 0) (= s2 1)) 0]
        [(and (= s1 0) (= s2 0)) 0]
        [else (error "Invalid Input")]))
(define (logical-or s1 s2)
  ;we can define in terms of not and and
  (let*([i1 (logical-not s1)]
        [i2 (logical-not s2)]
        [it (logical-and i1 i2)]
        [o (logical-not it)])
    o))
        

;;box for an Inverter
(define (inverter input output)
  (define (invert-input)
    (let([new-sig-val (logical-not (get-signal input))])
      (after-delay inverter_delay
                   (lambda ()
                     (set-signal! output new-sig-val)))))
  (add-action! input invert-input)
  'ok)

  
    
      
      
         
  
