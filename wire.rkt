;;;;;;;;This file consists of definitions realted to wire;;;;;;;;
(define wire%
  (class object%

    (init-field name)
    (init-field (signal-val 'undefined))
    (init-field (action-procedures '()))

    (super-new)

    (define/public (get-name)
      name)

    (define/public (get-signal)
      signal-val)

    (define/public (set-signal! new-val)
      (cond [(not (eq? signal-val new-val))
             (begin
               (set! signal-val new-val)
               (call-each action-procedures))]))

    (define/public (add-action! proc)
      (set! action-procedures (cons proc action-procedures)))

    (define/public (call-each procedures)
      (if (null? procedures)
          'executed
          (begin
            ((car procedures))
            (call-each (cdr procedures)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;