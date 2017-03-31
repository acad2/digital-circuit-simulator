;;;;;;;;;;;;scheduler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-scheduler)
  (let*([new-schedule (mlist (mcons 0 'empty))])
    new-schedule))

;;;;shcedule handling functions;;;;;;;;;
(define (empty? schedule)
  (if (= (mlength schedule) 1)
      (if (eq? (mcdr (mcar schedule)) 'empty)
          #t
          #f)
      #f))
(define (first-schedule-item schedule)
  (mcar schedule))
(define (remove-first-schedule-item schedule)
  (begin;;if there is only one process before the removal in scheduler then like begining
    (if (null? (mcdr schedule))
        (set! schedule (mlist (mcons 0 'empty)))
        (set! schedule (mcdr schedule)))
    schedule))
(define (add-to-schedule t proc schedule)
  (let*([new-pair (mlist (mcons t proc))]
        [schedule (append! schedule new-pair)])
    schedule))
(define (append! list1 list2)
  ;;walk the first list till the end and join list2
  (define (walk l)
    (if (null? (mcdr l))
        (begin
          (set-mcdr! l list2)
          list1)
        (walk (mcdr l))))
  (walk list1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;