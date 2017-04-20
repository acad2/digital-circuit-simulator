#lang racket
(include "scheduler.rkt")
(include "logicgates.rkt")
(include "wire.rkt")
(include "probe.rkt")

;;the objects
(define a (make-object wire% "a"))
(define b (make-object wire% "b"))
(define out (make-object wire% "o"))

(make-object inverter% a b)
(make-object or-gate% a b out)

(make-object probe% a)
(make-object probe% b)
(make-object probe% out)

;(send the-schedule add-to-schedule! 1 (lambda () (send a set-signal! 0)))
(send the-schedule add-to-schedule! 1 (lambda () (send b set-signal! 1)))
(propagate the-schedule)