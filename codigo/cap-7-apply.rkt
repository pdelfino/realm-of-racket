#lang racket

(require rackunit racket/trace)

(check-equal? (apply + '(1 2 3)) 6)

(check-equal? (apply + 1 2 '(3)) 6)

(define (highest-num lst)
  (define (iter lst accu)
    (cond ((null? lst) accu)
          ((< accu (first lst)) (iter (rest lst) (+ accu (- (first lst) accu))))
          (else (iter (rest lst) accu))))
  ;(trace iter)
  (iter lst 0))

(check-equal? (highest-num '(58 64 77 77 22 94 93 78)) 94)