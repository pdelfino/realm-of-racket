#lang racket

(require rackunit racket/trace)

(check-equal? (apply + '(1 2 3)) 6)

(check-equal? (apply + 1 2 '(3)) 6)

(define (highest-num-recur lst)
  (define (iter lst accu)
    (cond ((null? lst) accu)
          ((< accu (first lst)) (iter (rest lst) (+ accu (- (first lst) accu))))
          (else (iter (rest lst) accu))))
  ;(trace iter)
  (iter lst 0))

(define (highest lon) (apply max lon))

(check-equal? (highest-num-recur '(58 64 77 77 22 94 93 78))
              (highest '(58 64 77 77 22 94 93 78)))
