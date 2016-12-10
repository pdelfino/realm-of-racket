#lang racket

(require rackunit racket/trace)

(define (len-recur lista)
  (if (empty? lista)
      0
      (add1 (len-recur (cdr lista)))))

(define (len-iter lista)
  (define (iter lista accu)
    (if (empty? lista)
        accu
        (iter (cdr lista) (add1 accu))))
  ;(trace iter)
  (iter lista 0))

(check-equal? (len-iter (list 1 2 3 4 5))
              (len-recur (list 1 2 3 4 5))
              (length (list 1 2 3 4 5)))
