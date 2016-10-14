#lang racket

;lower e upper vem de upper-boundary e lower-boundary
(define (guess lower upper)
  (round (/ (+ lower upper) 2)))

(require racket/trace)

(define (game-iter number)
  (define (iter number count lower upper guess-status)
    (let ((iterative-guess (guess lower upper)))
    (cond ((= iterative-guess  number) (values (add1 count) iterative-guess))
          ((> iterative-guess  number)
           (iter  number
                  (add1 count)
                  lower
                  (sub1 iterative-guess)
                  iterative-guess))
          (else (iter number
                      (add1 count)
                      (add1 iterative-guess)
                      upper
                      iterative-guess)))))
  (trace iter)
  (if (< 0 number 101)
      (iter number 0 1 100 guess)
      ;; criei esse código apenas para números de 1 a 100
      #f))

(game-iter 12)
