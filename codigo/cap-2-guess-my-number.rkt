#lang racket

(define lower 1)

(define upper 100)

(define (computer-guess)
  (round (/ (+ lower upper) 2)))

(define (smaller)
  (set! upper (max lower (sub1 (computer-guess))))
  (computer-guess))

(define (bigger)
  (set! lower (min upper (add1 (computer-guess))))
  (computer-guess))

(define (start n m)
  (set! lower (min n m))
  (set! upper (max n m)))

;trabalhando querendo o 12, deu certo, bateu nos dois casos

; quero o 13!

