#lang racket

;Definição de Lazy Evaluation a partir de um lambda.
(define lazy+ (lambda () (apply + (build-list 50000 values))))

;Definições mais complexas de Lazy Evaluation
(define (make-lazy+ i)
  (lambda () (apply + (build-list (* 500 i) values))))

(define long-big-list (build-list 5000 make-lazy+))

(define (compute-every-1000th l)
  (for/list ([thunk l][i (in-naturals)]
                      #:when (zero? (remainder i 1000)))
    (thunk)))
;========================================================

;Lazy Evaluation + Memoized Computations (Memorização)

(define (memoize suspended-c)
  (define hidden #f)
  (define run? #f)
  (lambda ()
    (cond [run? hidden]
          [else (set! hidden (suspended-c))
                (set! run? #t)
                hidden])))

(define mlazy+ (memoize lazy+))

;Outra versão de memoize proposta pelo livro
(define (memoize.v2 suspended-c)
  (define (hidden)
    (define the-value (suspended-c))
    (set! hidden (lambda () the-value))
    the-value)
  (lambda () (hidden)))

;==============================================
; Built-ins de Lazy Evaluation :: Delay & Force

(define lazy+2 (delay (apply + (build-list 50000 values))))
;no caso acima a chamada deverá ser (force lazy+2), uma vez que chamar apenas
;uma vez que chamando apenas a variável o procedimento será retardado até ser necessário.

; Delay -> cria uma "promise", que é uma lazy evaluation cujo resultado será memorizado.

