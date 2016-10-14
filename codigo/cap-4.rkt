#lang racket

(require racket/trace)

;definição recursiva de um procedimento recursivo para contar o tamanho de uma lista
(define (len-recur lista)
  (if (empty? lista)
      0
      (add1 (len-recur (cdr lista)))))

(define listinha (list 1 2 1))

(trace len-recur)

;(len-recur listinha)

(define (len-iter lista)
  (define (iter lista count)
    (if (empty? lista)
        count
        (iter (cdr lista) (add1 count))))
  (trace iter)
  (iter lista 0))

(trace len-iter)

;(len-iter listinha)

;;;;;;;;;;;;;;;;;;;;;;;

(struct point (x y) #:transparent)

(define (square n)
  (* n n))

(define (distance-to-origin ponto)
  (sqrt (+ (square (point-x ponto))
           (square (point-y ponto)))))

(define A (point 3 4))

;(point-x A)
;(point-y A)

;(distance-to-origin A)

;A
;point-x

(define pt1 (point -1 2))

(define pt2 (point -1 2))

;(equal? pt1 pt2)

;(eq? pt1 pt2)

(require rackunit)

; teste bem sucedido (check-equal? (add1 4) 5)
(check-equal? (add1 4) 7)