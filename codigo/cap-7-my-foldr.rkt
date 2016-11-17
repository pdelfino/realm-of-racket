#lang racket

(require rackunit racket/trace)

; criar a built-in foldr, mas antes preciso entender o foldr
;entendendo primeiro o foldl, que é parecido com o map,
; mas o map necessariamente retorna numa lista

(check-equal? (foldl cons '() '(1 2 3 4)) '(4 3 2 1))
(check-equal? (foldl + 0 '(1 2 3 4)) 10)
(check-equal? (foldl + 1 '(1 2 3 4)) 11)

(check-equal? (foldl expt 2 '(1 2 3 4)) (expt 4 (expt 3 (expt 2 (expt 1 2)))))

;foldRIGHT
(check-equal? (foldr cons '() '(1 2 3 4)) '(1 2 3 4))

(define (my-foldl proc init lst)
  (define (iter proc lst accu)
    (cond ((empty? lst) accu)
          (else (iter proc (rest lst) (proc (car lst) accu)))))
 ; (trace iter)
  (iter proc lst init))

(check-equal? (my-foldl cons '() '(1 2 3 4)) (foldl cons '() '(1 2 3 4)))
(check-equal? (my-foldl + 0 '(1 2 3 4)) (foldl + 0 '(1 2 3 4)))
(check-equal? (my-foldl + 1 '(1 2 3 4)) (foldl + 1 '(1 2 3 4)))

(define (my-foldr proc init lst)
  (define (iter proc lst accu)
    (cond ((empty? lst) accu)
          (else (iter proc (rest lst) (proc (car lst) accu)))))
  ;(trace iter)
  (iter proc (reverse lst) init))

(check-equal? (my-foldr cons '() '(1 2 3 4)) '(1 2 3 4))
(check-equal? (my-foldr (lambda (v l) (cons (add1 v) l)) '() '(1 2 3 4))
              (foldr (lambda (v l) (cons (add1 v) l)) '() '(1 2 3 4)))

(define (my-foldr-book f base lst)
  (cond [(empty? lst) base]
        [else (f (first lst) (my-foldr-book f base (rest lst)))]))

;(trace my-foldr-book)

(check-equal? (my-foldr-book cons '() '(1 2 3 4)) (my-foldr cons '() '(1 2 3 4)))
(check-equal? (my-foldr-book + 0 '(1 2 3)) (foldr + 0 '(1 2 3)))

(define (my-foldl-book f base lst)
  (cond [(empty? lst) base]
        [else (my-foldl-book f (f (first lst) base) (rest lst))]))

(check-equal? (my-foldl-book cons '() '(1 2 3 4)) (foldl cons '() '(1 2 3 4)))
(check-equal? (my-foldl-book + 0 '(1 2 3 4)) (foldl + 0 '(1 2 3 4)))
(check-equal? (my-foldl-book + 1 '(1 2 3 4)) (foldl + 1 '(1 2 3 4)))

(trace my-foldr-book)
(check-=  (my-foldl-book + 0 '(1 2 3)) (my-foldr-book + 0 '(1 2 3)) 6)
