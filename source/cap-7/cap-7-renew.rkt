#lang racket

(require rackunit racket/trace)

(struct goo (loc expire) #:transparent)

(struct posn (x y) #:transparent)

(define SIZE 30)

(define EXPIRATION-TIME 150)

(define GOOS-EXAMPLE (list (goo (posn 1 2) 1)
                           (goo (posn 3 4) 2)
                           (goo (posn 5 6) 3)
                           (goo (posn 7 8) 4)
                           (goo (posn 9 10) 5)
                           (goo (posn 11 12) 0)))

(define GOOS-EXAMPLE-TWO (list (goo (posn 1 2) 0)
                               (goo (posn 3 4) 1)
                               (goo (posn 5 6) 2)
                               (goo (posn 7 8) 3)
                               (goo (posn 9 10) 4)
                               (goo (posn 11 12) 0)))

;fresh-goo cria um NOVO goo
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

;o que rotten faz?
;To ROT é desaparecer!
;rotten? é podre?
;função checa se já está podre o GOO!

(define (rotten? g)
  (zero? (goo-expire g)))

;decay faz o GOO envelhecer, veja que o loc, a localização é mantida,
;  e a idade é DIMINUÍDA em 1!
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

;a função  renew dá uma renovada nos GOOS com novos e aleatórios fresh-goo,
; além de tirar os que já envelheceram
;definição recursiva de um procedimento recursivo
(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))
;sem eu pedir aqui, rola o trace do que está sendo executado nos TEST CASES
;(trace renew)

; o que o ROT faz? Envelhece os goos meio da função !

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

;o que o age faz?
; primeiro ele renova os GOOS já no ZERO, gerando novo e, DEPOIS, envelhece todos
(define (age-goo goos)
  (rot (renew goos)))

;higher order functions have a procedure as a parameter/argument

;contruindo o my-filter

(define (my-filter pred lst)
  (define (iter lst accu)
    (cond ((null? lst) (reverse accu))
          ((pred (first lst))
           (iter (rest lst) (cons (first lst) accu)))
          (else (iter (rest lst) accu))))
  (iter lst '()))

(define (my-filter-recur pred lst)
  (cond ((null? lst) '())
        ((pred (first lst))
         (cons (first lst) (my-filter-recur pred (rest lst))))
        (else (my-filter-recur pred (rest lst)))))

;como fazer um RENEW GOOS com o MY-FILTER e o LAMBDA

;o lambda ali dentro retorna True or False!

(define (renew-lambda goos)
  (my-filter (lambda (f) (not (rotten? f))) goos))

(define (my-ormap proc  lst)
  (cond ((null? lst) #f)
        ((proc (car lst)) (proc (car lst)))
        (else (my-ormap proc (rest lst)))))

(define (my-ormap-book pred lst)
  (cond [(empty? lst) #f]
        [else (or (pred (first lst))
                  (my-ormap-book pred (rest lst)))]))

;entendendo como funciona o andmap


(define (my-andmap proc lst)
  (cond ((empty? lst) #t)
        ((equal? (proc (first lst)) #f) #f)
        (else (my-andmap proc (rest lst)))))

(define (my-andmap-book pred lst)
  (cond [(empty? lst) #t]
        [else (and (pred (first lst))
                   (my-andmap-book pred (rest lst)))]))


;(trace my-andmap-book)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; TESTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (zero? 0))
(check-false (zero? 1))

(check-false (rotten? (first GOOS-EXAMPLE)))
(check-true (rotten? (last GOOS-EXAMPLE)))

(check-equal? (decay (first GOOS-EXAMPLE)) (goo (posn 1 2) 0))

;teste para o renew
(check-equal? (goo-expire (last (renew GOOS-EXAMPLE))) 150)

(check-equal? (rot GOOS-EXAMPLE) (list
                                  (goo (posn 1 2) 0) ;era 1 virou 0
                                  (goo (posn 3 4) 1)
                                  (goo (posn 5 6) 2)
                                  (goo (posn 7 8) 3)
                                  (goo (posn 9 10) 4)
                                  (goo (posn 11 12) -1)))

(check-equal? (my-filter even? (range 1 40))
              '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38))
(check-equal? (my-filter-recur odd? (range 1 30))
              '(1 3 5 7 9 11 13 15 17 19 21 23 25 27 29))

(check-equal? (renew-lambda GOOS-EXAMPLE)
              (list
               (goo (posn 1 2) 1)
               (goo (posn 3 4) 2)
               (goo (posn 5 6) 3)
               (goo (posn 7 8) 4)
               (goo (posn 9 10) 5)))

(check-equal? (ormap add1 '(3 4 5)) 4)
(check-equal? (ormap add1 '()) #f)
(check-equal? (ormap equal? '(a b c) '(d e f))  #f)
(check-equal? (ormap + '(1 2 3) '(4 5 6)) 5)
(check-equal? (ormap positive? '(-1 -2 3)) #t)
(check-equal? (ormap  + '(1 2 3) '(4 5 6)) 5)

(check-equal? (my-ormap add1 '(3 4 5)) 4)
(check-equal? (my-ormap add1 '()) #f)
(check-equal? (my-ormap positive? '(1 2 a)) #t)
(check-equal? (my-ormap positive? '(-1 -2 3)) #t)
;(check-equal? (my-ormap  + '(1 2 3) '(4 5 6)) 5)

(check-equal? (my-ormap-book add1 '(3 4 5)) 4)
(check-equal? (my-ormap-book add1 '()) #f)
(check-equal? (my-ormap-book positive? '(1 2 a)) #t)
(check-equal? (my-ormap-book positive? '(-1 -2 3)) #t)
;(check-equal? (my-ormap-book  + '(1 2 3) '(4 5 6)) 5)

(check-equal? (andmap positive? '(1 2 3)) #t)
(check-equal? (andmap positive? '(1 -2 a)) #f)

(check-equal? (my-andmap positive? '(1 2 3)) #t)
(check-equal? (my-andmap positive? '(1 -2 a)) #f)

(check-equal? (my-andmap-book positive? '(1 2 3)) #t)
(check-equal? (my-andmap-book positive? '(1 -2 30)) #f)

(check-equal? (ormap number? empty) #f)
(check-equal? (andmap number? empty) #t)
(check-equal? (my-ormap number? empty) #f)
(check-equal? (my-andmap number? empty) #t)

