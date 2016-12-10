#lang racket

(require rackunit racket/trace)

(define lista (list 'beef 'chicken 'pork))

(struct student (name id# dorm))

(define calouro1 (student 'pedro 132300906 '1201))

;(student-name calouro1)
;(student-id# calouro1)
;(student-dorm calouro1)

;usar student-name é uma ACCESSSOR FUNCTION

(define mimi (student 'Mimi 1234 'NewHall))
(define nicole (student 'Nicole 5678 'NewHall))
(define rose (student 'Rose 8765 'NewHall))
(define eric (student 'Eric 4321 'NewHall))
(define in-class (list mimi nicole rose eric))

;(student-name (third in-class))
;in-class
;(car (cdr in-class))
;(student-id# (third in-class))

;criando uma nova estrutura, via struct, com uma instância e 4 fields

(struct student-body (freshman sophomores junior seniors))
(define all-students
  (student-body (list calouro1 (student 'Marry 0101 'OldHall)) ;aqui tem dois alunos como freshman, calouro1 e a mary
                (list (student 'Jeff 5678 'OldHall))
                (list (student 'Bob 4231 'Apartment))
                empty))

;(student-name (second (student-body-freshman all-students)))

;(student-name (first (student-body-junior all-students)))

;;all-students
;calouro1

(struct example (x y z))
(define ex1 (example 1 2 3))
(example-x ex1)

;OOOOOOOR

(struct example2 (p q r) #:transparent)
(define ex2 (example2 10 12 13))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; TESTS ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (string-append "pedro " "delfino") "pedro delfino")

(check-equal? (list) '())

(check-equal? (cons 'chicken '()) '(chicken)) 

(check-equal? lista '(beef chicken pork))

(check-equal? (first lista) 'beef)

(check-equal? (rest lista) '(chicken pork))

(check-equal? (first (rest lista)) (car (rest lista)))

(check-equal? (car (rest (rest lista))) (last lista))

(check-equal? (student-name calouro1) 'pedro)
