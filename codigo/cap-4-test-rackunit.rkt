#lang racket

(require rackunit)

;teste passa, por ser correto o output e a expectativa
(check-equal? (add1 6) 7)

;teste não passa, desalinhamento entre output e expectativa
(check-equal? (add1 6) 8)

; Os autores falam que todos os códigos que eles escrevem passa, antes, por testes.
;Caberia falar disso em alguma aula?