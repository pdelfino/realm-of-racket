#lang racket

;função recebe uma lista e retorna true (além do restante da lista)
;se o elemento estiver nessa lista! Se não tiver na lista, retorna #f

(require racket/trace)

(define (member-mine lista num)
  (cond ((empty? lista) #f)
        ((equal? (car lista) num) (cdr lista))       
        (else (member-mine (cdr lista) num))))

(define small-list (list  1 2 3 4 5 6 7 8))

(trace member-mine)

(member-mine '(() 1 2 3 4 5 6 7 8) 1)

(member-mine '(() 1 2 3 4 5 6 7 8) 8)

(member-mine '(() 1 2 3 4 5 6 7 8) 9)
