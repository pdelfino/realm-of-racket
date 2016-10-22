#lang racket
(define english-1
  '((Initial (1))
    (Final (9))
    (From 1 to 3 by NP)
    (From 1 to 2 by DET)
    (From 2 to 3 by N)
    (From 3 to 4 by BV)
    (From 4 to 5 by ADV)
    (From 4 to 5 by |#|)
    (From 5 to 6 by DET)
    (From 5 to 7 by DET)
    (From 5 to 8 by |#|)
    (From 6 to 7 by ADJ)    
    (From 6 to 6 by MOD)
    (From 7 to 9 by N)
    (From 8 to 8 by MOD)
    (From 8 to 9 by ADJ)
    (From 9 to 4 by CNJ)
    (From 9 to 1 by CNJ)))

(define (initial-nodes network)
  ( list-ref (assoc 'Initial network) 1))

(define (final-nodes network)
  (list-ref  (assoc 'Final network) 1))

(define (transitions network)
  (filter (lambda (x) (eq? (car x) 'From) network)))

(define (trans-node transition)
  (cdr (assoc 'From transition)))

(define(trans-newnode transition)
  (cdr (assoc 'to transition)))

(define (trans-label transition)
  (cdr (assoc 'by transition)))

(define abbreviations
  '((NP kim sandy lee)
    (DET a the her)
    (N consumer man woman)
    (BV is was)
    (CNJ and or)
    (ADJ happy stupid)
    (MOD very)
    (ADV often always sometimes)))

(define-struct (my-exception exn:fail:user) ())

#|Temos que criar uma exceção para esse caso ou vcs acham melhor usar alguma já built-inw

(define (recognize network tape)
  (with-handlers ((exn:fail)
    (dolist (initialnode (initial-nodes network))
     (recognize-next initialnode tape network))
   nil)))


Estou tendo problemas no "for" da função abixo. Sugestões? 
|#


(define (recognize-next node tape network)
  (if (and (null tape) (member node final-nodes network))
      (raise 'stop #t)
      (for ((transition (transitions network)))
        (if (eq? node (trans-node transition))
            (for ((newtape (recognize-move (trans-label transition) tape))))
            (recognize-next (trans-newnode transition) newtape network)))))




