#lang racket

(require racket/trace)

(require rackunit)

(define english-1
  '((Initial (1))
    (Final (10))
    (From 1 to 2 by NP)
    (From 1 to 3 by DET)
    (From 2 to 10 by PONT)
    (From 2 to 10 by |#|)
    (From 2 to 5 by BV)
    (From 2 to 7 by CNJ)
    (From 3 to 6 by ADJ)
    (From 3 to 4 by N)
    (From 3 to 8 by MOD)
    (From 4 to 10 by PONT)
    (From 4 to 7 by CNJ)
    (From 5 to 6 by ADJ)
    (From 5 to 3 by DET)
    (From 6 to 9 by ADV)
    (From 5 to 10 by PONT)
    (From 6 to 4 by N)
    (From 6 to 10 by PONT)
    (From 6 to 10 by |#|)
    (From 6 to 8 by MOD)
    (From 7 to 1 by |#|)
    (From 7 to 2 by NP)
    (From 7 to 3 by DET)
    (From 7 to 4 by N)
    (From 8 to 9 by ADV)
    (From 8 to 6 by ADJ)
    (From 9 to 8 by MOD)
    (From 9 to 6 by ADJ)
    (From 9 to 3 by DET)
    (From 10 to 1 by |#|)))


(define (getf x y)
  (if (eq? (car x) y)
      (cadr x)
      (getf (cdr x) y)))

(define (initial-nodes network)
  (list-ref (assoc 'Initial network) 1))

(define (final-nodes network)
  (list-ref  (assoc 'Final network) 1))

(define (transitions network)
  (filter (lambda (x) (eq? (car x) 'From)) network))

(define (trans-node transition)
  (getf transition 'From))

(define(trans-newnode transition)
  (getf transition 'to))

(define (trans-label transition)
  (getf transition 'by))

(define abbreviations
  '((NP kim sandy lee)
    (DET a the her)
    (N consumer man woman)
    (BV is was)
    (CNJ and or)
    (ADJ happy stupid)
    (MOD very)
    (ADV often always sometimes)
    (|#|)
    (fim)
    (PONT  ! ?  )))

(define (recognize network tape)
  ;; retorna t se reconhecer o tape - retorna null se não reconhecer
  ;; quando passamos um tape que não termina em sinal de pontuação, ele retorna '()
  (call/cc (lambda (return)
             (define (recognize-next node tape network)
               (if (null? tape)
                   (if (member node (final-nodes network))
                       (return #t)
                       (return '()))
                        ; successo
                   (for ([transition (transitions network)])
                     ;; testa cada transição da rede
                     (when (equal? node (trans-node transition)) ; if it starts at the right node
                       (for ([newtape (recognize-move (trans-label transition) tape)])
                         ;; testa cada valor possível
                         (recognize-next (trans-newnode transition) newtape network))))))
             (for ([initialnode (initial-nodes network)])
               (recognize-next initialnode tape network))
             null))) ; falha em reconhecer

(define (recognize-move label tape)
  (if (or (eq? label (car tape))
          (member (car tape) (or (assoc label abbreviations) '())))
      (list (cdr tape))
      (if (eq? label '|#|)
          (list tape)
          null)))

(define (generate network size)
  (for ((initialnode (initial-nodes network)))
    (generate-next initialnode null network size)))


(define (generate-next node tape network size)
  (cond ((or (member node (final-nodes network)) (> (length tape) size))
         (displayln tape))
        (else
         (for ([transition (transitions network)])
           (when (equal? node (trans-node transition))
             (for ([newtape (generate-move (trans-label transition) tape)])
               (generate-next (trans-newnode transition) (append tape (list newtape)) network size)))))))

(define (generate-move label tape)
  (if (eq? '|#| label )           
      '()
      (cdr (assoc label abbreviations))))


(check-equal? (recognize english-1 '(kim  ? )) #t)
(check-equal? (recognize english-1 '(kim eu ? )) '())
(check-equal? (recognize english-1 '(pedro  ? )) '())
(check-equal? (recognize english-1 '(kim is happy  )) '())
(check-equal? (recognize english-1 '(esse exercício é foda para caralho)) '())

(recognize english-1 '(kim was happy  ))
(generate english-1 3)
(generate english-1 2)
(generate english-1 1)
(generate english-1 0)
