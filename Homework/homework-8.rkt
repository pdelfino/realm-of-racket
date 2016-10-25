#lang racket


(require racket/trace)

(require rackunit)

(define english-1
  '((Initial (1))
    (Final (11))
    (From 1 to 2 by NP)
    (From 1 to 3 by DET)
    (From 2 to 10 by PONT)
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
    (From 10 to 11 by fim)))
    

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
    (PONT  ! ? )))

(define (recognize network tape)
  ;; returns t if sucessfully recognizes tape - nil otherwise
  (call/cc (lambda (return)
             (define (recognize-next node tape network)
               (if (null? tape)
                   (if (member node (final-nodes network))
                       (return #t)
                       (return '())); success
                   (for ([transition (transitions network)])
                     ;; try each transition of the network
                     (when (equal? node (trans-node transition)) ; if it starts at the right node
                       (for ([newtape (recognize-move (trans-label transition) tape)])
                         ;; try each possible new value of tape
                         (recognize-next (trans-newnode transition) newtape network))))))
             (for ([initialnode (initial-nodes network)])
               (recognize-next initialnode tape network))
             null))) ; failed to recognize

(define (recognize-move label tape)
  (if (or (eq? label (car tape))
          (member (car tape) (or (assoc label abbreviations) '())))
      (list (cdr tape))
      (if (eq? label '|#|)
          (list tape)
          null)))


(check-equal? (recognize english-1 '(kim  ? fim)) #t)

(check-equal? (recognize english-1 '(esse exercício é foda para caralho)) '())

'(kim is a very stupid man)


(define (generate network)
  (define (generate-next node tape network)
    (cond ((member node (final-nodes network))
           (displayln tape))
          (else
           (for ([transition (transitions network)])
             (when (equal? node (trans-node transition))
               (for ([newtape (generate-move (trans-label transition) tape)])
                 (generate-next (trans-newnode transition) newtape network)))))))
  
  (define (generate-move label tape)
    (if (or (eq? '|#| label )
            (eq? 'fim label))
        '()
        (cdr (assoc label abbreviations))))
  
  
  (for ((initialnode (initial-nodes network)))
    (generate-next initialnode null network)))



