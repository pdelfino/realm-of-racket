#lang racket

(require rackunit racket/trace)

;(check-equal? (for ((i '(1 2 3 4 5))) (display i)) (display 12345))

(check-equal? (for/fold ([sqrs 0])
                        ([i '(1 2 3 4 5 6 7 8 9 10)])
                (+ (sqr i) sqrs)) 385)

(define (my-for/fold-square num)
  (if (= num 0)
      (sqr 0)
      (+ (sqr num) (my-for/fold-square (sub1 num)))))

(check-equal? (my-for/fold-square 10) 385)

(check-equal? (foldl (lambda (i sqrs) (+ (sqr i) sqrs)) 0 '(1 2 3 4 5 6 7 8 9 10)) 385)

;(check-equal? (values 'this 'and-this 'and-that) 'this 'and-this 'and-that)

(define-values (var-a var-b var-c) (values 1 2 3))

(check-equal? var-a 1)

(check-equal? var-b 2)

(check-equal? var-c 3)

#|(define-values (x y)
  (if (string=? (today) "tuesday")
      (values 10 20)
      (values 42 55)))|#

(check-equal?(for/list ((i '(1 2 3 4 5))
                        #:when (odd? i)) i) '(1 3 5))

(check-equal?(for/fold ((sum 0))
                       ((i '(1 2 3 4 5))
                        #:when (even? i)) (+ sum i)) 6)

; #:when  , hashtag, dois pontos e when serve como o FILTER

(check-equal? (for/list ([ i '(1 2 3 4 5)]
                         [ j '(1 2 3 4)]
                         [ k '(5 4 3 2 1)])
                (list i j k)) '((1 1 5) (2 2 4) (3 3 3) (4 4 2)))

(check-equal? (for/list ([i '(1 2 3 4 5)]
                         [s '("a" "b" "c" "d" "e")]
                         #:when (and (even? i) (string=? s "d")))
                i) '(4))

#|(for (( i '(1 2 3)))
      (for ((j '(1 2 3)))
        (for ((k '(1 2 3)))
          (displayln (list i j k)))))|#

#|(for* ((i '(1 2 3))
       (j '(1 2 3))
       (k '(1 2 3)))
       (displayln (list i j k)))|#

(check-equal? (for*/list ((i '(1 2 3))
                          (j '(4 5 6)))
                (+ i j)) '(5 6 7 6 7 8 7 8 9))

