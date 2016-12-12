#lang racket

(require 2htdp/universe)
(require rackunit rackunit/text-ui)

(struct ttt (board moves))

(struct action (player position))

;(define (generate-ttt-tree player1 player2)
;  (define (generate-tree board player opponent)
;    (ttt board (generate-moves board player opponent)))
;  (define (generate-moves board0 player opponent)
;    (define free-fields (board-find-free-fields board0))
;    (for/list ((f free-fields))
;      
;      (define actnow (action player f))
;      
;      (define board1 (board-take-field board0 player f))
;      
;      (list actnow (generate-tree board1 opponent player))))
  ;; -- start here --
;  (generate-tree the-empty-board player1 player2))



(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)



(struct dice-world (src board gt))

(struct territory (index player dice x y))

(struct game (board player moves))

(struct move (action gt))

(define b0 (list (territory 0 0 2 'x 'y) (territory 1 1 1 'a 'b)))
(define b1 (list (territory 0 0 1 'x 'y) (territory 1 0 1 'a 'b)))
(define b2 (list (territory 1 0 1 'a 'b) (territory 0 0 1 'x 'y)))

(define gt2 (game b2 1 '()))

(define mv1 (move '() gt2))
(define gt1 (game b2 0 (list mv1)))

(define mv0 (move '(0 1) gt1))
(define gt0 (game b0 0 (list mv0)))

(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            (on-key interact-with-board)
            (on-draw draw-dice-world)
            (stop-when no-more-moves-in-world?
                       draw-end-of-dice-world)))

(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world)
      (create-world-of-dice-and-doom)
      new-world))

(define (no-more-moves-in-world? w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define player (game-player tree))
  (or (no-more-moves? tree)
      (for/and ((t board)) (= (territory-player t) player))))

(define (no-more-moves? x)
  (empty? (game-moves x)))

(define (draw-end-of-dice-world w)
  (define board (dice-world-board w))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene w (PLAIN)))
  (overlay message background))

(define (draw-dice-world w)
  (add-player-info
   (game-player (dice-world-gt w))
   (add-board-to-scene w (ISCENE))))

(define (interact-with-board w k)
  (cond [(key=? "left" k)
         (refocus-board w left)]
        [(key=? "right" k)
         (refocus-board w right)]
        [(key=? "p" k)
         (pass w)]
        [(key=? "\r" k)
         (mark w)]
        [(key=? "d" k)
         (unmark w)]
        [else w]))

(define (add-player-info player s)
  (define str (whose-turn player))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET s))

(define (add-board-to-scene w s)
  (define board
    (dice-world-board w))
  (define player (game-player (dice-world-gt w)))
  (define focus? (dice-world-src w))
  (define trtry1 (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s (add-territory trtry1 image s))
  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))
(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))

(define (add-territory t image scene)
  (place-image image (territory-x t) (territory-y t) scene))
(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (hexagon color) (draw-dice (territory-dice t))))

(define (color-chooser n)
  (list-ref COLORS n))

(define (draw-dice n)
  (define first-dice (get-dice-image 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice]) ([i (- n 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset (* height-dice (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

(define (get-dice-img i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))


(define (refocus-board w direction)
  (define source (dice-world-src w))
  (define board (dice-world-board w))
  (define tree
    (dice-world-gt w))
  (define player (game-player tree))
  (define (owner? tid)
    (if source (not (= tid player)) (= tid player)))
  (define new-board (rotate-until owner? board direction))
  (dice-world source new-board tree))

(define (rotate-until owned-by board rotate)
  (define next-list (rotate board))
  (if (owned-by (territory-player (first next-list)))
      next-list
      (rotate-until owned-by next-list rotate)))

(define (left l)
  (append (rest l) (list (first l))))

(define (right l)
  (reverse (left (reverse l))))

(define (pass w)
  (define m (find-move (game-moves (dice-world-gt w)) '()))
  (cond [(not m) w]
        [else (dice-world #f (game-board m) m)]))

(define (find-move moves action)
  (define m
    (findf (lambda (m) (equal? (move-action m) action)) moves))
  (and m (move gt m)))

(define (mark w)
  (define tree
    (define board
      (define source
        (define focus
          (dice-world-gt w))
        (dice-world-board w))
      (dice-world-src w))
    (territory-index (first board)))
  
  (if source
      (attacking w source focus)
      (dice-world focus board tree)))

(define (attacking w source target)
  (define feasible (game-moves (dice-world-gt w)))
  (define attack
    (list source target))
  (define next
    (find-move feasible attack))
  (if next (dice-world #f (game-board next) next) w))

(define (unmark w)
  (dice-world #f (dice-world-board w) (dice-world-gt w)))

(define (territory-build)
  (for/list ([n (in-range GRID)])
    (territory n (modulo n PLAYER#) (dice) (get-x n) (get-y n))))

(define (dice)
  (add1 (random DICE#)))
(define (get-x n)
  (+ OFFSET0
     (if (odd? (get-row n)) 0 (/ X-OFFSET 2))
     (* X-OFFSET (modulo n BOARD))))
(define (get-y n)
  (+ OFFSET0 (* Y-OFFSET (get-row n))))

(define (get-row pos)
  (quotient pos BOARD))

(define (game-tree board player dice)
  ;; create tree of attacks from this position; add passing move
  (define (attacks board)
    (for*/list ([src board]
                [dst (neighbors (territory-index src))]
                #:when (attackable? board player src dst))
      (define from (territory-index src))
      (define dice (territory-dice src))
      (define newb (execute board player from dst dice))
      (define more (cons (passes newb) (attacks newb)))
      (move (list from dst) (game newb player more))))
  ;; create a passing move and the rest of the game tree
  (define (passes board)
    (define-values (new-dice newb) (distribute board player dice))
    (move ‘() (game-tree newb (switch player) new-dice)))
  ;; -- START: --
  (game board player (attacks board)))

(define (switch player)
  (modulo (add1 player) PLAYER#))

(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice] [new-board '()]) ([t board])
    (if (and (= (territory-player t) player)
             (< (territory-dice t) DICE#)
             (not (zero? dice)))
        (values (- dice 1) (cons (add-dice-to t) new-board))
        (values dice (cons t new-board)))))

(define (add-dice-to t)
  (territory-set-dice t (add1 (territory-dice t))))

;(define (neighbors n)
;  (list upper-right
;        bottom-right
;        upper-left
;        lower-left
;        right
;        left))

(define (add b x)
  (if b empty (list x)))

(define (neighbors pos)
  (define top?
    (< pos BOARD))
  (define bottom?
    (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right?
    (zero? (modulo (add1 pos) BOARD)))
  (define left?
    (zero? (modulo pos BOARD)))
  (if even-row?
      (even-row pos top? bottom? right? left?)
      (odd-row pos top? bottom? right? left?)))

(define (even-row pos top? bottom? right? left?)
                  (append (add (or top? right?) (add1 (- pos board)))
                          (add (or bottom? right?) (add1 (+ pos board)))
                          (add top? (- pos board))
                          (add bottom? (+ pos board))
                          (add right? (add1 pos))
                          (add left? (sub1 pos))))

(define (attackable? board player src dst)
  (define dst-t
    (findf (lambda (t) (= (territory-index t) dst)) board))
  (and dst-t
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

(define (execute board player src dst dice)
  (for/list ([t board])
    (define idx (territory-index t))
    (cond [(= idx src) (territory-set-dice t 1)]
          [(= idx dst)
           (define s (territory-set-dice t (- dice 1)))
           (territory-set-player s player)]
          [else t])))

(define (won board)
  (define-values (best-score w) (winners board))
  (if (cons? (rest w)) "It's a tie." "You won."))

(define (winners board)
  (for/fold ([best 0][winners '()]) ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player) (+ result 1) result)))
