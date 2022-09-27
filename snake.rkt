;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |exercise 5 - snake|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./snake_lib.rkt")

; a game is...
; - (make-game snake (listof posn) (listof posn) number)
; (define-struct game (snake food obstacles ticks))

; a direction is one of...
; - 'up
; - 'down
; - 'left
; - 'right
; If this type looks new to you, its just a symbol.
; That is ‘up is a symbol and “up” is a string.
; Symbols are like strings without spaces. 


; a snake is...
; - (make-snake direction (listof posn))
; (define-struct snake (heading segments))

; segments is either
; - (cons posn empty)
; - (cons posn segments)
; That is, segments is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-food : game posn -> game
; Given a game and posn, returns a new game (so you want to call make-game here)
; where food has been added
; at that posn. 
(define (add-food g p)
  (make-game (game-snake g)
             (cons p (game-food g))
             (game-obstacles g)
             (game-ticks g))
  )

(check-expect
 (add-food (make-game (make-snake 'up (list (make-posn 1 2)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      5)
           (make-posn 6 7))
 (make-game (make-snake 'up (list (make-posn 1 2)))
            (list (make-posn 6 7) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            5))
(check-expect (add-food (make-game (make-snake 'down
                                               (list (make-posn 1 1)
                                                     (make-posn 1 2)
                                                     (make-posn 1 3)))
                                   (list (make-posn 0 1)
                                         (make-posn 10 9)
                                         (make-posn 20 30))
                                   (list (make-posn 23 30)
                                         (make-posn 45 2))
                                   5)
                        (make-posn 5 5))
              (make-game (make-snake 'down
                                     (list (make-posn 1 1)
                                           (make-posn 1 2)
                                           (make-posn 1 3)))
                         (list (make-posn 5 5)
                               (make-posn 0 1)
                               (make-posn 10 9)
                               (make-posn 20 30))
                         (list (make-posn 23 30)
                               (make-posn 45 2))
                         5)
              )
(check-expect (add-food (make-game (make-snake 'down
                                               (list (make-posn 1 1)
                                                     (make-posn 1 2)
                                                     (make-posn 1 3)))
                                   (list empty)
                                   (list (make-posn 23 30)
                                         (make-posn 45 2))
                                   5)
                        (make-posn 5 5))
              (make-game (make-snake 'down
                                     (list (make-posn 1 1)
                                           (make-posn 1 2)
                                           (make-posn 1 3)))
                         (list (make-posn 5 5) '())
                         (list (make-posn 23 30)
                               (make-posn 45 2))
                         5)
              )

; change-direction : game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 
(define (change-direction g d)
  (make-game (make-snake d (snake-segments (game-snake g)))
             (game-food g)
             (game-obstacles g)
             (game-ticks g))
  )

(check-expect
 (change-direction
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'left)
 (make-game (make-snake 'left (list (make-posn 1 2)))
            (list (make-posn 3 4))
            empty
            5))
(check-expect
 (change-direction
  (make-game (make-snake 'left (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'left)
 (make-game (make-snake 'left (list (make-posn 1 2)))
            (list (make-posn 3 4))
            empty
            5))
(check-expect
 (change-direction
  (make-game (make-snake 'up (list (make-posn 1 2)
                                   (make-posn 2 2)))
             empty
             empty
             200)
  'right)
 (make-game (make-snake 'right (list (make-posn 1 2)
                                     (make-posn 2 2)))
            empty
            empty
            200))
(check-expect
 (change-direction
  (make-game (make-snake 'up (list (make-posn 1 2)
                                   (make-posn 1 3)))
             empty
             empty
             200)
  'down)
 (make-game (make-snake 'down (list (make-posn 1 2)
                                    (make-posn 1 3)))
            empty
            empty
            200))

; game-score : game -> number
; Given a game, returns a score (as a number)
;;; long snake more points
;;; go fast more points (less ticks)
(define (game-score g)
  (- (* 200 (length (snake-segments (game-snake g))))
     (game-ticks g))
  )
; no tests are provided for game-score because it is open-ended
; feel free to implement it however you would like to

; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise.
; We strongly recommend writing helper functions for this question!
(define (game-over? g)
  (local [;hit-itself?: game -> boolean
          ;returns true if snake hit itself, false otherwise
          (define (hit-itself? g)
            ;head of snake has same posn as another of its segments
            (ormap (λ (seg) (equal? seg
                                    (first (snake-segments (game-snake g)))))
                   (rest (snake-segments (game-snake g))))
            )
          ;hit-wall?: game -> boolean
          ;returns true if snake hit the wall, false otherwise
          (define (hit-wall? g)
            ;if any of the x or y coordinates are less than 1 or greater than 50,
            ; - die
            (local [;coords-list g: game -> (listof numbers)
                    ;makes a list of all the x and y coordinates of the snake body
                    (define (coords-list g)
                      (append (map (λ (p) (posn-x p))
                                   (snake-segments (game-snake g)))
                              (map (λ (p) (posn-y p))
                                   (snake-segments (game-snake g)))))]
              (cond [(ormap (λ (x) (< x 1))
                            (coords-list g))          true]
                    [(ormap (λ (x) (> x 50))
                            (coords-list g))          true]
                    [else                         false]
                    )))
          ;hit-obs?: game -> boolean
          ;return true if snake hit an obstacle, false otherwise
          (define (hit-obs? g)
            ;are any of the obstacle posns equal to the head posn?
            (ormap (λ (p) (equal? p (first (snake-segments (game-snake g)))))
                   (game-obstacles g)
                   ))
          ]
    (cond
      ;snake dies if hit itself
      [(hit-itself? g)         true]
      ;snake dies if hit a wall
      [(hit-wall? g)           true]
      ;snake dies if hit obstacle
      [(hit-obs? g)            true]
      [else                    false]
      )
    ))

(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn -1 1))) empty empty 5))
 true)
;tests hit-itself scenario
(check-expect (game-over? (make-game (make-snake 'down
                                                 (list (make-posn 4 3)
                                                       (make-posn 4 4)
                                                       (make-posn 4 5)
                                                       (make-posn 4 6)
                                                       (make-posn 3 6)
                                                       (make-posn 2 6)
                                                       (make-posn 1 6)
                                                       (make-posn 1 5)
                                                       (make-posn 1 4)
                                                       (make-posn 1 3)
                                                       (make-posn 2 3)
                                                       (make-posn 3 3)
                                                       (make-posn 4 3)
                                                       (make-posn 5 3)))
                                     empty
                                     empty
                                     27))
              true)
(check-expect (game-over? (make-game (make-snake 'up
                                                 (list (make-posn 4 4)
                                                       (make-posn 4 4)
                                                       (make-posn 4 5)
                                                       ))
                                     empty
                                     empty
                                     27))
              true)
;tests hit-wall with x or y being greater than 50 scenario
(check-expect (game-over? (make-game (make-snake 'left
                                                 (list (make-posn 50 20)
                                                       (make-posn 49 20)
                                                       (make-posn 48 20)))
                                     empty
                                     empty
                                     30))
              false)
(check-expect (game-over? (make-game (make-snake 'left
                                                 (list (make-posn 51 20)
                                                       (make-posn 50 20)
                                                       (make-posn 49 20)
                                                       (make-posn 48 20)))
                                     empty
                                     empty
                                     30))
              true)
(check-expect (game-over? (make-game (make-snake 'left
                                                 (list (make-posn 30 50)
                                                       (make-posn 30 49)
                                                       (make-posn 30 48)
                                                       (make-posn 30 47)))
                                     empty
                                     empty
                                     30))
              false)
(check-expect (game-over? (make-game (make-snake 'left
                                                 (list (make-posn 30 51)
                                                       (make-posn 30 50)
                                                       (make-posn 30 49)
                                                       (make-posn 30 48)))
                                     empty
                                     empty
                                     30))
              true)
;tests hit-obstacles
(check-expect (game-over? (make-game (make-snake 'up
                                                 (list (make-posn 3 7)
                                                       (make-posn 3 6)
                                                       (make-posn 3 5)
                                                       (make-posn 3 4)))
                                     empty
                                     (list (make-posn 40 1)
                                           (make-posn 3 7)
                                           (make-posn 34 21))
                                     50))
              true)
(check-expect (game-over? (make-game (make-snake 'up
                                                 (list (make-posn 3 7)
                                                       (make-posn 3 6)
                                                       (make-posn 3 5)
                                                       (make-posn 3 4)))
                                     empty
                                     (list (make-posn 40 1)
                                           (make-posn 3 5)
                                           (make-posn 34 21))
                                     50))
              false)
                                                             
; advance-game : game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not. 
(define (advance-game g)
  (local [;newhead: game -> posn
          ;gives posn of new head of snake based on previous head and direction
          (define (newhead g)
            (cond [(equal? 'up (snake-heading (game-snake g)))
                   (make-posn (posn-x (first (snake-segments (game-snake g))))
                              (+ 1 (posn-y (first (snake-segments (game-snake g))))))]
                  [(equal? 'down (snake-heading (game-snake g)))
                   (make-posn (posn-x (first (snake-segments (game-snake g))))
                              (- (posn-y (first (snake-segments (game-snake g)))) 1))]
                  [(equal? 'left (snake-heading (game-snake g)))
                   (make-posn (- (posn-x (first (snake-segments (game-snake g)))) 1)
                              (posn-y (first (snake-segments (game-snake g)))))]
                  [(equal? 'right (snake-heading (game-snake g)))
                   (make-posn (+ (posn-x (first (snake-segments (game-snake g)))) 1)
                              (posn-y (first (snake-segments (game-snake g)))))]
                  ))
          ;newfoods: (listof posns), posn -> (listof posns)
          ;gets rid of eaten food
          (define (newfoods oldfoods newhead)
            (filter (λ (p) (not (equal? p newhead)))
                    oldfoods))
          ;newbody: (listof posns), posn, (listof posns) -> (listof posns)
          ;if eats food, new body same as old snake segments. otherwise, delete
          ;- last posn in old snake segments
          (define (newbody oldsegs newhead oldfoods)
            (cond [(ormap (λ (p)
                            (equal? p newhead))
                          oldfoods)             oldsegs]
                  [else                           (reverse (rest (reverse oldsegs)))]
                  ))
          ]
    (make-game (make-snake (snake-heading (game-snake g)) ;no change
                           ;add new head to front of snake's new body
                           (cons (newhead g)
                                 (newbody (snake-segments (game-snake g))
                                          (newhead g)
                                          (game-food g))))
               (newfoods (game-food g) (newhead g))
               (game-obstacles g) ;no change
               (+ 1 (game-ticks g)))
               ))

(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             empty
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)))
            empty
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             (list (make-posn 2 1) (make-posn 8 9))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)
                                    (make-posn 3 3)))
            (list (make-posn 8 9))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
;test up, left, right directions
(check-expect (advance-game (make-game (make-snake 'up
                                                   (list (make-posn 1 3)
                                                         (make-posn 1 4)))
                                       (list (make-posn 17 3)
                                             (make-posn 5 2)
                                             (make-posn 1 1))
                                       empty
                                       30))
              (make-game (make-snake 'up
                                     (list (make-posn 1 4)
                                           (make-posn 1 3)))
                         (list (make-posn 17 3)
                               (make-posn 5 2)
                               (make-posn 1 1))
                         empty
                         31))
(check-expect (advance-game (make-game (make-snake 'right
                                                   (list (make-posn 1 3)
                                                         (make-posn 1 4)
                                                         (make-posn 1 5)
                                                         (make-posn 2 5)))
                                       (list (make-posn 17 3)
                                             (make-posn 5 2)
                                             (make-posn 2 3)
                                             (make-posn 1 1))
                                       empty
                                       30))
              (make-game (make-snake 'right
                                     (list (make-posn 2 3)
                                           (make-posn 1 3)
                                           (make-posn 1 4)
                                           (make-posn 1 5)
                                           (make-posn 2 5)
                                           ))
                         (list (make-posn 17 3)
                               (make-posn 5 2)
                               (make-posn 1 1))
                         empty
                         31))
(check-expect (advance-game (make-game (make-snake 'left
                                                   (list (make-posn 2 3)
                                                         (make-posn 2 4)
                                                         (make-posn 2 5)
                                                         (make-posn 3 5)))
                                       (list (make-posn 17 3)
                                             (make-posn 5 2)
                                             (make-posn 2 3)
                                             (make-posn 1 3)
                                             (make-posn 1 1))
                                       empty
                                       30))
              (make-game (make-snake 'left
                                     (list (make-posn 1 3)
                                           (make-posn 2 3)
                                           (make-posn 2 4)
                                           (make-posn 2 5)
                                           (make-posn 3 5)
                                           ))
                         (list (make-posn 17 3)
                               (make-posn 5 2)
                               (make-posn 2 3)
                               (make-posn 1 1))
                         empty
                         31))
(check-expect (advance-game (make-game (make-snake 'left
                                                   (list (make-posn 2 3)
                                                         (make-posn 2 4)
                                                         (make-posn 2 5)
                                                         (make-posn 3 5)))
                                       (list (make-posn 17 3)
                                             (make-posn 5 2)
                                             (make-posn 2 3)
                                             (make-posn 1 1))
                                       empty
                                       30))
              (make-game (make-snake 'left
                                     (list (make-posn 1 3)
                                           (make-posn 2 3)
                                           (make-posn 2 4)
                                           (make-posn 2 5)
                                           ))
                         (list (make-posn 17 3)
                               (make-posn 5 2)
                               (make-posn 2 3)
                               (make-posn 1 1))
                         empty
                         31))
                            
; a starting game to experiment with
(define game-start
  (make-game (make-snake 'up (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             (list (make-posn 30 15)
                   (make-posn 13 16)
                   (make-posn 4 4)
                   (make-posn 45 40)
                   (make-posn 10 39)
                   (make-posn 15 49))
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game advance-game add-food change-direction game-score game-over?))

;to start a game
(play game-start)

;to make sure all procedures are defined with no typos
(check-expect (procedure? add-food) #true)
(check-expect (procedure? change-direction) #true)
(check-expect (procedure? game-score) #true)
(check-expect (procedure? game-over?) #true)
(check-expect (procedure? advance-game) #true)