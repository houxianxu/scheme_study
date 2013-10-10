;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname liwei) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "gui.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "gui.ss" "teachpack" "htdp")))))
;; 第一题
;; GCD : number number -> number
;; to computer the Greatest Common Divisor of two numbers
(define (GCD m n)
  (local ((define (better-GCD larger smaller)
            (cond ((= smaller 0) larger)
                  (else (better-GCD smaller (remainder larger smaller))))))
    (cond ((and (integer? m) (integer? n))
           (better-GCD (max m n) (min m n)))
          (else (error "m and/or n are/is not integer")))))

;; test
(equal? (GCD 4 3) 1)
(equal? (GCD 2 0) 2)

;; 第二题
;; sort-n : number-n (listof number) -> listof number
;; sort listof number which contains n (number-n) numbers
(define (sort-n n lon)
  (local ((define (sort1 n1 lon1)
            (cond ((empty? lon1) (cons n1 empty))
                  (else 
                   (cond ((<= n1 (car lon1)) (cons n1 lon1))
                         (else (cons (car lon1) (sort1 n1 (cdr lon1)))))))))
    (cond 
      ((empty? lon) empty)
      (else (sort1 (car lon) (sort-n n (cdr lon)))))))

;; test
(equal? (sort-n 5 (list 3 4 2 134 4)) (list 2 3 4 4 134))

;; 第三题
;;...

;; 第四题 
;; (random-between-3-14) -> number
;; produce a random number between 3 and 14
(define (random-between-3-14)
  (let ((random1 (random 15)))
    (if (>= random1 3)
        random1
        (random-between-3-14))))
 

;; Question 5
;; Data Definition
(define-struct card (rank kind))
;; a card is a structure
;; (make-struct rank kind)
;; rank is a number between 3 and 14, included. 
;; kind is a symbol, which is spades, hearts, clubs, or diamonds
(define kinds
  (list 'spades 'hearts 'clubs 'diamonds))
;; all-cards -> listof cards
(define all-cards
  (local ((define (cards kind)
            (map (λ (n) 
                   (make-card n kind)) 
                 (build-list 12 (λ (x) (+ 3 x))))))
    (append (cards 'spades) (cards 'hearts) (cards 'clubs) (cards 'diamonds))))

;; (hand-cards) -> listof cards
;; computer 17 cards ramdomly from all-cards

;; random-item-list : a list -> one item in the list
;; compute a intem from a given list randomly
(define (random-item-list alist)
  (local ((define random-index (random (length alist)))
          (define (ref-list n l)
            (cond ((= n 0) (car l))
                  (else (ref-list (- n 1) (cdr l))))))
    (ref-list random-index alist)))
;; (random-a-card) -> a card
;; to abstract a card from all-card randomly
(define (random-a-card-from-all-cards)
  (local ((define a-card (random-item-list all-cards)))
  ;;(local ((define random-rank (random-between-3-14))
          ;;(define random-kind (random-item-list kinds))
         ;; (define a-card 
            ;;(car (filter (λ (x)
                     ;;(symbol=? (card-kind x) random-kind))
                    ;;(filter (λ (x)
                              ;;(= (card-rank x) random-rank))
                         ;;   all-cards))))
    (begin
      (set! all-cards 
            (remove-a-card a-card all-cards))
      a-card)))
            
;; remove-a-card : card lof-card -> lof-card
;; to remove a given card from a list of cards
(define (remove-a-card a-card lof-card)
  (cond 
    ((empty? a-card) lof-card)
    ((and (= (card-rank a-card) (card-rank (car lof-card)))
          (symbol=?(card-kind a-card) (card-kind (car lof-card))))
        (cdr lof-card))
    
    (else (cons (car lof-card) (remove-a-card a-card (cdr lof-card))))))

;; remove-cards : lof-cards lof-cards -> lof-cards
;; to remove alof-cards from a lisf cards
(define (remove-cards lof-cards another-lof-cards)
  (local ((define rest-another-lof-cards another-lof-cards)
          (define (remove-a-card a-card another-lof-cards)
            (cond ((empty? a-card) another-lof-cards)
                  (else
                   (cond ((and (= (card-rank a-card) (card-rank (car another-lof-cards)))
                               (symbol=?(card-kind a-card) (card-kind (car another-lof-cards))))
                          (cdr another-lof-cards))
                         (else (cons (car another-lof-cards) (remove-a-card a-card (cdr another-lof-cards)))))))))
          
     (cond 
       ((empty? lof-cards) rest-another-lof-cards)
       (else 
         (begin
         (set! rest-another-lof-cards (remove-a-card (car lof-cards) rest-another-lof-cards))
         (remove-cards (cdr lof-cards) rest-another-lof-cards)
         )))))

(define (remove-lof-cards lof-cards lof-lof-cards)
     
     (cond 
       ((empty? lof-cards) lof-lof-cards)
       (else 
         (cond
           ((equal? lof-cards (car lof-lof-cards))
            (rest lof-lof-cards))
           (else 
            (cons (car lof-lof-cards)
                  (remove-lof-cards lof-cards (rest lof-lof-cards))))))))
    
    
 
  
;; random-hand-cards : number lof-cards -> lof-cards
;; to randomly abstract n cards from a given listof cards 

(define (random-cards n)
  (local ((define (random-a-card)
            (local ((define a-card (random-item-list all-cards)))
              (begin 
                (set! all-cards (remove-a-card a-card all-cards))
                a-card))))
    (cond ((= n 0) empty)
        (else (cons (random-a-card) (random-cards (- n 1)))))))
;; random-hand-cards -> lisof-cards
;; randomly abstract 17 cards from a all-cards 
(define cards-number 17)
(define (random-hand-cards)
  (random-cards cards-number))


;; sort-cards : lof-cards -> lof-cards
;; sort a list of cards by rank
(define (sort-cards lof-cards)
  (local ((define (sort1 a-card lof-cards1)
            (cond ((empty? lof-cards1) (cons a-card empty))
                  (else 
                   (cond ((<= (card-rank a-card) (card-rank (car lof-cards1))) (cons a-card lof-cards1))
                         (else (cons (car lof-cards1) (sort1 a-card (cdr lof-cards1)))))))))
    (cond 
      ((empty? lof-cards) empty)
      (else (sort1 (car lof-cards) (sort-cards (cdr lof-cards)))))))

(define my-sorted-cards (sort-cards (random-hand-cards)))
(define ai-sorted-cards (sort-cards (random-hand-cards)))

;; group-cards : lof-cards -> lof-lof-cards
;; to group list of cards by rank
(define (group-cards lof-cards)
  (local ((define rest-lof-cards lof-cards)
          (define first-group empty)
          (define first-rank 
            (if (empty? rest-lof-cards)
                empty
                (card-rank (car rest-lof-cards)))))
    (cond ((empty? rest-lof-cards) empty)
          (else
           (begin (set! first-group 
              (filter (λ (x) 
                    (= first-rank (card-rank x)))
                  lof-cards))
           (set! rest-lof-cards
                  (remove-cards first-group lof-cards))
           (cons first-group (group-cards rest-lof-cards)))))))
;;sort-lof-cards : lof-lof-cards -> lof-lof-cards
;; to sort the lof-lof-cards by length of each element
(define (length-sort-lof-cards lof-lof-cards)
  (local ((define (sort2 a-lof-card lof-lof-cards1)
            (cond ((empty? lof-lof-cards1) (cons a-lof-card empty))
                  (else 
                   (cond ((<= (length a-lof-card) (length (car lof-lof-cards1)))
                          (cons a-lof-card (length-sort-lof-cards  lof-lof-cards1)))
                         (else (cons (car lof-lof-cards1) (sort2 a-lof-card (cdr lof-lof-cards1))))))))) 
                   
    (cond 
      ((empty? lof-lof-cards) empty)
      (else (sort2 (car lof-lof-cards) (length-sort-lof-cards (cdr lof-lof-cards)))))))
    
(define ai-grouped-sorted-cards (group-cards ai-sorted-cards))
(define ai-grouped-length-sorted-cards (length-sort-lof-cards ai-grouped-sorted-cards))
(define my-grouped-sorted-cards (group-cards my-sorted-cards))
(define my-grouped-length-sorted-cards (length-sort-lof-cards my-grouped-sorted-cards))

              
;; 到此实现了对牌的分组与排序

;; card-initial> card card -> boolean
;; determine which card is larger, 
;; if the rank is the same, then spades, hearts, clubs, diamonds are defined from smallest to largest
(define (card-initial> a-card another-card)
  (cond ((> (card-rank a-card) (card-rank another-card))
        true)
        ((< (card-rank a-card) (card-rank another-card))
        false)
        (else
         (cond
           ((symbol=? (card-kind a-card) 'spades) false)
           ((and (symbol=? (card-kind a-card) 'hearts)
                 (or (symbol=? (card-kind another-card) 'clubs)
                     (symbol=? (card-kind another-card) 'diamonds)))
            false)
           ((and (symbol=? (card-kind a-card) 'clubs)
                 (symbol=? (card-kind another-card) 'diamonds))
            false)
           (else true)))))


;; card_> card card -> boolean
;; determine which card is larger,just by rank rather than kind 

(define (card_> a-card another-card)
  (cond ((> (card-rank a-card) (card-rank another-card))
        true)
        (else false)))


;; begin playing the game
(define (play)
    (if (card-initial> (car (car my-grouped-length-sorted-cards)) (car (car ai-grouped-length-sorted-cards)))
        (begin 
          (print "ai is the first one to play")
          (newline)
          (sleep-for-a-while 2)
          (ai-first-play))
        (begin
          (print "you are the first one to play, begin to give cards by (my-play-cards (list (make-card number symbol ...)))")
          (newline)
          (sleep-for-a-while 4)
          (print "your cards")
          (print my-grouped-length-sorted-cards)
          (newline)
          (sleep-for-a-while 1)
          (my-play-cards(users-card))
          )))
;; ai-first-play : -> lof-cards
;; to give the first cards or card for the ai, and effectly set the ai-sorted-cards to be 
;; the rest lof-cards after removing the the given cards

 

(define give-card empty)
;; ai is the first player
(define (ai-first-play)
  (begin (set! give-card (car ai-grouped-length-sorted-cards))
         (set! ai-grouped-length-sorted-cards (cdr ai-grouped-length-sorted-cards))
         (print "given cards")
         (print give-card)
         (newline)
         (sleep-for-a-while 2)
         (if (empty? ai-grouped-length-sorted-cards)
             (print "ai wins!")
             (print "your turn, begin to give cards by (my-play-cards (list (make-card number symbol ...)))"))
         (newline)
         (sleep-for-a-while 2)
         (print "your rest cards")
         (newline)
         (sleep-for-a-while 1)
          (print my-grouped-length-sorted-cards)
          (newline)
          (sleep-for-a-while 1)
          (my-play-cards (users-card))))
  
;; -> lof-cards 
;; read the cards for user's
(define (users-card)
  (local ((define lof-rank-kind (read))
          (define (my-lof-cards lof-rank-kind)
            (cond
              ((empty? lof-rank-kind) empty)
              (else 
               (cons (make-card (car lof-rank-kind) (cadr lof-rank-kind))
                     (my-lof-cards (cddr lof-rank-kind)))))))
    (my-lof-cards lof-rank-kind)))


;; the user plays the game by give lof-cards
(define (my-play-cards lof-cards)
  (begin
    (set! give-card lof-cards)
    (set! my-grouped-length-sorted-cards (remove-lof-cards lof-cards  my-grouped-length-sorted-cards))
    (if (empty? my-grouped-length-sorted-cards)
        (print "you win!")
        (begin (print "ai plays")
               (sleep-for-a-while 2)
               (ai-play)))))


;; (ai-play) : lof-cards -> lof-cards
;; to determine the list of cards ai could play according to the cards the user gave

(define (ai-play)
  (local ((define n (length give-card))
          (define available-cards-ai
            (filter (λ (x) (= (length x) n))
                      ai-grouped-length-sorted-cards))
          (define (chose-from-available lof-cards)
            (cond
              ((empty? lof-cards) 
               (begin (print"there is no card can be bigger than the use's, then it is your turn to play")
                      (sleep-for-a-while 2)
                      (print "your rest cards")
                      (newline)
                      (sleep-for-a-while 1)
                       (print my-grouped-length-sorted-cards)
                       (newline)
                       (sleep-for-a-while 1)
                       (my-play-cards (users-card))))
              (else
               (cond
                 ((card_> (car (car lof-cards)) (car give-card))
                  (begin 
                    (set! give-card (car lof-cards))
                    (set! ai-grouped-length-sorted-cards
                          (remove-lof-cards give-card  ai-grouped-length-sorted-cards))
                    (print give-card)
                    (newline)
                    (sleep-for-a-while 2)
                    (if (empty? ai-grouped-length-sorted-cards)
                        (print "ai wins!")
                        (print "your turn, begin to give cards by (my-play-cards (list (make-card number symbol ...)))"))
                    (newline)
                    (sleep-for-a-while 3)
                    (print "your rest cards")
                    (newline)
                    (sleep-for-a-while 1)
                    (print my-grouped-length-sorted-cards)
                    (newline)
                    (sleep-for-a-while 1)
                    (my-play-cards (users-card))))
                 (else (chose-from-available (rest lof-cards))))))))
  (chose-from-available available-cards-ai)))
            
;; (don't play) -> 
;; For user, if there is no card can be bigger than the ai's, the it is time for ai to play
(define (donnot-play)
  (begin
    (print "ai plays")
    (ai-first-play)))
  
  
#|(print "your cards")
 (sleep-for-a-while 2)
my-grouped-length-sorted-cards  
(sleep-for-a-while 2)
(play)
  |#       
          


  
;; test
 (equal? (card-initial> (make-card 11 'clubs) (make-card 12 'clubs)) false)
 (equal? (card-initial> (make-card 11 'clubs) (make-card 11 'spades)) true)
 (equal? (remove-cards (list (make-card 3 'diamonds)
                          (make-card 9 'diamonds))
                    (list (make-card 2 'diamonds) (make-card 3 'diamonds) 
                          (make-card 4 'diamonds) (make-card 5 'diamonds)
                          (make-card 9 'diamonds)))
      (list (make-card 2 'diamonds) (make-card 4 'diamonds) (make-card 5 'diamonds)))
 (equal? (group-cards
          (list (make-card 3 'spades) (make-card 3 'diamonds) (make-card 3 'heart)
                (make-card 9 'spades) (make-card 9 'diamonds)
                (make-card 10 'spades)))
         (list 
          (list (make-card 3 'spades) (make-card 3 'diamonds) (make-card 3 'heart))
          (list (make-card 9 'spades) (make-card 9 'diamonds))
          (list (make-card 10 'spades))))
 
  (play)