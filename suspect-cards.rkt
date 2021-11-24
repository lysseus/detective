#lang racket

;;;
;;; SUSPECT CARDS
;;;
;;; This module provides a Card struct, a hash for the 20 Suspects,
;;; and hash for the 14 private questions.
;;;

(provide (struct-out Card)
         SUSPECT-CARDS
         QUESTIONS)

(require (only-in "common.rkt" SUSPECT-NAMES))

;; Each Card simulates the suspect cards used by the game 
(struct Card (id name role status ques) #:transparent)

(define SUSPECT-CARDS
  (hash 1 (Card 1 (hash-ref SUSPECT-NAMES 1) "BARTENDER" 11 '(1 2 9 13 14))
        2 (Card 2 (hash-ref SUSPECT-NAMES 2) "TYCOON" "PLAYBOY" '(2 3 9 13 14))
        3 (Card 3 (hash-ref SUSPECT-NAMES 3) "FLAMENCO DANCER" 13 '(3 4 5 13 14))
        4 (Card 4 (hash-ref SUSPECT-NAMES 4) "PROMOTER" 14 '(4 5 9 13 14))
        5 (Card 5 (hash-ref SUSPECT-NAMES 5) "RETIRED COP" "SINGLE" '(1 4 9 13 14))
        6 (Card 6 (hash-ref SUSPECT-NAMES 6) "PRODUCER" 16 '(5 6 12 13 14))
        7 (Card 7 (hash-ref SUSPECT-NAMES 7) "DOCKWORKER" "SINGLE" '(2 5 12 13 14))
        8 (Card 8 (hash-ref SUSPECT-NAMES 8) "BASKETBALL PLAYER" "SINGLE" '(3 5 12 13 14))
        9 (Card 9 (hash-ref SUSPECT-NAMES 9) "MUSICIAN" 19 '(1 5 12 13 14))
        10 (Card 10 (hash-ref SUSPECT-NAMES 10) "RESTAURANT OWNER" "BACHELOR" '(7 8 12 13 14))
        11 (Card 11 (hash-ref SUSPECT-NAMES 11) "LANDLADY" 1 '(6 7 11 13 14))
        12 (Card 12 (hash-ref SUSPECT-NAMES 12) "SECRETARY" "STILL LOOKING" '(2 6 11 13 14))
        13 (Card 13 (hash-ref SUSPECT-NAMES 13) "LATIN SINGER" 3 '(3 7 11 13 14))
        14 (Card 14 (hash-ref SUSPECT-NAMES 14) "SOCIALITE" 4 '(1 6 11 13 14))
        15 (Card 15 (hash-ref SUSPECT-NAMES 15) "ACTRESS" "ELIGIBLE" '(4 7 11 13 14))
        16 (Card 16 (hash-ref SUSPECT-NAMES 16) "ACCOUNTANT" 6 '(6 8 10 13 14))
        17 (Card 17 (hash-ref SUSPECT-NAMES 17) "GOSSIP COLUMNIST" "UNATTACHED" '(2 7 10 13 14))
        18 (Card 18 (hash-ref SUSPECT-NAMES 18) "LADY NEWSCASTER" "NOT LOOKING" '(1 8 10 13 14))
        19 (Card 19 (hash-ref SUSPECT-NAMES 19) "GROUPIE" 9 '(3 8 10 13 14))
        20 (Card 20 (hash-ref SUSPECT-NAMES 20) "WAITRESS" "SINGLE" '(4 8 10 13 14))))

(define QUESTIONS
  (hash 1 "Did the murderer go to the EAST SIDE?"
        2 "Did a MALE do it?"
        3 "What AREA did the murderer go to?"
        4 "Was the MURDER WEAPON a .38?"
        5 "Where was the .38 hidden?"
        6 "Where was the .45 hidden?"
        7 "Which PLACE contained only 3 suspects?"
        8 "Did the murderer go to PLACE A, B or C?"
        9 "Were you on the EAST SIDE?"
        10 "Which AREA were you in?"
        11 "Were you at PLACE A, B or C?"
        12 "Were you where a WEAPON was hidden?"
        13 "Are an odd-numbered suspect's PRINTS on the .38?"
        14 "Are an odd-numbered suspect's PRINTS on the .45?"))
