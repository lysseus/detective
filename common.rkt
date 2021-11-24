#lang racket

;;;
;;; COMMON
;;;
;;; Common bindings for Racket Detective.
;;;

(provide SUSPECT-KEY-VAL
         QUESTION-KEY-VAL
         ACCUSE-KEY-VAL
         ENTER-KEY-VAL
         END-TURN-KEY-VAL
         ON-KEY-VAL
         OFF-KEY-VAL
         LEFT-KEY-VAL
         RIGHT-KEY-VAL
         FACTS-KEY-VAL
         CONSOLE-KEY-VAL
         SUSPECT-IDS
         SUSPECT-NAMES
         suspect->name
         name->suspect
         PLACE-IDS
         PLACE-NAMES
         place->name
         name->place
         AREA-IDS
         AREA-NAMES
         area->name
         name->area
         SIDE-IDS
         SIDE-NAMES
         side->name
         name->side
         SEX-IDS
         SEX-NAMES
         sex->name
         name->sex
         WEAPON-IDS
         WEAPON-NAMES
         weapon->name
         name->weapon
         PRINTS-IDS
         PRINTS-NAMES
         prints->name
         name->prints
         BOOLEAN-IDS
         BOOLEAN-NAMES
         boolean->name
         name->boolean
         male?
         female?)

(define (male? s#)
  (and (number? s#) (< 0 s# 11)))
(define (female? s#)
  (and (number? s#) (< 10 s# 21)))

(define (~n v) (string-downcase (~a v)))

(define SUSPECT-KEY-VAL "s")
(define QUESTION-KEY-VAL "q")
(define ACCUSE-KEY-VAL "a")
(define ENTER-KEY-VAL "\r")
(define END-TURN-KEY-VAL "t")
(define ON-KEY-VAL " ")
(define OFF-KEY-VAL "escape")
(define LEFT-KEY-VAL "left")
(define RIGHT-KEY-VAL "right")
(define FACTS-KEY-VAL "f")
(define CONSOLE-KEY-VAL "c")

(define SUSPECT-IDS (range 1 21))
(define SUSPECT-NAMES
  (hash 1 "LENNY LITTLE"
        2 "AL FAROOK"
        3 "PEPE PEREZ"
        4 "TONY RACHETI"
        5 "MICKEY O'MALLEY"
        6 "MAX FINEFLUGLE"
        7 "RIPP RAPP"
        8 "BUSTER BAILEY"
        9 "ROCKY ROLL"
        10 "LING TONG"
        11 "IVY LITTLE"
        12 "LUCY TUMBLE"
        13 "PIPER PEREZ"
        14 "DINA RICHETI"
        15 "EILEEN STELLAR"
        16 "JOAN FINEFLUGLE"
        17 "ROSE PETTLE"
        18 "DORIS DILL"
        19 "CANDY ROLL"
        20 "SING WONG"))
(define (suspect->name n) (hash-ref SUSPECT-NAMES n "?"))
(define name->suspect
  (let ([h (for/hash ([(k v) SUSPECT-NAMES])
                   (values (~n v) k))])
    (λ (k) (hash-ref h (~n k) 0))))

(define PLACE-IDS '(A b C d E F))
(define PLACE-NAMES
  (hash 'A "Art Show"
        'b "Theater"
        'C "Card Party"
        'd "Docks"
        'E "Embassy"
        'F "Factory"))
(define (place->name n) (hash-ref PLACE-NAMES n "?"))
(define name->place
  (let ([h (for/hash ([(k v) PLACE-NAMES])
                   (values (~n v) k))])
    (λ (k) (hash-ref h (~n k) 0))))

(define AREA-IDS '(3 4 5))
(define AREA-NAMES
  (hash 3 "Uptown"
        4 "Midtown"
        5 "Downtown"))
(define (area->name n) (hash-ref AREA-NAMES n "?"))
(define name->area
  (let ([h (for/hash ([(k v) AREA-NAMES])
                   (values (~n v) k))])
    (λ (k) (hash-ref h (~n k) 0))))

(define SIDE-IDS (range 1 3))
;; East = 2, which corresponds to "yes" for East?
(define SIDE-NAMES
  (hash 1 "West Side"
        2 "East Side"))
(define (side->name n) (hash-ref SIDE-NAMES n "?"))
(define name->side
  (let ([h (for/hash ([(k v) SIDE-NAMES])
                   (values (~n v) k))])
    (λ (k) (hash-ref h (~n k) 0))))

(define SEX-IDS (range 1 3))
;; Male = 2, which corresponds to "yes" for Male?
(define SEX-NAMES
  (hash 1 "Female"
        2 "Male"))
(define (sex->name n) (hash-ref SEX-NAMES n "?"))
(define name->sex
  (let ([h (for/hash ([(k v) SEX-NAMES])
                   (values (~n v) k))])
    (λ (k) (hash-ref h (~n k) 0))))

(define WEAPON-IDS (range 1 3))
(define WEAPON-NAMES
  (hash 1 ".38"
        2 ".45"))
(define (weapon->name n) (hash-ref WEAPON-NAMES n "?"))
(define name->weapon
  (let ([h (for/hash ([(k v) WEAPON-NAMES])
                   (values (~n v) k))])
    (λ (k) (hash-ref h (~n k) 0))))

(define PRINTS-IDS (range 1 3))
;; Odd = 2, which corresponds to "Yes"
(define PRINTS-NAMES
  (hash 1 "Even"
        2 "Odd"))
(define (prints->name n) (hash-ref PRINTS-NAMES n "?"))
(define name->prints
  (let ([h (for/hash ([(k v) PRINTS-NAMES])
                   (values (~n v) k))])
    (λ (k) (hash-ref h (~n k) 0))))

(define BOOLEAN-IDS (range 1 3))
(define BOOLEAN-NAMES
  (hash 1 "No"
        2 "Yes"))
(define (boolean->name n) (hash-ref BOOLEAN-NAMES n "?"))
(define name->boolean
  (let ([h (for/hash ([(k v) BOOLEAN-NAMES])
                   (values (~n v) k))])
    (λ (k) (hash-ref h (~n k) 0))))
