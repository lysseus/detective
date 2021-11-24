#lang racket

;;;
;;; MURDER INFO
;;
;;; Creates the Murder-Info for Racket Detective.
;;;

(provide (struct-out Murder-Info)
         (struct-out Place-Info)
         (struct-out Suspect-Info)
         make-Murder-Info
         east-side?
         area
         place)

(require utils/list
         "common.rkt"
         "suspect-cards.rkt")

(struct Murder-Info (victim-id
                     victim-place
                     murderer-id
                     .38?
                     .38-place
                     .45-place
                     3-sus-place
                     places
                     suspects)
  #:transparent)

(struct Suspect-Info (place area side alibis) #:transparent)
(struct Place-Info (area side weapon suspects) #:transparent)

(define ODD-MALES (range 1 11 2))
(define EVEN-MALES (range 2 11 2))
(define ODD-FEMALES (range 11 21 2))
(define EVEN-FEMALES (range 12 21 2))

(define (east-side? info id)
  (define s (hash-ref (Murder-Info-suspects info) id))
  (= (name->side "east side") (Suspect-Info-side s)))

(define (area info id)
  (define sinfo (hash-ref (Murder-Info-suspects info) id))
  (Suspect-Info-area sinfo))

(define (place info id)
  (define sinfo (hash-ref (Murder-Info-suspects info) id))
  (Suspect-Info-place sinfo))

(define (make-Murder-Info)
  (define alibis (for/list ([odd-male (shuffle ODD-MALES)]
                            [even-male (shuffle EVEN-MALES)]
                            [odd-female (shuffle ODD-FEMALES)]
                            [even-female (shuffle EVEN-FEMALES)])
                   (shuffle (list odd-male even-male odd-female even-female))))

  (define alibis-grps
    (cons (list (caar alibis)) (shuffle (cons (cdar alibis) (cdr alibis)))))

  (define place-info-grps
    (zip (shuffle PLACE-IDS)
         (shuffle AREA-IDS)
         (shuffle SIDE-IDS)))

  (define VICTIM-ID (first (first alibis-grps)))
  (define VICTIM-PLACE (first (first place-info-grps)))
  (define MURDERER-ID (first (second alibis-grps)))
  (define .38? (car (shuffle (list #t #f))))
  (define MURDERER-PLACE (first (second place-info-grps)))
  (define .38-PLACE
    (first (shuffle (remove* (list VICTIM-PLACE
                                   MURDERER-PLACE) PLACE-IDS))))
  (define .45-PLACE
    (first (shuffle (remove* (list VICTIM-PLACE
                                   MURDERER-PLACE
                                   .38-PLACE) PLACE-IDS))))
  (define PLACES (make-hash))
  (for ([place-info place-info-grps]
        [alibis alibis-grps]
        [w '(#f #f #f #f .38 .45)])
    (hash-set! PLACES (first place-info)
               (Place-Info (second place-info)
                           (third place-info)
                           w
                           alibis)))
  (define 3-SUSPECTS-PLACE
    (for/last ([p (hash-keys PLACES)]
               [p-info (hash-values PLACES)]
               #:when (= 3 (length (Place-Info-suspects p-info))))
      p))
  (define SUSPECTS (make-hash))

  (for ([alibis (rest alibis-grps)]
        [place-info (rest place-info-grps)])
    (for ([suspect alibis])
      (hash-set! SUSPECTS
                 suspect
                 (Suspect-Info (first place-info)
                          (second place-info)
                          (third place-info)
                          (remove suspect alibis)))))
  
  (Murder-Info VICTIM-ID
                VICTIM-PLACE
                MURDERER-ID
                .38?
                .38-PLACE
                .45-PLACE
                3-SUSPECTS-PLACE
                PLACES
                SUSPECTS))
