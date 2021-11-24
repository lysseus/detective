#lang racket

;;;
;;; CASE FACTS
;;;

(provide (struct-out Case-Facts)
         (struct-out Victim-Facts)
         (struct-out Murder-Facts)
         (struct-out Place-Facts)
         (struct-out Suspect-Facts)
         make-Case-Facts
         odd-fingerprints
         log-suspect-facts)

(require (only-in "common.rkt"
                  PLACE-IDS SUSPECT-IDS male? female? name->sex name->weapon name->boolean)
         "suspect-clues.rkt")

(struct Case-Facts (victim murder places suspects ques pool)
  #:mutable #:transparent)

(struct Victim-Facts (id place-id) #:mutable #:transparent)

(struct Murder-Facts (sex place area side weapon .38-place .45-place .38-prints .45-prints
                          3-sus-place ABC)
  #:mutable #:transparent)

(struct Place-Facts (area side weapon alibis eliminated?)
  #:mutable #:transparent)

(struct Suspect-Facts (place area side ABC at-weapon-place alibis eliminated? questioned?)
  #:mutable #:transparent)

(define (odd-fingerprints info facts s# m# .38?)  
  ;; Suspect data
  (define suspects (Case-Facts-suspects facts))
  (define sfacts (hash-ref suspects s#))
  (define s#-sex (if (male? s#) (name->sex "male") (name->sex "female")))
  ;; Murder data
  (define mfacts (Case-Facts-murder facts))
  (define m#-sex (Murder-Facts-sex mfacts))
  (define (lie) (car (shuffle (list 'yes 'no '?))))
  (define same-sex? (cond
                      [(false? m#-sex) #f]
                      [else (= s#-sex m#-sex)]))
  (define same-weapon-place? (cond
                               [(false? (Murder-Facts-weapon mfacts)) #f]
                               [(false? (Murder-Facts-.38-place mfacts)) #f]
                               [(false? (Suspect-Facts-place sfacts)) #f]
                               [(and .38?
                                     (= (name->weapon ".38") (Murder-Facts-weapon mfacts))
                                     (eq? (Murder-Facts-.38-place mfacts)
                                          (Suspect-Facts-place sfacts)))]
                               [(and (= (name->weapon ".45") (Murder-Facts-weapon mfacts))
                                     (eq? (Murder-Facts-.45-place mfacts)
                                          (Suspect-Facts-place sfacts)))]
                               [else #f]))
  (cond    
    [(and same-sex? same-weapon-place?)
     (if (odd? m#) 'yes 'no)]
    [(and (number? (Suspect-Facts-at-weapon-place sfacts))
          (= (name->boolean "yes") (Suspect-Facts-at-weapon-place sfacts)))
     (lie)]
    [else '?]))

(define (log-suspect-facts id clues case)
  (define suspects (Case-Facts-suspects case))
  (define sfacts (hash-ref suspects id))
  (define place (Suspect-Clues-place clues))
  (define area (Suspect-Clues-area clues))
  (define side (Suspect-Clues-side clues))
  (define alibis (Suspect-Clues-alibis clues))
  (unless (false? place) (log-suspect-facts-place id case place))
  (unless (false? area) (log-suspect-facts-area id case area))
  (unless (false? side) (log-suspect-facts-side id case side))
  (unless (empty? alibis) (log-suspect-facts-alibis id case alibis))
  (set-Suspect-Facts-questioned?! sfacts #t))

(define (log-suspect-facts-place id case place)
  ;; Set the place for this Suspect Facts
  (define suspect-facts (Case-Facts-suspects case))
  (define sfacts (hash-ref suspect-facts id))    
  (set-Suspect-Facts-place! sfacts place)
  ;; Add the suspect to the Place Facts
  (define place-facts (Case-Facts-places case))
  (define pfacts (hash-ref place-facts place))
  (define alibis (set->list (set-add (list->set (Place-Facts-alibis pfacts)) id)))
  (set-Place-Facts-alibis! pfacts alibis))

(define (log-suspect-facts-area id case area)
  ;; Set the area for this Suspect Facts
  (define suspect-facts (Case-Facts-suspects case))
  (define sfacts (hash-ref suspect-facts id))    
  (set-Suspect-Facts-area! sfacts area)
  ;; Add the area to the Place Facts
  (define place (Suspect-Facts-place sfacts))
  (unless (false? place)
    (define place-facts (Case-Facts-places case))
    (define pfacts (hash-ref place-facts place))
    (set-Place-Facts-area! pfacts area)))

(define (log-suspect-facts-side id case side)
  ;; Set the side for this Suspect Facts
  (define suspect-facts (Case-Facts-suspects case))
  (define sfacts (hash-ref suspect-facts id))    
  (set-Suspect-Facts-side! sfacts side)
  ;; Add the area to the Place Facts
  (define place (Suspect-Facts-place sfacts))
  (unless (false? place)
    (define place-facts (Case-Facts-places case))
    (define pfacts (hash-ref place-facts place))
    (set-Place-Facts-side! pfacts side)))

(define (log-suspect-facts-alibis id case alibis)
  ;; Set the alibis for this Suspect Facts
  (define suspect-facts (Case-Facts-suspects case))
  (define sfacts (hash-ref suspect-facts id))
  (set-Suspect-Facts-alibis! sfacts
                             (remove-duplicates (append alibis
                                                        (Suspect-Facts-alibis sfacts))))
  ;; Add the alibis to the Place Facts
  (define place (Suspect-Facts-place sfacts))
  (unless (false? place)
    (define place-facts (Case-Facts-places case))
    (define pfacts (hash-ref place-facts place))
    (set-Place-Facts-alibis! pfacts
                             (remove-duplicates (append alibis
                                                        (Place-Facts-alibis pfacts))))))

(define (make-Case-Facts victim-id place-id)
  (Case-Facts
   (Victim-Facts victim-id place-id)
   (Murder-Facts #f #f #f #f #f #f #f #f #f #f #f)
   (for/hash ([p PLACE-IDS])
     (values p (Place-Facts #f #f #f (if (eq? p place-id)
                                         (list victim-id)
                                         '())
                            (if (eq? place-id p) #t #f))))
   (for/hash ([s SUSPECT-IDS])
     (values s (Suspect-Facts #f #f #f #f #f '() (if (= victim-id s) #t #f)
                              (if (= victim-id s) #t #f))))
   (remove victim-id SUSPECT-IDS)
   (remove victim-id SUSPECT-IDS)))
