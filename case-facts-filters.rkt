#lang racket

;;;
;;; CASE FACTS FILTERS
;;;

(provide filter-facts)

(require "common.rkt"
         "case-facts.rkt")

(define/contract (hash-fold hsh (accessor identity))
  (->* (hash?) (procedure?) hash?)
  (define (fold lst)
    (define ans
      (remove-duplicates
       (for/list ([x lst])
         (remove-duplicates
          (flatten
           (filter (λ (v) (not (set-empty? (set-intersect (list->set x)
                                                          (list->set v)))))
                   lst))))))
    (cond
      [(= (length lst) (length ans)) ans]
      [else (fold ans)]))
  (define lst
    (for/list ([(k v) hsh])
      (cons k (accessor v))))  
  (for*/hash ([v (fold lst)]
              [s# v])
    (values s# (remove s# (set->list v)))))

(define (normalize-suspect/alibis cfacts)
  (define mfacts (Case-Facts-murder cfacts))
  (define suspects (Case-Facts-suspects cfacts))
  (define places (Case-Facts-places cfacts))

  (define weapon-places (remove* '(#f)
                                 (list (Murder-Facts-.38-place mfacts)
                                       (Murder-Facts-.45-place mfacts))))
  
  ;; Normalize the known alibis for each suspect
  (for ([(s# alibis) (hash-fold suspects Suspect-Facts-alibis)])
    (define sfacts (hash-ref suspects s#))
    (set-Suspect-Facts-alibis! sfacts alibis))

  ;; Iterate over suspeects.
  (for ([(s# sfacts) suspects])

    ;; We know the suspect's place.
    (when (Suspect-Facts-place sfacts)
      ;; Set the ABC for this suspect.
      (if (member (Suspect-Facts-place sfacts) (take PLACE-IDS 3))
          (set-Suspect-Facts-ABC! sfacts (name->boolean "yes"))
          (set-Suspect-Facts-ABC! sfacts (name->boolean "no")))
          
        ;; Set at-weapon-place for suspect when known.
      (cond
        [(empty? weapon-places) (void)]
        ;; When we know both weapon places, we can answer yes or no.
        [(= 2 (length weapon-places))        
         (if (member (Suspect-Facts-place sfacts) weapon-places)
             (set-Suspect-Facts-at-weapon-place! sfacts (name->boolean "yes"))
             (set-Suspect-Facts-at-weapon-place! sfacts (name->boolean "no")))]
        ;; We only know 1 weapon place. We can only answer yes.
        [else
         (when (member (Suspect-Facts-place sfacts) weapon-places)
           (set-Suspect-Facts-at-weapon-place! sfacts (name->boolean "yes")))]))
    
    (for ([a# (Suspect-Facts-alibis sfacts)])
      (define afacts (hash-ref suspects a#))
      ;; Normalize place.
      (if (Suspect-Facts-place sfacts)
          (set-Suspect-Facts-place! afacts (Suspect-Facts-place sfacts))
          (set-Suspect-Facts-place! sfacts (Suspect-Facts-place afacts)))
      ;; Normalize area.
      (if (Suspect-Facts-area sfacts)
          (set-Suspect-Facts-area! afacts (Suspect-Facts-area sfacts))
          (set-Suspect-Facts-area! sfacts (Suspect-Facts-area afacts)))
      ;; Normalize side.
      (if (Suspect-Facts-side sfacts)
          (set-Suspect-Facts-side! afacts (Suspect-Facts-side sfacts))
          (set-Suspect-Facts-side! sfacts (Suspect-Facts-side afacts)))
      ;; Normalize ABC.
      (if (Suspect-Facts-ABC sfacts)
          (set-Suspect-Facts-ABC! afacts (Suspect-Facts-ABC sfacts))
          (set-Suspect-Facts-ABC! sfacts (Suspect-Facts-ABC afacts)))
      ;; Normalize at-weapon-place.
      (if (Suspect-Facts-at-weapon-place sfacts)
          (set-Suspect-Facts-at-weapon-place! afacts (Suspect-Facts-at-weapon-place sfacts))
          (set-Suspect-Facts-at-weapon-place! sfacts (Suspect-Facts-at-weapon-place afacts))))
    
    (define p# (Suspect-Facts-place sfacts))
    ;; When the suspect declares a place, add him to the place alibis.
    (when p#
      (define pfacts (hash-ref places p#))
      (unless (member s# (Place-Facts-alibis pfacts))
        (set-Place-Facts-alibis! pfacts (cons s# (Place-Facts-alibis pfacts))))
      ;; When the suspect declares an area, add it to the place.
      (when (Suspect-Facts-area sfacts)
        (set-Place-Facts-area! pfacts (Suspect-Facts-area sfacts)))
      ;; When the suspect declares a side, add it to the place.
      (when (Suspect-Facts-side sfacts)
        (set-Place-Facts-side! pfacts (Suspect-Facts-side sfacts))))))

(define (normalize-places cfacts)
  (define mfacts (Case-Facts-murder cfacts))
  (define places (Case-Facts-places cfacts))
  (define suspects (Case-Facts-suspects cfacts))

  ;; Normalize victim place
  (define v# (Victim-Facts-id (Case-Facts-victim cfacts)))
  (define vfacts (hash-ref suspects v#))
  (define vp# (Victim-Facts-place-id (Case-Facts-victim cfacts)))
  (define vpfacts (hash-ref places vp#))
  (set-Suspect-Facts-place! vfacts vp#)
  
  ;; Iterate over places collecting unassigned areas and sides.
  (define-values (areas sides sus 3-sus-place)
    (for/fold ([areas (append AREA-IDS AREA-IDS)]
               [sides (append SIDE-IDS SIDE-IDS SIDE-IDS)]
               [sus empty]
               [3-sus-place #f])
              ([(p# pfacts) places])
      (values (if (Place-Facts-area pfacts)
                  (remove (Place-Facts-area pfacts) areas)
                  areas)
              (if (Place-Facts-side pfacts)
                  (remove (Place-Facts-side pfacts) sides)
                  sides)
              (append (Place-Facts-alibis pfacts) sus)
              (if (= 3 (length (Place-Facts-alibis pfacts)))
                  p#
                  3-sus-place))))

  ;; Normalize victim area.
  (when (= 1 (length areas))
    (set-Place-Facts-area! vpfacts (first areas))
    (set-Suspect-Facts-area! vfacts (first areas)))

  ;; Normalize victim side.  
  (when (= 1 (length sides))
    (set-Place-Facts-side! vpfacts (first sides))
    (set-Suspect-Facts-side! vfacts (first sides)))

  ;; Normalize 3-sus-place
  (when (and (= 20 (length sus)) 3-sus-place)
    (set-Murder-Facts-3-sus-place! mfacts 3-sus-place)))

(define (filter-suspects cfacts)
  (define mfacts (Case-Facts-murder cfacts))
  (define suspects (Case-Facts-suspects cfacts))
  
  (define (mfacts-sex? v)
    (define sex (Murder-Facts-sex mfacts))
    (cond
      [(false? sex) #f]
      [else
       (= (name->sex v) sex)]))
  (define (suspect-male? n) (< n 11))
  (define (suspect-female? n) (> n 10))
  (for ([(s# sfacts) suspects])
    ;; Filter for murder sex
    ;; When we know the murder sex we can eliminate the opposite-sex suspects.
    (cond
      [(false? (Murder-Facts-sex mfacts)) (void)]
      [(or (and (mfacts-sex? 'male) (suspect-male? s#))
           (and (mfacts-sex? 'female) (suspect-female? s#)))
       (void)]
      [else
       (set-Suspect-Facts-eliminated?! sfacts #t)])

    ;; Filter suspects at weapon place.
    (when (equal? (name->boolean "yes") (Suspect-Facts-at-weapon-place sfacts))
      (set-Suspect-Facts-eliminated?! sfacts #t))))



(define (filter-places mfacts places suspects)
  ;; Returns true if there is a match for area and side
  (define (match? place-area place-side murder-area murder-side)
    (cond
      [(or (false? place-area)
           (false? place-side)
           (false? murder-area)
           (false? murder-side)) #f]
      [else
       (and (= place-area murder-area)
            (= place-side murder-side))]))
  (define val (for/last ([(id p) places]
                         #:when (match? (Place-Facts-area p)
                                        (Place-Facts-side p)
                                        (Murder-Facts-area mfacts)
                                        (Murder-Facts-side mfacts)))
                (list id p)))

  ;; We matched a place to area and side
  (unless (false? val)
    (set-Murder-Facts-place! mfacts (first val))
    (for* ([(p-id pfacts) places]
           [a# (Place-Facts-alibis pfacts)])
      (define afacts (hash-ref suspects a#))
      (cond
        [(eq? (first val) p-id)
         (when (false? (Suspect-Facts-place afacts))
           (set-Suspect-Facts-place! afacts p-id))]
        [else         
         (set-Place-Facts-eliminated?! pfacts #t)         
         (set-Suspect-Facts-eliminated?! afacts #t)])))
  
  ;; Filtering out places that have suspects at-weapon-place.
  (for ([(p# pfacts) places])
    (define alibis  (Place-Facts-alibis pfacts))
    (define s# (if (empty? alibis) #f (first alibis)))
    (when s#
      (define sfacts (hash-ref suspects s#))
      (when (equal? (Suspect-Facts-at-weapon-place sfacts) (name->boolean "yes"))        
        (set-Place-Facts-eliminated?! pfacts #t)))))

(define (filter-murder cfacts)  
  (define mfacts (Case-Facts-murder cfacts))  
  (define places (Case-Facts-places cfacts))
  (define suspects (Case-Facts-suspects cfacts))

  ;; Filter victim
  (define victim-id (Victim-Facts-id (Case-Facts-victim cfacts)))
  (define vfacts (hash-ref suspects victim-id))
  (set-Suspect-Facts-eliminated?! vfacts #t)
  
  ;; Filter suspects
  (filter-suspects cfacts)

  ;; Filter murder place
  (filter-places mfacts places suspects)

  (for ([(p# pfacts) places])
    (when (Place-Facts-weapon pfacts)      
      (set-Place-Facts-eliminated?! pfacts #t)
      (for ([s# (Place-Facts-alibis pfacts)])
        (define sfacts (hash-ref suspects s#))
        (set-Suspect-Facts-at-weapon-place! sfacts (name->boolean "yes"))
        (set-Suspect-Facts-eliminated?! sfacts #t))))
  
  (for* ([(p-id pfacts) places]
         [a# (Place-Facts-alibis pfacts)])
    (define afacts (hash-ref suspects a#))
    ;; Filter murder-place
    (unless (false? (Murder-Facts-place mfacts))
      (unless (eq? (Murder-Facts-place mfacts) p-id)
        (when (false? (Place-Facts-eliminated? pfacts))          
          (set-Place-Facts-eliminated?! pfacts #t))
        (when (false? (Suspect-Facts-eliminated? afacts))
          (set-Suspect-Facts-eliminated?! afacts #t))))
    
    ;; Filter for murder area
    (when (and (Murder-Facts-area mfacts)
               (Place-Facts-area pfacts))      
      (unless (= (Murder-Facts-area mfacts) (Place-Facts-area pfacts))
        (when (false? (Place-Facts-eliminated? pfacts))          
          (set-Place-Facts-eliminated?! pfacts #t))
        (when (false? (Suspect-Facts-eliminated? afacts))
          (set-Suspect-Facts-eliminated?! afacts #t))))

    ;; Filter for murder side
    (when (and (Murder-Facts-side mfacts)
               (Place-Facts-side pfacts))
      (unless (= (Murder-Facts-side mfacts) (Place-Facts-side pfacts))
        (when (false? (Place-Facts-eliminated? pfacts))          
          (set-Place-Facts-eliminated?! pfacts #t))
        (when (false? (Suspect-Facts-eliminated? afacts))
          (set-Suspect-Facts-eliminated?! afacts #t))))
    
    ;; Filter for murder-ABC
    (unless (false? (Murder-Facts-ABC mfacts))
      (define ps (if (= (name->boolean 'yes) (Murder-Facts-ABC mfacts))
                     (drop PLACE-IDS 3)
                     (take PLACE-IDS 3)))
      (for ([p-id ps])
        (define pfacts (hash-ref places p-id))        
        (set-Place-Facts-eliminated?! pfacts #t)
        (for ([a# (Place-Facts-alibis pfacts)])
          (define afacts (hash-ref suspects a#))
          (set-Suspect-Facts-eliminated?! afacts #t)))))

  (define weapon (Murder-Facts-weapon mfacts))
  (define .38-prints (cond
                       [(false? weapon) #f]
                       [(not (string=? ".38" (weapon->name weapon))) #f]
                       [else (Murder-Facts-.38-prints mfacts)]))
  (define .45-prints (cond
                       [(false? weapon) #f]
                       [(not (string=? ".45" (weapon->name weapon))) #f]
                       [else (Murder-Facts-.45-prints mfacts)]))
  ;; prints will be 0 (unknown), 1 (even), or 2 (odd).
  (define prints (let ([ans (or .38-prints .45-prints)])
                   (if ans ans 0)))
  ;; We will eliminate the opposite prints suspect.
  (define prints? (cond
                    [(= (name->prints "odd") prints) odd?]
                    [(= (name->prints "even") prints) even?]
                    [else (const #t)]))
  
  ;; Iterate over suspects.
  (for ([(s# sfacts) suspects])
    ;; Filter for murder area
    (unless (or (false? (Suspect-Facts-area sfacts))
                (false? (Murder-Facts-area mfacts)))
      (unless (= (Suspect-Facts-area sfacts) (Murder-Facts-area mfacts))
        (set-Suspect-Facts-eliminated?! sfacts #t)))
    
    ;; Filter for murder side
    (unless (or (false? (Suspect-Facts-side sfacts))
                (false? (Murder-Facts-side mfacts)))
      (unless (= (Suspect-Facts-side sfacts) (Murder-Facts-side mfacts))
        (set-Suspect-Facts-eliminated?! sfacts #t)))
    
    ;; Filter for murder ABC
    (unless (or (false? (Suspect-Facts-ABC sfacts))
                (false? (Murder-Facts-ABC mfacts)))
      (unless (= (Suspect-Facts-ABC sfacts) (Murder-Facts-ABC mfacts))
        (set-Suspect-Facts-eliminated?! sfacts #t)))

    (unless (prints? s#)
      (set-Suspect-Facts-eliminated?! sfacts #t))))

(define (normalize-eliminatted cfacts)
  (define mfacts (Case-Facts-murder cfacts))
  (define places (Case-Facts-places cfacts))
  (define suspects (Case-Facts-suspects cfacts))

  (define (place-full? p#)
    (define pfacts (hash-ref places p#))
    (define alen (length (Place-Facts-alibis pfacts)))
    (cond
      [(= 4 alen) #t]
      [else
       (and (eq? p# (Murder-Facts-3-sus-place mfacts))
            (= 3 alen))]))
  
  (define ps (remove* (for/fold ([acc empty])
                                ([(p# pfacts) places])
                        (values (if (Place-Facts-eliminated? pfacts)
                                    (cons p# acc)
                                    acc)))
                      PLACE-IDS))
  ;; When we have only 1 place remaining it must be the murderer's place.
  (when (= 1 (length ps))
    (define p# (first ps))
    (define pfacts (hash-ref places p#))
    (set-Murder-Facts-place! mfacts p#)
    (if (Murder-Facts-area mfacts)
        (set-Place-Facts-area! pfacts (Murder-Facts-area mfacts))
        (set-Murder-Facts-area! mfacts (Place-Facts-area pfacts)))
    (if (Murder-Facts-side mfacts)
        (set-Place-Facts-side! pfacts (Murder-Facts-side mfacts))
        (set-Murder-Facts-side! mfacts (Place-Facts-side pfacts)))
    ;; If he place is full, eliminate all suspects not at the murderer's place.
    (when (place-full? p#)
      (for ([(s# sfacts) suspects])
        (unless (eq? p# (Suspect-Facts-place sfacts))
          (set-Suspect-Facts-eliminated?! sfacts #t))))))

(define (filter-facts cfacts)
  (define suspects (Case-Facts-suspects cfacts))
  (define (ferror name e) (error (format "~a: ~a" name (exn-message e))))
  ;; Initialize eliminated suspects to allow for changeed answers to question #13 and #14.
  (for ([(s# sfacts) suspects])
    (set-Suspect-Facts-eliminated?! sfacts #f))
  (with-handlers ([exn:fail? (λ (e) (λ (e) (ferror 'normalize-suspect-alibis)))])
    (normalize-suspect/alibis cfacts))
  (with-handlers ([exn:fail? (λ (e) (λ (e) (ferror 'normalize-places)))])
    (normalize-places cfacts))
  (with-handlers ([exn:fail? (λ (e) (λ (e) (ferror 'filter-murder)))])
    (filter-murder cfacts))
  (with-handlers ([exn:fail? (λ (e) (λ (e) (ferror 'normalize-eliminated)))])
    (normalize-eliminatted cfacts))

  ;; Set the ques and pool of suspects.
  (define-values (ques pool)
    (for/fold ([ques empty]
               [pool empty])
              ([(id sfacts) suspects])
      (values (if (Suspect-Facts-questioned? sfacts)
                  ques
                  (cons id ques))
              (if (Suspect-Facts-eliminated? sfacts)
                  pool
                  (cons id pool)))))
  (set-Case-Facts-ques! cfacts (sort ques <))
  (set-Case-Facts-pool! cfacts (sort pool <)))
