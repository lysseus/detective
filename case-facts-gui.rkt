#lang racket

;;;
;;; CASE FACTS GUI
;;;

(provide draw-case-facts)

(require 2htdp/image
         "case-facts.rkt"
         "common.rkt"
         "common-gui.rkt")

(define pen         (make-pen "darkred" 2 "solid" "round" "round"))

(define SUSPECT-NAME-IMGS/ID-PERIOD (text-img-hash SUSPECT-NAMES KEY-VAL-MASK/PERIOD))
(define PLACE-NAME-IMGS/ID-PERIOD   (text-img-hash PLACE-NAMES KEY-VAL-MASK/PERIOD))
(define AREA-NAME-IMGS/ID-PERIOD    (text-img-hash AREA-NAMES KEY-VAL-MASK/PERIOD))
(define SIDE-NAME-IMGS/ID-PERIOD    (text-img-hash SIDE-NAMES KEY-VAL-MASK/PERIOD))
(define SEX-NAME-IMGS               (text-img-hash SEX-NAMES VAL-MASK))
(define WEAPON-NAME-IMGS            (text-img-hash WEAPON-NAMES VAL-MASK))
(define PRINTS-NAME-IMGS            (text-img-hash PRINTS-NAMES VAL-MASK))
(define BOOLEAN-NAME-IMGS           (text-img-hash BOOLEAN-NAMES VAL-MASK
                                                   #:undefined-val "___"))

(define victim-lbl (text "Victim: " (current-font-size) (current-font-color)))
(define murder-lbl (text "MURDER FACTS: " (current-font-size) (current-font-color)))
(define victim-place-lbl (text "Place: " (current-font-size) (current-font-color)))
(define (draw-victim-facts facts)
  (define id (Victim-Facts-id facts))
  (define id/name (hash-ref SUSPECT-NAME-IMGS/ID-PERIOD id))
  (define place-id (Victim-Facts-place-id facts))
  (define place-id/name (hash-ref PLACE-NAME-IMGS/ID-PERIOD place-id))
  (beside victim-lbl
          id/name
          (λspacer)
          victim-place-lbl
          place-id/name))

(define sex-lbl (text "Sex: " (current-font-size) (current-font-color)))
(define (draw-murder-sex facts)
  (define img (hash-ref SEX-NAME-IMGS (Murder-Facts-sex facts)))
  (beside sex-lbl img))
(define place-lbl (text "Place: " (current-font-size) (current-font-color)))
(define (draw-murder-place facts)
  (define img (hash-ref PLACE-NAME-IMGS/ID-PERIOD (Murder-Facts-place facts)))
  (beside place-lbl img))
(define area-lbl (text "Area: " (current-font-size) (current-font-color)))
(define (draw-murder-area facts)
  (define img (hash-ref AREA-NAME-IMGS/ID-PERIOD (Murder-Facts-area facts)))
  (beside area-lbl img))
(define side-lbl (text "Side: " (current-font-size) (current-font-color)))
(define (draw-murder-side facts)
  (define img (hash-ref SIDE-NAME-IMGS/ID-PERIOD (Murder-Facts-side facts)))
  (beside side-lbl img))
(define 3-sus-place-lbl (text "3 Suspects Place: " (current-font-size) (current-font-color)))
(define (draw-murder-3-sus-place facts)
  (define img (hash-ref PLACE-NAME-IMGS/ID-PERIOD (Murder-Facts-3-sus-place facts)))
  (beside 3-sus-place-lbl img))
(define ABC-lbl (text "ABC?: " (current-font-size) (current-font-color)))
(define (draw-murder-ABC facts)
  (define img (hash-ref BOOLEAN-NAME-IMGS (Murder-Facts-ABC facts)))
  (beside ABC-lbl img))
(define weapon-lbl (text "Weapon: " (current-font-size) (current-font-color)))
(define (draw-murder-weapon facts)
  (define img (hash-ref WEAPON-NAME-IMGS (Murder-Facts-weapon facts)))
  (beside weapon-lbl img))
(define .38-place-lbl (text ".38 Place: " (current-font-size) (current-font-color)))
(define (draw-murder-.38-place facts)
  (define img (hash-ref PLACE-NAME-IMGS/ID-PERIOD (Murder-Facts-.38-place facts)))
  (beside .38-place-lbl img))
(define .45-place-lbl (text ".45 Place: " (current-font-size) (current-font-color)))
(define (draw-murder-.45-place facts)
  (define img (hash-ref PLACE-NAME-IMGS/ID-PERIOD (Murder-Facts-.45-place facts)))
  (beside .45-place-lbl img))
(define .38-prints-lbl (text ".38 Prints: " (current-font-size) (current-font-color)))
(define (draw-murder-.38-prints facts)
  (define img (hash-ref PRINTS-NAME-IMGS (Murder-Facts-.38-prints facts)))
  (beside .38-prints-lbl img))
(define .45-prints-lbl (text ".45 Prints: " (current-font-size) (current-font-color)))
(define (draw-murder-.45-prints facts)
  (define img (hash-ref PRINTS-NAME-IMGS (Murder-Facts-.45-prints facts)))
  (beside .45-prints-lbl img))
(define (draw-murder-facts facts)
  (define img-1 (beside/align "center"                              
                              (draw-murder-place facts)
                              (λspacer)
                              (draw-murder-area facts)
                              (λspacer)
                              (draw-murder-side facts)))
  (define img-2 (beside/align "center"
                              (draw-murder-sex facts)
                              (λspacer)
                              (draw-murder-3-sus-place facts)
                              (λspacer)
                              (draw-murder-ABC facts)))
  (define img-3 (beside/align "center"
                              (draw-murder-weapon facts)
                              (λspacer)
                              (beside/align "center"
                                            (above/align "left"
                                                         (draw-murder-.38-place facts)
                                                         (draw-murder-.45-place facts))
                                            (λspacer)
                                            (above/align "left"
                                                         (draw-murder-.38-prints facts)
                                                         (draw-murder-.45-prints facts)))))  
  (above/align "left" murder-lbl img-1 img-2 (λspacer) img-3))

(define section-1-lbl (above (text "1" (* 4 (current-font-size)) (current-font-color))
                               (text "WHAT?" (quotient (current-font-size) 2) (current-font-color))))
(define (draw-section-1 case-facts)    
  (beside/align "center"
                                section-1-lbl
                                (λspacer)
                                (above/align "left"
                                             (draw-victim-facts (Case-Facts-victim case-facts))
                                             (λspacer)
                                             (draw-murder-facts (Case-Facts-murder case-facts)))))

(define (draw-place-place place-id)
  (hash-ref PLACE-NAME-IMGS/ID-PERIOD place-id))
(define (draw-place-area place-facts)
  (define id (Place-Facts-area place-facts))
  (define img (hash-ref AREA-NAME-IMGS/ID-PERIOD id))
  (beside/align "center" area-lbl img))
(define (draw-place-side place-facts)
  (define id (Place-Facts-side place-facts))
  (define img (hash-ref SIDE-NAME-IMGS/ID-PERIOD id))
  (beside/align "center" side-lbl img))
(define alibi-box (square (* 2 (current-font-size)) 'outline (current-font-color)))
(define (draw-place-alibis place-facts)
  (define alibis (take (append (sort (Place-Facts-alibis place-facts) <)
                               '("" "" "" "")) 4))
  (define imgs (for/list ([n alibis])
                 (overlay (text (~a n) (current-font-size) (current-font-color))
                          alibi-box)))
  (above (beside (first imgs) (second imgs))
         (beside (third imgs) (fourth imgs))))
(define weapon-box (rectangle (* 2 (image-width alibi-box))
                              (image-height alibi-box)
                              'outline (current-font-color)))
(define (draw-place-weapon place-facts)
  (define id (Place-Facts-weapon place-facts))
  (define img (hash-ref WEAPON-NAME-IMGS id))
  (overlay img weapon-box))
(define (draw-place place-id place-facts)
  (define img
    (above/align "center"
                 (above/align "left"
                              (draw-place-place place-id)
                              (draw-place-area place-facts)
                              (draw-place-side place-facts))
                 (above/align "left"
                              (draw-place-alibis place-facts)
                              (draw-place-weapon place-facts))))
  (define img-box-1 (rectangle (+ (* 2 (current-font-size)) (image-width img))
                          (+ (* 2 (current-font-size)) (image-height img))
                          'outline 'transparent))  
  (define p (overlay img img-box-1))
  (if (Place-Facts-eliminated? place-facts)
      (add-line
       (add-line p 0 0 (image-width p) (image-height p) pen)
       0 (image-width p) (image-height p) 0 pen)
      p))

(define (draw-place-facts facts)
  (define img
    (above
   (apply beside
          (for/list ([id (take PLACE-IDS 3)])
            (define place-facts (hash-ref facts id))
            (draw-place id place-facts)))
   (apply beside
          (for/list ([id (drop PLACE-IDS 3)])
            (define place-facts (hash-ref facts id))
            (draw-place id place-facts)))))
  (define img-box (rectangle (+ (* 2 (current-font-size)) (image-width img))
                          (+ (* 2 (current-font-size)) (image-height img))
                          'outline (current-font-color)))
  (define img-box-2 (rectangle (+ 2 (image-width img-box))
                               (+ 2 (image-height img-box))
                               'outline (current-font-color)))
  img)

(define section-2-lbl (above (text "2" (* 4 (current-font-size)) (current-font-color))
                             (text "WHERE?" (quotient (current-font-size) 2) (current-font-color))))
(define (draw-section-2 case-facts)  
  (beside/align "center"
                              section-2-lbl
                              (λspacer)
                              (draw-place-facts (Case-Facts-places case-facts))))

(define (draw-suspect id suspect-facts)
  (define img
    (beside
     (hash-ref SUSPECT-NAME-IMGS/ID-PERIOD id)
     (λspacer)
     (text (~a (if (false? (Suspect-Facts-place suspect-facts))
                   "_"
                   (Suspect-Facts-place suspect-facts))) (current-font-size) (current-font-color))
     (λspacer)
     (text (~a (if (false? (Suspect-Facts-area suspect-facts))
                   "_"
                   (Suspect-Facts-area suspect-facts))) (current-font-size) (current-font-color))
     (λspacer)
     (text (~a (if (false? (Suspect-Facts-side suspect-facts))
                   "_"
                   (Suspect-Facts-side suspect-facts))) (current-font-size) (current-font-color))
     (λspacer)
     (text (~a (if (false? (Suspect-Facts-ABC suspect-facts))
                   "___"
                   (boolean->name (Suspect-Facts-ABC suspect-facts)))) (current-font-size) (current-font-color))
     (λspacer)
     (λspacer)
     (text (~a (cond
                 [(false? (Suspect-Facts-at-weapon-place suspect-facts))
                      "___"]
                 [(= (name->boolean 'yes) (Suspect-Facts-at-weapon-place suspect-facts))
                  "Yes"]
                 [else "No "])) (current-font-size) (current-font-color))
     (λspacer)
     (text (~a (if (empty? (Suspect-Facts-alibis suspect-facts))
                   "(________)"
                   (Suspect-Facts-alibis suspect-facts))) (current-font-size) (current-font-color))))
  (if (Suspect-Facts-eliminated? suspect-facts)
      (add-line img
                0
                (quotient (image-height img) 2)
                (image-width img)
                (quotient (image-height img) 2)
                pen)
      img))

(define sn-img (hash-ref SUSPECT-NAME-IMGS/ID-PERIOD 1))
(define sn-width (image-width sn-img))
(define sn-height (image-height sn-img))
(define sn-txt (text "Suspect" (current-font-size) (current-font-color)))
(define sn-lbl (overlay sn-txt (rectangle sn-width sn-height 'solid 'transparent)))
(define p-lbl (text "P" (current-font-size) (current-font-color)))
(define a-lbl (text "A" (current-font-size) (current-font-color)))
(define s-lbl (text "S" (current-font-size) (current-font-color)))
(define s-ABC-lbl (text "ABC" (current-font-size) (current-font-color)))
(define @W?-lbl (text "@W?" (current-font-size) (current-font-color)))
(define alibis-lbl (text "Alibis" (current-font-size) (current-font-color)))

(define (draw-suspect-facts facts)
  (above/align "left"
               (beside sn-lbl
                       (λspacer)
                       p-lbl
                       (λspacer)
                       a-lbl
                       (λspacer)
                       s-lbl
                       (λspacer)
                       s-ABC-lbl
                       (λspacer)
                       @W?-lbl
                       (λspacer)
                       alibis-lbl)
               (apply above/align "left"
                      (for/list ([id SUSPECT-IDS])
                        (draw-suspect id (hash-ref facts id))))))

(define section-3-lbl (above (text "3" (* 4 (current-font-size)) (current-font-color))
                             (text "WHO?" (quotient (current-font-size) 2) (current-font-color))))
(define (draw-section-3 case-facts)  
  (beside/align "center"
                              section-3-lbl
                              (λspacer)
                              (draw-suspect-facts (Case-Facts-suspects case-facts))))

(define questionable-lbl (text "Number of Suspects Left to Question: " (current-font-size) (current-font-color)))
(define (draw-ques suspects)
  (above/align "left"
               (beside/align "center"
                             questionable-lbl
                             (text (~a (length suspects)) (current-font-size) (current-font-color)))
               (λspacer)
               (text (~a suspects) (current-font-size) (current-font-color))))

(define accusable-lbl (text "Number of Likely Suspects: " (current-font-size) (current-font-color)))
(define (draw-pool suspects)
  (above/align "left"
               (beside/align "center"
                             accusable-lbl
                             (text (~a (length suspects)) (current-font-size) (current-font-color)))
               (λspacer)
               (text (~a suspects) (current-font-size) (current-font-color))))

(define section-4-lbl (above (text "4" (* 4 (current-font-size)) (current-font-color))
                             (text "ACCUSE!" (quotient (current-font-size) 2) (current-font-color))))
(define (draw-section-4 case-facts)  
  (beside/align "center"
                              section-4-lbl
                              (λspacer)
                              (above/align "left"
                                           (draw-ques (Case-Facts-ques case-facts))
                                           (λspacer)
                                           (λspacer)
                                           (draw-pool (Case-Facts-pool case-facts)))))

(define keys-lbl (let* ([FONT-SIZE (current-font-size)]
                        [FONT-COLOR (current-font-color)]
                        [hdr (text "KEYS" (current-font-size) (current-font-color))]
                        [t1 (text "Console" (current-font-size) (current-font-color))]                        
                        [spc (text "\t" (current-font-size) (current-font-color))]
                        [k1 (text "C" (current-font-size) (current-font-color))])
                   (scale .6 (double-frame
                              (above
                               hdr
                               (beside/align "top"
                                             t1
                                             spc
                                             k1))))))

(define (draw-case-facts case-facts)
  (parameterize ([current-font-size (current-font-size)]
                 [current-font-color (current-font-color)])
    (define-values (s1-img s2-img s3-img s4-img)
      (let* ([s1-img (draw-section-1 case-facts)]
             [s2-img (draw-section-2 case-facts)]
             [s3-img (draw-section-3 case-facts)]
             [s4-img (draw-section-4 case-facts)]
             [w-left (max (image-width s1-img) (image-width s2-img))]
             [h-left (+ (image-height s1-img) (image-height s2-img))]
             [w-right (max (image-width s3-img) (image-width s4-img))]
             [h-right (+ (image-height s3-img) (image-height s4-img))]
             [Δh (- h-left h-right)])
        (values (double-frame (overlay/align "left" "center"
                                             s1-img (rectangle w-left (image-height s1-img) 'solid 'transparent)))
                (double-frame (overlay/align "left" "center"
                                             s2-img (rectangle w-left (image-height s2-img) 'solid 'transparent)))
                (double-frame (overlay/align "left" "center"
                                             s3-img (rectangle w-right (image-height s3-img) 'solid 'transparent)))
                (double-frame (overlay/align "left" "top"
                                             s4-img (rectangle w-right (+ (image-height s4-img) Δh)
                                                               'solid 'transparent))))))
    (define img-left (above/align "left"
                                  s1-img
                                  s2-img))     
    (define img-right (above/align "left"
                                   s3-img
                                   s4-img))  
    (scale .9 (above/align "right"
                           (beside/align "top"
                                         img-left
                                         img-right)
                           (λspacer)
                           keys-lbl))))
