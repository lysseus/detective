#lang racket

;;;
;;; console-gui.rkt
;;;
;;; Gui for drawing the game console.

(provide draw-console/card-panel)

(require 2htdp/image
         "common.rkt"
         "common-gui.rkt"
         "suspect-cards-gui.rkt")

(define MAX-WIDTH 800)
(define-values (ACT-WIDTH ACT-HEIGHT)
  (let ()
    (define-values (w h)
      (for/fold ([w empty]
                 [h empty])
                ([v '("SUSPECT" "QUESTION" "ACCUSE" "ENTER" "END TURN")])
        (define img (text v (current-font-size) 'white))
        (values (cons (image-width img) w) (cons (image-height img) h))))
    (values (apply max w) (apply max h))))
(define SQ-PAD 5)

(define SCR-FONT-COLOR 'white)
(define SQ-COLOR 'black)
(define SIDES-IMG-COLOR 'lightorange)
(define AREAS-IMG-COLOR 'lightblue)
(define PLACES-IMG-COLOR 'yellow)
(define ALIBIS-IMG-COLOR 'lightgreen)
(define BO-COLOR 'transparent)
(define CONSOLE-COLOR 'white)

(define ALIBIS-TXT (text "\tSuspect(s)" (current-font-size) 'black))
(define SQ (square (+ (* 2 SQ-PAD) (current-font-size)) 'solid 'transparent))
(define ALIBIS-IMG  
  (above/align "left"
               (beside (rectangle (* 2 (image-width SQ)) (image-height SQ) 'solid ALIBIS-IMG-COLOR)
                       (rectangle (image-width SQ) (image-height SQ) 'solid 'transparent)
                       (rectangle (* 2 (image-width SQ)) (image-height SQ) 'solid ALIBIS-IMG-COLOR))
               (overlay/align "left" "center"                 
                              ALIBIS-TXT
                              (rectangle MAX-WIDTH
                                         (* 2 (image-height ALIBIS-TXT))
                                         'solid ALIBIS-IMG-COLOR))))
(define ALIBIS-LBL (overlay/align "right" "top"
                                  ALIBIS-IMG
                                  (rectangle (+ 5 (image-width ALIBIS-IMG))
                                             (+ 5 (image-height ALIBIS-IMG))
                                             'solid BO-COLOR)))

(define SUSPECT-TXT (text "Suspect Alibis" (current-font-size) 'black))
(define SUSPECT-IMG
  (overlay/align "left" "center"
                 ALIBIS-TXT
                 (rectangle MAX-WIDTH
                            (* 2 (image-height SUSPECT-TXT))
                            'solid ALIBIS-IMG-COLOR)))
(define SUSPECT-LBL (overlay/align "right" "top"
                                   SUSPECT-IMG
                                   (rectangle (+ 5 (image-width ALIBIS-IMG))
                                              (+ 5 (image-height ALIBIS-IMG))
                                              'solid BO-COLOR)))

(define PLACES-TXT-1     
  (above/align "left"
               (text (format "~a ~a" 'A (hash-ref PLACE-NAMES 'A)) (current-font-size) 'black)
               (text (format "~a ~a" 'b (hash-ref PLACE-NAMES 'b)) (current-font-size) 'black)
               (text (format "~a ~a" 'C (hash-ref PLACE-NAMES 'C)) (current-font-size) 'black)))
(define PLACES-TXT-2
  (above/align "left"
               (text (format "~a ~a" 'd (hash-ref PLACE-NAMES 'd)) (current-font-size) 'black)
               (text (format "~a ~a" 'E (hash-ref PLACE-NAMES 'E)) (current-font-size) 'black)
               (text (format "~a ~a" 'F (hash-ref PLACE-NAMES 'F)) (current-font-size) 'black)))
(define PLACES-TXT
  (above
   (text "PLACES:" (quotient (current-font-size) 3) 'black)
   (beside/align "top"
                 (overlay PLACES-TXT-1
                          (rectangle (+ 10 (image-width PLACES-TXT-1))
                                     (image-height PLACES-TXT-1)
                                     'solid 'transparent))
                 (rectangle 40 (image-height PLACES-TXT-1) 'solid 'transparent)
                 (overlay PLACES-TXT-2
                          (rectangle (+ 10 (image-width PLACES-TXT-2))
                                     (image-height PLACES-TXT-2)
                                     'solid 'transparent)))))
(define PLACES-IMG (overlay/align "left" "center"
                                  PLACES-TXT
                                  (rectangle MAX-WIDTH
                                             (+ (* 2 SQ-PAD) (image-height PLACES-TXT))

                                             'solid PLACES-IMG-COLOR)))
(define A+P-IMG
  (above/align "left"
               (beside
                (rectangle (+ SQ-PAD (current-font-size))
                           (image-height ALIBIS-LBL)
                           'solid PLACES-IMG-COLOR)
                ALIBIS-LBL)
               PLACES-IMG))
(define PLACES-LBL
  (overlay/align "right" "top"
                 A+P-IMG
                 (rectangle (+ (* 2 SQ-PAD) (current-font-size) (image-width A+P-IMG))
                            (+ SQ-PAD (image-height A+P-IMG))
                            'solid BO-COLOR)))

(define AREAS-TXT (above
                   (text "AREAS:" (quotient (current-font-size) 3) 'black)
                   (text (format "~a ~a\t~a ~a\t~a ~a"
                                 3 (hash-ref AREA-NAMES 3)
                                 4 (hash-ref AREA-NAMES 4)
                                 5 (hash-ref AREA-NAMES 5))
                         (current-font-size) 'black)))
(define AREAS-IMG
  (overlay/align "left" "center"
                 (overlay AREAS-TXT
                          (rectangle (+ (* 2 SQ-PAD) (image-width AREAS-TXT))
                                     (image-height AREAS-TXT)
                                     'solid 'transparent))
                 (rectangle MAX-WIDTH
                            (* 2 (image-height AREAS-TXT))
                            'solid AREAS-IMG-COLOR)))
(define A+P+A-IMG
  (above/align "left"
               (beside
                (rectangle (+ SQ-PAD (current-font-size))
                           (image-height PLACES-LBL)
                           'solid AREAS-IMG-COLOR)
                PLACES-LBL)
               AREAS-IMG))
(define AREAS-LBL
  (overlay/align "right" "top"
                 A+P+A-IMG
                 (rectangle (+ SQ-PAD (image-width A+P+A-IMG))
                            (+ SQ-PAD (image-height A+P+A-IMG))
                            'solid BO-COLOR)))

(define SIDES-TXT (above
                   (text "SIDES:" (quotient (current-font-size) 3) 'black)
                   (text (format "~a ~a\t~a ~a"
                                 1 (hash-ref SIDE-NAMES 1)
                                 2 (hash-ref SIDE-NAMES 2))
                         (current-font-size) 'black)))
(define SIDES-IMG
  (overlay/align "left" "center"
                 (overlay SIDES-TXT
                          (rectangle (+ (* 2 SQ-PAD) (image-width SIDES-TXT))
                                     (image-height SIDES-TXT)
                                     'solid 'transparent))
                 (rectangle  MAX-WIDTH
                             (* 2 (image-height SIDES-TXT))
                             'solid SIDES-IMG-COLOR)))
(define S+A+P+A-IMG
  (above/align "left"
               (beside
                (rectangle (+ SQ-PAD (current-font-size))
                           (image-height AREAS-LBL)
                           'solid SIDES-IMG-COLOR)
                AREAS-LBL)
               SIDES-IMG))
(define SIDES-LBL
  (overlay/align "right" "top"
                 S+A+P+A-IMG
                 (rectangle (+ SQ-PAD (image-width S+A+P+A-IMG))
                            (+ SQ-PAD (image-height S+A+P+A-IMG))
                            'solid BO-COLOR)))

(define (draw-screen-sq v)
  (overlay (text (~a v) (current-font-size) SCR-FONT-COLOR)
           (square (+ (* 2 SQ-PAD) (current-font-size)) 'solid 'transparent)))

(define (draw-screen scr)
  (define img
    (beside (draw-screen-sq (vector-ref scr 0))
            (draw-screen-sq (vector-ref scr 1))
            (draw-screen-sq "")
            (draw-screen-sq (vector-ref scr 2))
            (draw-screen-sq (vector-ref scr 3))
            (draw-screen-sq (vector-ref scr 4))
            (draw-screen-sq "")
            (draw-screen-sq (vector-ref scr 5))
            (draw-screen-sq (vector-ref scr 6))))
  (overlay img
           (rectangle (image-width img)
                      (+ (* 2 SQ-PAD) (image-height img))
                      'solid SQ-COLOR)))

(define SCREEN-WIDTH 500)

(define (draw-screen-panel scr)
  (define img (above/align "left"
                           (draw-screen scr)
                           SIDES-LBL))
  (crop/align "left" "top"
              SCREEN-WIDTH (image-height img)
              img))

(define (draw-act-key ke act)
  (define k? (cond
               [(string=? ke act) #t]
               [else #f]))
  (define txt-img (text
                   (cond
                     [(string=? act SUSPECT-KEY-VAL) "SUSPECT"]
                     [(string=? act QUESTION-KEY-VAL) "QUESTION"]
                     [(string=? act ACCUSE-KEY-VAL) "ACCUSE"]
                     [(string=? act ENTER-KEY-VAL) "ENTER"]
                     [(string=? act END-TURN-KEY-VAL) "END TURN"]) (current-font-size) 'white))
  (define key-img (rectangle (+ 5 ACT-WIDTH) (* 2 (+ 5 ACT-HEIGHT)) 'solid
                             (if k? 'red 'blue)))
  (overlay txt-img
           key-img
           (rectangle (+ 2 (image-width key-img))
                      (+ 2 (image-height key-img)) 'outline (if k? 'black 'transparent))
           (rectangle (+ 20 (image-width key-img))
                      (+ 5 (image-height key-img))'solid 'transparent)))

(define (draw-num-key ke num)
  (define k (string->number ke))
  (define k? (cond
               [(and (number? k) (= k num)) #t]
               [else #f]))
  
  (define txt-img (text (~a num) (current-font-size) 'white))
  (define key-img (square (* 2 (+ 5 (current-font-size))) 'solid (if k? 'red 'blue)))
  (overlay txt-img
           key-img
           (square (+ 2 (image-width key-img)) 'outline (if k? 'black 'transparent))
           (square (+ 5 (image-width key-img)) 'solid 'transparent)))

(define (draw-pwr-key ke pwr)
  (define k? (cond
               [(string=? ke pwr) #t]
               [else #f]))
  
  (define txt-img (text (~a (cond
                              [(string=? pwr ON-KEY-VAL) "ON"]
                              [(string=? pwr OFF-KEY-VAL) "OFF"]))
                        (current-font-size) 'white))
  (define key-img (square (* 2 (+ 5 (current-font-size))) 'solid (if k? 'red 'blue)))
  (overlay txt-img
           key-img
           (square (+ 2 (image-width key-img)) 'outline (if k? 'black 'transparent))
           (square (+ 5 (image-width key-img)) 'solid 'transparent)))

(define (draw-pwrpad ke)
  (beside
   (draw-pwr-key ke " ")
   (draw-pwr-key ke "escape")))

(define (draw-numpad ke)
  (beside/align "top"
                (above
                 (draw-num-key ke 1)
                 (draw-num-key ke 4)
                 (draw-num-key ke 7))
                (above
                 (draw-num-key ke 2)
                 (draw-num-key ke 5)
                 (draw-num-key ke 8))
                (above
                 (draw-num-key ke 3)
                 (draw-num-key ke 6)
                 (draw-num-key ke 9)
                 (draw-num-key ke 0))))

(define (draw-keypad ke)
  (define kp
    (beside/align "top"
                  (above/align "left"
                               (draw-numpad ke)
                               (draw-pwrpad ke))
                  (above/align "left"
                               (draw-act-key ke SUSPECT-KEY-VAL)
                               (draw-act-key ke QUESTION-KEY-VAL)
                               (draw-act-key ke ACCUSE-KEY-VAL)
                               (draw-act-key ke ENTER-KEY-VAL)
                               (draw-act-key ke END-TURN-KEY-VAL))))
  (overlay kp (rectangle (+ (* 20 SQ-PAD) (image-width kp))
                         (image-height kp)
                         'solid 'transparent)))

(define (draw-console scr ke)
  (define c (beside/align "top"
                          (draw-screen-panel scr)
                          (draw-keypad ke)))
  (overlay c
           (rectangle (image-width c)
                      (image-height c)
                      'solid CONSOLE-COLOR)))

(define keys-lbl (let* ([FONT-SIZE (current-font-size)]
                        [FONT-COLOR (current-font-color)]
                        [hdr (text "KEYS" FONT-SIZE FONT-COLOR)]
                        [t1 (text "Suspect" FONT-SIZE FONT-COLOR)]
                        [t2 (text "Question" FONT-SIZE FONT-COLOR)]
                        [t3 (text "Accuse" FONT-SIZE FONT-COLOR)]
                        [t4 (text "End Turn" FONT-SIZE FONT-COLOR)]
                        [t5 (text "Case Facts sheet" FONT-SIZE FONT-COLOR)]
                        [spc (text "\t" FONT-SIZE FONT-COLOR)]
                        [k1 (text "S" FONT-SIZE FONT-COLOR)]
                        [k2 (text "Q" FONT-SIZE FONT-COLOR)]
                        [k3 (text "A" FONT-SIZE FONT-COLOR)]
                        [k4 (text "T" FONT-SIZE FONT-COLOR)]
                        [k5 (text "F" FONT-SIZE FONT-COLOR)])
                   (scale .6 (double-frame
                              (above
                               hdr
                               (beside/align "top"
                                             (above/align "left" t1 t2 t3 t4 t5)
                                             spc
                                             (above/align "left" k1 k2 k3 k4 k5)))))))

(define (draw-console/card-panel scr evt s#)
  (parameterize ([current-font-size (current-font-size)]
                 [current-font-color (current-font-color)])
    (above/align "right"
                 (double-frame
                  (beside/align "top"                     
                                (draw-console scr evt)
                                (draw-card-panel evt s#)))
                 (λspacer)
                 keys-lbl)))
