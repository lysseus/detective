#lang racket

;;;
;;; Suspect Cards GUI.
;;;

(provide draw-card-panel)

(require 2htdp/image
         (only-in "common-gui.rkt" current-font-size current-font-color)
         "suspect-cards.rkt")

(define CARD-COLOR 'lightyellow)
(define BO-COLOR 'gold)
(define EMPTY-CARD (rectangle 525 200 'solid BO-COLOR))

(define (draw-questions ques)
  (apply above/align "left"
         (for/list ([q ques])
           (text (format "~a. ~a" q (hash-ref QUESTIONS q))
                 (current-font-size) (current-font-color)))))

(define (draw-suspect-card n)
  (define c (hash-ref SUSPECT-CARDS n #f))
  (cond
    [(false? c) EMPTY-CARD]
    [else
     (define name (Card-name c))
     (define role (Card-role c))
     (define status (Card-status c))
     (define ques (Card-ques c))
     (define card (above/align "left"
                               (beside
                                (overlay (text (~a n) (* 2 (current-font-size)) (current-font-color))
                                         (square (* 4 (current-font-size)) 'solid 'transparent))                                
                                (above/align "left"
                                             (text (~a name) (current-font-size) (current-font-color))
                                             (text (~a role) (current-font-size) (current-font-color))
                                             (cond
                                               [(number? status)
                                                (text (format "MARRIED TO ~a" (Card-name (hash-ref SUSPECT-CARDS status)))
                                                      (current-font-size) (current-font-color))]
                                               [else (text (~a status) (current-font-size)
                                                           (current-font-color))])))
                               (square (current-font-size) 'solid 'transparent)
                               (draw-questions ques)))
     (overlay card
              (rectangle
               (+ 15 (image-width card)) (+ 15 (image-height card))
               'solid CARD-COLOR)
              (rectangle
               (+ 20 (image-width card)) (+ 20 (image-height card))
               'solid BO-COLOR))]))

(define (draw-scroll-key ke val)
  (define k? (cond
               [(string=? ke val) #t]
               [else #f]))
  
  (define txt-img (text (if (string=? val "left") "<-" "->") (current-font-size) 'white))
  (define key-img (square (* 2 (+ 5 (current-font-size))) 'solid (if k? 'red 'blue)))
  (overlay txt-img
           key-img
           (square (+ 2 (image-width key-img)) 'outline (if k? 'black 'transparent))
           (square (+ 5 (image-width key-img)) 'solid 'transparent)))

(define (draw-keys-panel ke)
  (define left (draw-scroll-key ke "left"))
  (define right (draw-scroll-key ke "right"))
  (define buff (square (current-font-size) 'solid 'transparent))
  (define keys (beside left buff right))
  (define frame (rectangle (image-width keys)
                           (* 2 (image-height keys))
                           'solid 'transparent))
  (overlay keys frame))

(define (draw-card-panel ke n)
  (parameterize ([current-font-size 20]
                 [current-font-color 'black])
    (above/align "center"
                 (draw-suspect-card n)
                 (draw-keys-panel ke))))

