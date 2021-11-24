#lang racket

;;;
;;; COMMON GUI
;;;
;;; Provides bindings for common GUI elements.
;;;

(provide VAL-MASK
         KEY-VAL-MASK
         KEY-VAL-MASK/PERIOD
         current-font-size
         current-font-color
         text-img-hash
         λspacer
         double-frame)

(require 2htdp/image)

(define/contract current-font-size
  (parameter/c  (integer-in 1 255))
  (make-parameter 24))
(define/contract current-font-color
  (parameter/c  image-color?)
  (make-parameter 'black))
(define λspacer (thunk (square (current-font-size) 'solid 'transparent)))

(define VAL-MASK (λ (v) (format "~a" v)))
(define KEY-VAL-MASK (λ (k v) (format "~a ~a" k v)))
(define KEY-VAL-MASK/PERIOD (λ (k v) (format "~a. ~a" k v)))

(define (text-img-hash #:font-size (font-size (current-font-size))
                       #:font-color (font-color (current-font-color))
                       #:undefined-key (undefined-key #f)
                       #:undefined-val (undefined-val "")
                       txt-hash mask)
  (let* ([hsh (for/hash ([(k v) txt-hash])
                (define txt (cond
                              [(= 2 (procedure-arity mask))
                               (mask k v)]
                              [else (mask v)]))
                (values k (text txt font-size font-color)))]
         [w (apply max (map image-width (hash-values hsh)))]
         [h (apply max (map image-height (hash-values hsh)))])
    (define frame (rectangle w h 'solid 'transparent))
    (define undefined-img (text undefined-val font-size font-color))
    (for/hash ([k (cons undefined-key (hash-keys hsh))])
      (values k (overlay/align "left" "center"
                               (hash-ref hsh k undefined-img)
                               frame)))))


(define (double-frame img (clr (current-font-color)))
  (define padded-img (above (λspacer)
                            (beside (λspacer) img (λspacer))
                            (λspacer)))
  (define w (image-width padded-img))
  (define h (image-height padded-img))
  (overlay padded-img
           (rectangle (+ 2 w) (+ 2 h) 'outline clr)
           (rectangle (+ 4 w) (+ 4 h) 'outline clr)
           (rectangle (+ 6 w) (+ 6 h) 'outline clr)))
