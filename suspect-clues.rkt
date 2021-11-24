#lang racket

;;;
;;; SUSPECT CLUES
;;
;;; Creates the Suspect Clues for Racket Detective.
;;;

(provide (struct-out Suspect-Clues)
         make-clues)

(require "common.rkt"
         "murder-info.rkt")

(struct Suspect-Clues (place area side alibis) #:mutable #:transparent)

(define/contract (distribute-info p a s n1 n2 n3 (n4 0))
  (->i ([p symbol?]
        [a (integer-in 3 5)]
        [s (integer-in 1 2)]
        [n1 (integer-in 0 2)]
        [n2 (integer-in 0 2)]
        [n3 (integer-in 0 2)]
        [n4 (integer-in 0 2)])
       ()
       #:pre (n1 n2 n3 n4) (= 3 (+ n1 n2 n3 n4))
       (result any/c))
  (define lst (shuffle (range 3)))
  (define v1 (sort (take lst n1) <))
  (define v2 (sort (take (remove* v1 lst) n2) <))
  (define v3 (sort (take (remove* (append v1 v2) lst) n3) <))
  (define v4 (sort (take (remove* (append v1 v2 v3) lst) n4) <))
  (define ans (list v1 v2 v3 v4))
  (define result (map (λ (v) (for/list ([n (range 3)])
                               (if (member n v) n #f)))
                      ans))
  (map (λ (v) (map (λ (n) (case n
                            [(0) p]
                            [(1) a]
                            [(2) s]
                            [else n])) v)) result))

(define (distribute-alibis-3-1 vs)
  (list (take vs 2)
        (take (drop vs 1) 2)
        (take (drop vs 2) 1)))

(define (distribute-alibis-3-2 vs)
  (list (take vs 3)
        (take (drop vs 1) 1)
        (take (drop vs 2) 1)))

(define (distribute-alibis-4-1 vs)
  (list (take vs 2)
        (take (drop vs 1) 2)
        (take (drop vs 2) 2)
        (take (drop vs 3) 1)))

(define (distribute-alibis-4-2 vs)
  (list (take vs 3)
        (take (drop vs 1) 1)
        (take (drop vs 2) 2)
        (take (drop vs 3) 1)))

(define (distribute-alibis n s1 s2 s3 (s4 0))
  (cond
    ;; 3 suspect distribution
    [(zero? s4)
     (define vs (shuffle (list s1 s2 s3)))
     (if (= 1 n)         
         (distribute-alibis-3-1 vs)
         (distribute-alibis-3-2 vs))]
    ;; 4 suspect distribution
    [else
     (define vs (shuffle (list s1 s2 s3 s4)))
     (if (= 1 n)         
         (distribute-alibis-4-1 vs)
         (distribute-alibis-4-2 vs))]))

(define (distribute-clues p a s s1 s2 s3 s4)
  (define n (random 1 3))
  (define alibis (sort (distribute-alibis n s1 s2 s3 s4)
                       < #:key (λ (v) (length v))))
  (define anums (map (λ (v) (length v)) alibis))      
  (define infos (case (random 1 3)
                  [(1) (distribute-info p a s 2 1 0 0)]
                  [else (distribute-info p a s 1 1 1 0)]))
  (define inums (map (λ (v) (length (remove* '(#f) v))) infos))  
  (for/list ([alibis alibis]
             [info infos])
    (cons (first alibis)
          (Suspect-Clues (first info)
                         (second info)
                         (third info)
                         (rest alibis)))))

(define (make-clues info)
  (define victim-place-id (Murder-Info-victim-place info))
  (define 3-sus-place-id (Murder-Info-3-sus-place info))
  (define places (Murder-Info-places info))
  (define suspects (Murder-Info-suspects info))
  (define CLUES (make-hash))
  (define ans
    (for/fold ([acc empty])
              ([p# (remove victim-place-id PLACE-IDS)])
      (define pfacts (hash-ref places p#))
      (define alibis (Place-Info-suspects pfacts))
      (values (append (distribute-clues p#
                                        (Place-Info-area pfacts)
                                        (Place-Info-side pfacts)
                                        (first alibis)
                                        (second alibis)
                                        (third alibis)
                                        (if (eq? p# 3-sus-place-id)
                                            0
                                            (fourth alibis)))
                      acc))))    
  (make-hash ans))
