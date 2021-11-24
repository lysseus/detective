#lang racket

;;;
;;; RACKET DETECTIVE
;;;

(require 2htdp/image
         2htdp/universe
         (only-in racket/gui
                  play-sound)
         utils/states
         "common.rkt"
         "suspect-cards.rkt"
         "console-gui.rkt"
         "murder-info.rkt"
         "suspect-clues.rkt"
         "case-facts.rkt"
         "case-facts-gui.rkt"
         "case-facts-filters.rkt")

(struct world (console?
               level-max
               level-count
               state
               screen
               suspect-id
               question-id
               card-id
               murder-info
               suspect-clues
               case-facts) 
  #:mutable #:transparent)

;; For sound effects
(define SOUND-PATH "sounds/")
(define SOUND-INTRO (string-append SOUND-PATH "3beeps.wav"))
(define SOUND-MURDER (string-append SOUND-PATH "2shots-and-dirge.wav"))
(define SOUND-ERROR (string-append SOUND-PATH "single-beep.wav"))
(define SOUND-WRONG-ACCUSATION (string-append SOUND-PATH "wrong-accusation.wav"))
(define SOUND-WINNER (string-append SOUND-PATH "police-siren-hi-low.wav"))

;;---------------------------------------------------------------------------------------------
;; Task Procedures
;;---------------------------------------------------------------------------------------------

(define (clear-screen ws state kval)
  (screen-set! (world-screen ws) 'clear))

(define (screen-set! #:clear-first? (clear-first? #f)
                     scr fld (val ""))
  (define v (~a val))
  (unless (false? clear-first?) (vector-fill! scr ""))
  (cond
    [(eq? fld 'clear) (vector-fill! scr "")]
    [(eq? fld 'side) (vector-set! scr 0 v)]
    [(eq? fld 'area) (vector-set! scr 1 v)]
    [(eq? fld 'place) (vector-set! scr 2 v)]
    [(eq? fld 'yes)
     (vector-fill! scr "")
     (vector-set! scr 2 "Y")
     (vector-set! scr 3 "E")
     (vector-set! scr 4 "S")]
    [(eq? fld '?)
     (vector-fill! scr "0")]
    [(eq? fld 's1)
     (cond
       [(= 1 (string-length v))
        (vector-set! scr 3 "")
        (vector-set! scr 4 (~a v))]
       [(= 2 (string-length v))
        (vector-set! scr 3 (substring (~a val) 0 1))
        (vector-set! scr 4 (substring (~a val) 1 2))])]
    [(eq? fld 's2)
     (cond
       [(= 1 (string-length v))
        (vector-set! scr 5 "")
        (vector-set! scr 6 (~a v))]
       [(= 2 (string-length v))
        (vector-set! scr 5 (substring (~a val) 0 1))
        (vector-set! scr 6 (substring (~a v) 1 2))])]
    [(eq? fld 'no)
     (vector-fill! scr "")
     (vector-set! scr 5 "N")
     (vector-set! scr 6 "O")]
    [(eq? fld 'error)
     (play-sound SOUND-ERROR #t)
     (vector-fill! scr "")
     (vector-set! scr 0 "E")
     (vector-set! scr 1 "E")]))

(define (store-level ws state kval)
  (clear-screen ws state kval)
  (define scr (world-screen ws))
  (vector-set! scr 6 kval))

(define (store-number ws state kval)
  (define scr (world-screen ws))
  (define n1 (vector-ref scr 6))
  (vector-set! scr 5 (if (string=? n1 "0") "" n1))
  (vector-set! scr 6 kval))

(define (intro-game ws state kval)
  (clear-screen ws state kval)
  ;; Play 3 beeps intro.
  (play-sound SOUND-INTRO #t))

(define (beg-game ws state kval)
  (define scr (vector->list (world-screen ws)))
  (define lvl (string->number (apply string-append scr)))
  (cond
    [(and (natural? lvl) (< 0 lvl 4))
     (set-world-level-max! ws lvl)
     ;; Play 2 "silencer" gun shots and funeral dirge.  
     (play-sound SOUND-MURDER #t)
     (define info (make-Murder-Info))
     (define v (Murder-Info-victim-id info))
     (define p (Murder-Info-victim-place info))
     (define clues (make-clues info))
     (set-world-murder-info! ws info)
     (set-world-suspect-clues! ws clues)
     (set-world-card-id! ws v)
     (set-world-case-facts! ws (make-Case-Facts v p))
     (screen-set! (world-screen ws) 's2 v #:clear-first? #t)
     (screen-set! (world-screen ws) 'place p)]
    [else
     (screen-set! (world-screen ws) 'error)
     (raise '(game . error))]))

(define (end-game ws state kval)
  (define mi (world-murder-info ws))
  (define murderer-id (Murder-Info-murderer-id mi))
  (define s (hash-ref (Murder-Info-suspects mi) murderer-id))
  (define place (Suspect-Info-place s))
  (define side (Suspect-Info-side s))
  (define area (Suspect-Info-area s))
  (screen-set! (world-screen ws) 'side side #:clear-first? #t)
  (screen-set! (world-screen ws) 'area area)
  (screen-set! (world-screen ws) 'place place)
  (screen-set! (world-screen ws) 's2 murderer-id)
  (set-world-card-id! ws murderer-id))

(define (end-suspect ws state kval)
  (set-world-level-count! ws 0)
  (define scr (vector->list (world-screen ws)))
  (define id (string->number (apply string-append scr)))
  (define suspects (Murder-Info-suspects (world-murder-info ws)))
  (cond
    [(hash-has-key? suspects id)     
     (set-world-suspect-id! ws id)
     (set-world-card-id! ws id)
     (define info (world-murder-info ws))
     (define clues (hash-ref (world-suspect-clues ws) id))
     ;; Log the facts for this suspect
     (log-suspect-facts id clues (world-case-facts ws))
     ;; Populate the screen with clues
     (screen-set! (world-screen ws) 'clear)
     (unless (false? (Suspect-Clues-place clues))
       (screen-set! (world-screen ws) 'place (Suspect-Clues-place clues)))
     (unless (false? (Suspect-Clues-area clues))
       (screen-set! (world-screen ws) 'area (Suspect-Clues-area clues)))
     (unless (false? (Suspect-Clues-side clues))
       (screen-set! (world-screen ws) 'side (Suspect-Clues-side clues)))
     (unless (empty? (Suspect-Clues-alibis clues))
       (define alibis (Suspect-Clues-alibis clues))
       (define-values (s1 s2) (cond
                                [(= 2 (length alibis)) (values (first alibis) (second alibis))]
                                [else (values #f (first alibis))]))
       (unless (false? s1)
         (screen-set! (world-screen ws) 's1 s1))
       (screen-set! (world-screen ws) 's2 s2))]
    [else
     (screen-set! (world-screen ws) 'error)
     (raise 'error)]))

(define (odd-fingerprints info facts s# m# .38?)
  ;; Corrolate all known facts first.
  (filter-facts facts)
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

(define (end-question ws state kval)
  (define scr (vector->list (world-screen ws)))
  (define n (string->number (apply string-append scr)))
  (define suspect (world-suspect-id ws))
  (define valid-questions (Card-ques (hash-ref SUSPECT-CARDS suspect)))
  (cond    
    [(and (< (world-level-count ws) (world-level-max ws))
          (member n valid-questions))
     (set-world-level-count! ws (add1 (world-level-count ws)))
     (define s# (world-suspect-id ws))
     (define murder-info (world-murder-info ws))
     (define m# (Murder-Info-murderer-id murder-info))
     (define case-facts (world-case-facts ws))
     (define murder-facts (Case-Facts-murder case-facts))
     (define places (Case-Facts-places (world-case-facts ws)))
     (define suspects (Case-Facts-suspects (world-case-facts ws)))
     (set-world-question-id! ws n)
     (screen-set! (world-screen ws) 'clear)
     (case n
       [(1) ; Did the murderer go to the EAST Side?        
        (define ans (east-side?  murder-info m#))
        (set-Murder-Facts-side! murder-facts
                                (if (false? ans)
                                    (name->side "West Side")
                                    (name->side "East Side")))
        (screen-set! (world-screen ws) (if (false? ans) 'no 'yes))]
       [(2) ; Did a MALE do it?
        (define ans (if (< m# 11) 'yes 'no))
        (set-Murder-Facts-sex! murder-facts
                               (if (eq? ans 'yes)
                                   (name->sex "Male")
                                   (name->sex "Female")))
        (screen-set! (world-screen ws) ans)]
       [(3) ; What AREA did the murderer go to?
        (define ans (area murder-info m#))
        (set-Murder-Facts-area! murder-facts ans)
        (screen-set! (world-screen ws) 'area ans)]
       [(4) ; Was the MURDER WEAPON a .38?
        (define ans (Murder-Info-.38? murder-info))
        (set-Murder-Facts-weapon! murder-facts (if ans
                                                   (name->weapon ".38")
                                                   (name->weapon ".45")))
        (screen-set! (world-screen ws) (if (false? ans) 'no 'yes))]
       [(5) ; Where was the .38 hidden?
        (define ans (Murder-Info-.38-place murder-info))
        (set-Murder-Facts-.38-place! murder-facts ans)
        (define pfacts (hash-ref places ans))
        (set-Place-Facts-weapon! pfacts (name->weapon ".38"))        
        (screen-set! (world-screen ws) 'place ans)]
       [(6) ; Where was the .45 hidden?
        (define ans (Murder-Info-.45-place murder-info))
        (set-Murder-Facts-.45-place! murder-facts ans)
        (define pfacts (hash-ref places ans))
        (set-Place-Facts-weapon! pfacts (name->weapon ".45"))
        (screen-set! (world-screen ws) 'place ans)]
       [(7) ; Which PLACE contained only 3 suspects?
        (define ans (Murder-Info-3-sus-place murder-info))
        (set-Murder-Facts-3-sus-place! murder-facts ans)
        (screen-set! (world-screen ws) 'place ans)]
       [(8) ; Did the murderer go to PLACE A, B or C?
        (define ans (member (place murder-info m#) (take PLACE-IDS 3)))
        (set-Murder-Facts-ABC! murder-facts
                               (if (false? ans)
                                   (name->boolean "No")
                                   (name->boolean "Yes")))
        (screen-set! (world-screen ws) (if (false? ans) 'no 'yes))]
       [(9) ; Were you on the EAST SIDE?        
        (define sfacts (hash-ref suspects s#))
        (define ans (east-side?  murder-info s#))
        (set-Suspect-Facts-side! sfacts (if (false? ans)
                                            (name->side "West Side")
                                            (name->side "East Side")))
        (screen-set! (world-screen ws) (if (false? ans) 'no 'yes))]
       [(10) ; Which AREA were you in?
        (define sfacts (hash-ref suspects s#))
        (define ans (area murder-info s#))
        (set-Suspect-Facts-area! sfacts ans)
        (screen-set! (world-screen ws) 'area ans)]
       [(11) ; Were you at PLACE A, B or C?
        (define sfacts (hash-ref suspects s#))
        (define ans (member (place murder-info s#) (take PLACE-IDS 3)))
        (set-Suspect-Facts-ABC! sfacts (if (false? ans)
                                           (name->boolean "No")
                                           (name->boolean "Yes")))
        (screen-set! (world-screen ws) (if (false? ans) 'no 'yes))]
       [(12) ; Were you where a WEAPON was hidden?
        (define sfacts (hash-ref suspects s#))
        (define ans (member (place murder-info s#)
                            (list (Murder-Info-.38-place murder-info)
                                  (Murder-Info-.45-place murder-info))))
        (set-Suspect-Facts-at-weapon-place! sfacts (if (false? ans)
                                                       (name->boolean "No")
                                                       (name->boolean "Yes")))
        (screen-set! (world-screen ws) (if (false? ans) 'no 'yes))]
       [(13) ; Are an odd-numbered suspect's PRINTS on the .38?
        (define ans (odd-fingerprints murder-info
                                      case-facts
                                      s#
                                      m#
                                      #t))
        (set-Murder-Facts-.38-prints! murder-facts
                                      (name->boolean ans))
        (screen-set! (world-screen ws) ans)]
       [(14) ; Are an odd-numbered suspect's PRINTS on the .45?
        (define ans (odd-fingerprints murder-info
                                      case-facts
                                      s#
                                      m#
                                      #f))
        (set-Murder-Facts-.45-prints! murder-facts
                                      (name->boolean ans))        
        (screen-set! (world-screen ws) ans)])]    
    [else
     (screen-set! (world-screen ws) 'error)
     (raise 'error)]))

(define (end-accuse ws state kval)
  (define scr (vector->list (world-screen ws)))
  (define n (string->number (apply string-append scr)))
  (cond
    [(and (number? n) (< 0 n 21))
     (set-world-card-id! ws n)
     (set-State-task-id! (world-state ws) 'end)
     (define m (Murder-Info-murderer-id (world-murder-info ws)))
     (if (= n m)
         ;; Universal police hi-low.
         (play-sound SOUND-WINNER #t)
         ;; 2 shots, followed by error beep and funeral dirge.
         (play-sound SOUND-WRONG-ACCUSATION #t))
     (screen-set! (world-screen ws) (if (= n m) 'yes 'no))]
    [else
     (set-State-task-id! (world-state ws) 'error)
     (screen-set! (world-screen ws) 'error)]))

(define (beg-facts ws state kval)  
  (define case-facts (world-case-facts ws))
  (if (false? case-facts)
      (raise '(game . end))
      (filter-facts case-facts))
  (set-world-console?! ws #f))


(define (end-facts ws state kval)
  (set-world-console?! ws #t))

(define (scroll-card ws state kval)
  (case kval
    [("left") (set-world-card-id! ws (modulo (sub1 (world-card-id ws)) 20))]
    [("right") (set-world-card-id! ws (modulo (add1 (world-card-id ws)) 20))]))

;; The STATES hash defines state groupings of tasks that handle events. Each task
;; within a state grouping has a task-id, an event (in this game its an on-key value),
;; a task-procedure that is executed when the state-id, task-id, and event are matched,,
;; and a new state-id and task-id to be used in the handling of the next event.
;;
;; The task-procedure is intended to manipulate any of the world's non-event states, but
;; may also override the new state-id, task-id by raising a new task-id symbol or
;; state-id/task-id pair.
;;
;; A #f in a task represents either no task-procedure to execute or no change in the
;; state-id, or task-id value. A "^" indicates that the stack of state-id/task-ids is
;; to be popped.
;;
;; In this particular game this allows the Facts states to return to the particular state
;; that invoked it. 
(define STATES
  (states-hash
   ;; Game Intro States
   (game    
    (begin string->number store-level  game begin)
    (begin ENTER-KEY-VAL  beg-game     turn end)

    (end   ON-KEY-VAL     intro-game   game begin)
    (end FACTS-KEY-VAL   beg-facts    facts   begin)

    (error string->number  store-level game begin))
   
   ;; Turn States
   (turn
    (end SUSPECT-KEY-VAL clear-screen suspect begin)
    (end ACCUSE-KEY-VAL  clear-screen accuse  begin)
    (end OFF-KEY-VAL     end-game     game    end)
    (end LEFT-KEY-VAL    scroll-card  turn    end)
    (end RIGHT-KEY-VAL   scroll-card  turn    end)
    (end FACTS-KEY-VAL   beg-facts    facts   begin))
   
   ;; Suspect States
   (suspect
    (begin FACTS-KEY-VAL    beg-facts    facts   begin)
    (begin LEFT-KEY-VAL     scroll-card  suspect begin)
    (begin RIGHT-KEY-VAL    scroll-card  suspect begin)
    (begin string->number   store-number suspect begin)
    (begin ENTER-KEY-VAL    end-suspect  suspect end)
    (begin END-TURN-KEY-VAL clear-screen turn    end)
    (begin OFF-KEY-VAL      end-game     game    end)

    (end   FACTS-KEY-VAL    beg-facts    facts    begin)
    (end   QUESTION-KEY-VAL clear-screen question begin)
    (end   END-TURN-KEY-VAL clear-screen turn     end)
    (end   OFF-KEY-VAL      end-game     game     end)
    
    (error SUSPECT-KEY-VAL  clear-screen suspect  begin)
    (error END-TURN-KEY-VAL clear-screen turn     end)
    (error OFF-KEY-VAL      end-game     game     end))
   
   ;; Question States
   (question
    (begin FACTS-KEY-VAL    beg-facts    facts    begin)
    (begin string->number   store-number question begin)
    (begin ENTER-KEY-VAL    end-question question end)
    (begin END-TURN-KEY-VAL clear-screen turn     end)
    (begin OFF-KEY-VAL      end-game     game     end)

    (end   FACTS-KEY-VAL    beg-facts    facts    begin)
    (end   QUESTION-KEY-VAL clear-screen question begin)
    (end   END-TURN-KEY-VAL clear-screen turn     end)
    (end   OFF-KEY-VAL      end-game     game     end)
    
    (error QUESTION-KEY-VAL clear-screen question begin)
    (error END-TURN-KEY-VAL clear-screen turn     end)
    (error OFF-KEY-VAL      end-game     game     end))
   
   ;; Accuse States
   (accuse
    (begin FACTS-KEY-VAL    beg-facts    facts   begin)
    (begin string->number   store-number accuse begin)
    (begin ENTER-KEY-VAL    end-accuse   accuse end)
    (begin END-TURN-KEY-VAL #f           turn   end)
    (begin OFF-KEY-VAL      end-game     game   end)

    (end   FACTS-KEY-VAL    beg-facts    facts  begin)
    (end   END-TURN-KEY-VAL #f           turn   end)
    (end   OFF-KEY-VAL      end-game     game   end)
    (error ACCUSE-KEY-VAL   clear-screen accuse begin)
    (error END-TURN-KEY-VAL #f           turn   end)
    (error OFF-KEY-VAL      end-game     game   end))

   ;; Facts States
   (facts
    (begin CONSOLE-KEY-VAL end-facts    ^    #f))))

;; This game is only interested in on-key events. All the "buttons" are keyboard driven.
(define (key-handler ws ke)
  ;; Translate numpad values into equivalent non-numpad values. 
  (define kval (cond
                 [(key=? ke "numpad-enter") ENTER-KEY-VAL]
                 [else (string-trim (string-downcase ke) "numpad")]))
  (state-handler ws STATES (world-state ws) kval)
  ws)

(define MT-WIDTH 1600)
(define MT-HEIGHT 850)
(define MT (empty-scene MT-WIDTH MT-HEIGHT))

(define (render ws)
  (if (world-console? ws)
      (place-image/align
       (draw-console/card-panel (world-screen ws)
                                              (State-event (world-state ws))
                                              (world-card-id ws))
       0 0       
       "left" "top"
       MT)
      (place-image/align
       (draw-case-facts (world-case-facts ws))       
       0 0
       "left" "top"
       MT)))

(define (new-world)
  (world #t #f #f (State 'game 'end "") (make-vector 7 "") #f #f #f #f #f #f))

(big-bang (new-world)
  (to-draw render)
  (on-key key-handler)
  (name "Racket Detective"))
