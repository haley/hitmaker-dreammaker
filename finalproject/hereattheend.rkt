#lang racket/gui
(require racket/port)
(require racket/trace)
#|
 ;   ;   ;;;   ;   ;   ;;;;  ;;;;;     ;   ;  ;;;;;  ;;;;;  ;;;;  ;;;  ;   ;   ;;;;
 ;   ;  ;   ;  ;   ;  ;      ;         ;  ;   ;      ;      ;   ;  ;   ;;  ;  ;
 ;;;;;  ;   ;  ;   ;   ;;;   ;;;  ;;;  ;;;    ;;;    ;;;    ;;;;   ;   ; ; ;  ; ;;;
 ;   ;  ;   ;  ;   ;      ;  ;         ;  ;   ;      ;      ;      ;   ;  ;;  ;   ;
 ;   ;   ;;;    ;;;   ;;;;;  ;;;;;     ;   ;  ;;;;;  ;;;;;  ;     ;;;  ;   ;   ;;;

Hit Maker - Dream Maker has been a collaboration between Haley Fletcher and Alfredo Hernandez 

|#
;----------------------------------------------------------------------:
;Defines the chords in each key

(struct chords_key ( i ii iii iv v vi vii) #:transparent) ;creates the templates for the chord bank
         ;a
(define key_A (chords_key 'A 'Bm 'C#m 'D 'E7 'F#m 'G#dim))
         ;b
(define key_B (chords_key 'B 'C#m 'D#m 'E 'F#7 'G#m 'A#dim))
         ;c
(define key_C (chords_key 'C 'Dm 'Em 'F 'G7 'Am 'Bdim))
         ;d
(define key_D (chords_key 'D 'Em 'F#m 'G 'A7 'Bm 'C#dim))
         ;e
(define key_E (chords_key 'E 'F#m 'G#m 'A 'B7 'C#m 'D#dim))
         ;f
(define key_F (chords_key 'F 'Gm 'Am 'B♭ 'C7 'Dm 'Edim))
         ;g
(define key_G (chords_key 'G 'Am 'Bm 'F 'D7 'Em 'F#dim))


(struct chords ( I II III IV) #:transparent) ;actual struct that will be pulled from

;----------------------------------------------------------------------:
;Creates the chord progressions

(define (chord_progression_full genre key)
  (cond [ (equal? key 'A) (set! key key_A)]
        [ (equal? key 'B) (set! key key_B)]
        [ (equal? key 'C) (set! key key_C)]
        [ (equal? key 'D) (set! key key_D)]
        [ (equal? key 'E) (set! key key_E)]
        [ (equal? key 'F) (set! key key_F)]
        [ (equal? key 'G) (set! key key_G)])
  (progression genre key))


(define (progression genre key) ;Creates the accurate progression based off of user inputs
  (cond [ (equal? genre 'Jazz ) (chords (chords_key-ii key) (chords_key-v key) (chords_key-i key) (chords_key-i key))]
        [ (equal? genre 'Pop ) (chords (chords_key-i key) (chords_key-v key) (chords_key-vi key) (chords_key-iv key))]
        [ (equal? genre 'Rock ) (chords (chords_key-i key) (chords_key-iv key) (chords_key-v key) (chords_key-iv key))]
        [ (equal? genre 'Dowop) (chords (chords_key-i key) (chords_key-vi key) (chords_key-iv key) (chords_key-v key))]
    (chords)))

;-------------------------------------------------------------------------:
;Voodoo Chord Magic
;-------------------------------------------------------------------------:

(define (lyrics genre verses)
  (if (equal? verses 0)
      ;then
      ""
      ;else
     
 (string-append
      (cond
    ((equal? genre 'Jazz) (search empty "jazzlyrics.txt" 1))
    ((equal? genre 'Rock) (search empty "rocklyrics.txt" 1))
    ((equal? genre 'Dowop) (search empty "dowoplyrics.txt" 1))
    ((equal? genre 'Pop) (search empty "poplyrics.txt"  1))
    )
   "
"
    ;  (lyrics genre (- verses 1))
      )))


(define (search listof document lines)
 (define in (open-input-file document))
 (define listof (port->list read-line in) )
  (if (> lines 1)
  ;then
(string-append "
"
 (list-ref listof (- (random 1 (+ (length listof) 1)) 1))
 "
"
  (search listof document (- lines 1)))
 ;else
 (string-append "
"(list-ref listof (- (random 1 (+ (length listof) 1)) 1)) "
" ))
  )

 (define (print_chords genre key verses)
(if (> verses 0)
    ;then
    (string-append "
" (symbol->string (chords-I (chord_progression_full genre key) )) "                      "
               (symbol->string (chords-II (chord_progression_full genre key) )) 
(lyrics genre verses)

 (symbol->string (chords-III (chord_progression_full genre key) ))"                        "
  (symbol->string (chords-IV (chord_progression_full genre key) ))
(lyrics genre verses)
  
 (symbol->string (chords-I (chord_progression_full genre key) )) "                       "
               (symbol->string (chords-II (chord_progression_full genre key) )) 
(lyrics genre verses)

 (symbol->string (chords-III (chord_progression_full genre key) ))"                         "
  (symbol->string (chords-IV (chord_progression_full genre key) ))
(lyrics genre verses)
  "


"
 (print_chords genre key (- verses 1)))
  ;else
  (string-append "
")
               )
)

;----------------------------------------------------------------
;GUI Maker
;----------------------------------------------------------------

;Makes the frame

(define window (new frame% [label "Hit Maker: Dream Maker"]
                    [width 220]
                    [height 187]))
(send window show #t)

;----------------------------------------------------------------
;Makes the genre list
;sets the empty variable

(define genre '())
(define genre-list (new choice% [label "Pick a Genre    "] ;creates the actual list
                        [choices (list "Jazz" "Pop" "Dowop" "Rock")]
                        [parent window]
                        [callback
                         (lambda (gl e)
                          (case (send gl get-selection)
                            {(0) (set! genre 'Jazz)}  
                            {(1) (set! genre 'Pop)}   
                            {(2) (set! genre 'Dowop)}  
                            {(3) (set! genre 'Rock)}))]  
                        [vert-margin 2]
                        [horiz-margin 2]))

(send genre-list show #t)

;----------------------------------------------------------------
;Makes the key list
;sets the empty variable

(define key '())        
(define key-list (new choice% [label "Pick a Key                  "]
                        [choices (list "A" "B" "C" "D" "E" "F" "G")]
                        [parent window]
                        [callback
                         (lambda (kl e)
                          (case (send kl get-selection)
                            {(0) (set! key 'A) }  
                            {(1) (set! key 'B) }   
                            {(2) (set! key 'C) }
                            {(3) (set! key 'D) }
                            {(4) (set! key 'E) }
                            {(5) (set! key 'F) }
                            {(6) (set! key 'G) }
                            ))]
                        [vert-margin 2]
                        [horiz-margin 2]))

(send key-list show #t)

;----------------------------------------------------------------
;Makes the verse length
;sets the initial verse length

(define verses 0)
(define verse-list (new choice% [label "How Many Verses?   "]
                        [choices (list "2" "3" "4" "5")]
                        [parent window]
                        [callback
                         (lambda (vl e)
                          (case (send vl get-selection)
                            {(0) (set! verses 2)}  
                            {(1) (set! verses 3)}   
                            {(2) (set! verses 4)}  
                            {(3) (set! verses 5)}
                            ))] 
                        [vert-margin 2]
                        [horiz-margin 2]))

(send verse-list show #t)

;----------------------------------------------------------------
;Sets a warning label

(define instructions (new message% [label "Please select a choice. Will not run if no choice is made by user."]
                          [parent window]))

(send instructions show #t)

;----------------------------------------------------------------
;Makes the button that runs the program

(define final-button (new button% [parent window] [label "Ready to make a Hit?"]
     [callback (lambda (button event)
                 (send new-window  show #t)
                 (display (print_chords genre key verses)) (send end-message show #t))]
                      
     [horiz-margin 7]))

(send final-button show #t)

;---------------------------------------------------------------
;Different possible status labels

(define new-window (new frame% [label "Status"]
                       [width 200]
                       [height 50]))

(define end-message (new message% [label "Your Finished Hit Song Is In the Command Window!\n                                Thank you!"]
                                          [parent new-window]))


