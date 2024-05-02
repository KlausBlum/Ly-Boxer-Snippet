\version "2.24.3"

% Necessary predicates
#(define (color-or-false? obj)
   (or (color? obj) (eq? obj #f)))

#(define (hide-target? obj)
   (if (member
        obj
        #'("none"
            "staff"
            "music"
            "all"))
       #t
       #f))

#(define (caption? obj)
   (or (string? obj)
       (markup? obj)
       (eq? obj #f)))


% some helper functions:

#(define-markup-command (on-box layout props radius color arg) (number? scheme? markup?)
   (let* ((stencil (interpret-markup layout props arg))
          (X-ext (ly:stencil-extent stencil X))
          (Y-ext (ly:stencil-extent stencil Y)))
     (if (color? color)
         (ly:stencil-add (ly:make-stencil
                          (list 'color color
                                (ly:stencil-expr (ly:round-filled-box X-ext Y-ext radius))
                                X-ext Y-ext)) stencil)
         stencil)
     )
   )

#(define (rotate-point point-to-add rotation x-center y-center)
   "Rotate the given point (point-to-add) around (x-center, y-center) by
     the given rotation angle (in degrees)."
   (let*
    (
      (x-to-add (car point-to-add))
      (y-to-add (cdr point-to-add))
      ; convert (x-to-add | y-to-add) to polar coordinates (distance ; direction):
      (x-diff (- x-to-add x-center))
      (y-diff (- y-to-add y-center))
      (distance (sqrt (+ (expt x-diff 2) (expt y-diff 2))))
      (direction
       (if (eq? 0 x-diff)
           ;(then...)
           (if (> y-diff 0) 90 -90)
           ;(else...)
           (+ (atan (/ y-diff x-diff)) (if (< x-diff 0) 3.141592653589 0))
           )
       )
      ; apply rotation:
      (new-direction (+ direction (* rotation (/ 3.14159265 180))))
      (new-x (+ x-center (* distance (cos new-direction))))
      (new-y (+ y-center (* distance (sin new-direction))))
      )
    ; return rotated point as pair of coordinates:
    (cons new-x new-y)
    )
   )

#(define (expand-range range point-to-add)
   "Expand the borders of the given range until it contains the added point.
    Return the expanded range."
   (let*
    ; split pair of pairs into separate variables for better usability:
    (
      (x-lo (car (car range)))
      (x-hi (cdr (car range)))
      (y-lo (car (cdr range)))
      (y-hi (cdr (cdr range)))
      (x-to-add (car point-to-add))
      (y-to-add (cdr point-to-add))
      )
    ; initial values are #f. Replace them, if present:
    (if (eq? #f x-lo) (set! x-lo x-to-add))
    (if (eq? #f x-hi) (set! x-hi x-to-add))
    (if (eq? #f y-lo) (set! y-lo y-to-add))
    (if (eq? #f y-hi) (set! y-hi y-to-add))
    ; now expand borders:
    (if (< x-to-add x-lo) (set! x-lo x-to-add))
    (if (> x-to-add x-hi) (set! x-hi x-to-add))
    (if (< y-to-add y-lo) (set! y-lo y-to-add))
    (if (> y-to-add y-hi) (set! y-hi y-to-add))
    ; return expanded range as pair of pairs:
    (cons (cons x-lo x-hi) (cons y-lo y-hi))
    )
   )



#(define-event-class 'music-boxer-event 'span-event)

#(define-event-class 'box-event 'music-event)

#(define (add-grob-definition grob-name grob-entry)
   (set! all-grob-descriptions
         (cons ((@@ (lily) completize-grob-entry)
                (cons grob-name grob-entry))
               all-grob-descriptions)))

#(define (define-grob-property symbol type? description)
   ;(if (not (equal? (object-property symbol 'backend-doc) #f))
   ;    (ly:error (_ "symbol ~S redefined") symbol))

   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc description)
   symbol)

#(map
  (lambda (x)
    (apply define-grob-property x))

  `(
     (acknowledge-finger-interface ,boolean? "Include fingerings in box?")
     (acknowledge-script-interface ,boolean? "Include scripts in box?")
     ; add more properties here
     (color ,color-or-false? "Background color for filling the rectangle")
     (border-color ,color-or-false? "Border color for the rectangle")
     (border-width ,number? "Width / thickness of the border rectangle")
     (broken-bound-padding ,number? "Amount of protrusion into the margin when split by a line break")
     (border-radius ,number? "")
     (shorten-pair ,number-pair? "")
     (l-zigzag-width ,number? "")
     (r-zigzag-width ,number? "")
     (open-on-bottom ,boolean? "")
     (open-on-top ,boolean? "")
     ; (hide ,hide-target? "")   ; TODO?
     (angle ,number? "")
     (caption ,caption? "")
     (caption-padding ,number? "")
     (caption-radius ,number? "")
     (caption-align-bottom ,boolean? "")
     (caption-halign ,number? "")  ; from -1=left to 1=right
     (caption-color ,color-or-false? "")  ; ##f will use border-color
     (caption-keep-y ,boolean? "")
     (caption-translate-x ,number? "")
     (set-top-edge ,boolean? "")
     (set-bottom-edge ,boolean? "")
     (set-left-edge ,boolean? "")
     (set-right-edge ,boolean? "")
     (set-caption-extent ,boolean? "")
     ))


#(define (music-boxer-stencil grob)
   (let* (
           (elts (ly:grob-object grob 'elements))
           (refp-X (ly:grob-common-refpoint-of-array grob elts X))
           (refp-Y (ly:grob-common-refpoint-of-array grob elts Y))
           (offset (ly:grob-relative-coordinate grob refp-X X))

           (padding (ly:grob-property grob 'padding 0.3))
           (xext (interval-widen (ly:relative-group-extent elts refp-X X) padding))
           (yext (interval-widen (ly:relative-group-extent elts refp-Y Y) padding))
           (border-width (ly:grob-property grob 'border-width 0.25))
           (color (ly:grob-property grob 'color (rgb-color 0.8  0.8  1.0)))
           (border-color (ly:grob-property grob 'border-color (rgb-color 0.3  0.3  0.9)))
           (broken-bound-padding (ly:grob-property grob 'broken-bound-padding 4))
           (border-radius (ly:grob-property grob 'border-radius 0))
           (shorten-pair (ly:grob-property grob 'shorten-pair (cons 0 0)))
           (y-lower (car yext))
           (y-upper (cdr yext))
           (l-zigzag-width (ly:grob-property grob 'l-zigzag-width 0))
           (r-zigzag-width (ly:grob-property grob 'r-zigzag-width 0))
           (open-on-bottom (ly:grob-property grob 'open-on-bottom #f))
           (open-on-top    (ly:grob-property grob 'open-on-top #f))
           ; (hide (ly:grob-property grob 'hide "none"))   ; TODO?
           (angle (ly:grob-property grob 'angle 0))
           (caption (ly:grob-property grob 'caption #f))
           (caption-padding (ly:grob-property grob 'caption-padding 0.25))
           (caption-radius (ly:grob-property grob 'caption-radius 0.25))
           (caption-align-bottom (ly:grob-property grob 'caption-align-bottom #f))
           (caption-halign (ly:grob-property grob 'caption-halign -1))  ; from -1=left to 1=right
           (caption-color (ly:grob-property grob 'caption-color #f)) ; ##f will use border-color
           (caption-keep-y (ly:grob-property grob 'caption-keep-y #f))
           (caption-translate-x (ly:grob-property grob 'caption-translate-x 0))
           (set-top-edge (ly:grob-property grob 'set-top-edge #f))
           (set-bottom-edge (ly:grob-property grob 'set-bottom-edge #f))
           (set-left-edge (ly:grob-property grob 'set-left-edge #f))
           (set-right-edge (ly:grob-property grob 'set-right-edge #f))
           (set-caption-extent (ly:grob-property grob 'set-caption-extent #f))

           (y-l-lower (if (number? y-lower) y-lower (car y-lower)))
           (y-r-lower (if (number? y-lower) y-lower (cdr y-lower)))
           (y-l-upper (if (number? y-upper) y-upper (car y-upper)))
           (y-r-upper (if (number? y-upper) y-upper (cdr y-upper)))

           (open-on-left
            (and (ly:spanner? grob)
                 (= 1 (ly:item-break-dir (ly:spanner-bound grob LEFT)))))
           (open-on-right
            (and (ly:spanner? grob)
                 (= -1 (ly:item-break-dir (ly:spanner-bound grob RIGHT)))))

           (stil empty-stencil)
           )  ; let* definitions

     (set! stil
           (ly:stencil-add
            (if (color? color)
                (ly:make-stencil (list 'color color
                                       (list 'round-filled-box
                                             (- (- (car xext) border-width)) (+ (cdr xext) border-width)
                                             (- (car yext)) (cdr yext)
                                             0.0)
                                       xext yext))
                empty-stencil)
            (if (and (> border-width 0) (color? border-color))  ; only add stencil if set to a valid color (could also be set to ##f)
                (stencil-with-color
                 (ly:round-filled-box
                  (cons (- (car xext) border-width) (+ (cdr xext) border-width))
                  (cons (- (car yext) border-width) (car yext))
                  0)
                 border-color)
                empty-stencil)
            (if (and (> border-width 0) (color? border-color))  ; only add stencil if set to a valid color (could also be set to ##f)
                (stencil-with-color
                 (ly:round-filled-box
                  (cons (- (car xext) border-width) (+ (cdr xext) border-width))
                  (cons (cdr yext) (+ (cdr yext) border-width))
                  0)
                 border-color)
                empty-stencil)
            (if (and (not open-on-right) (> border-width 0) (color? border-color))  ; only add stencil if set to a valid color (could also be set to ##f)
                (stencil-with-color
                 (ly:round-filled-box
                  (cons (cdr xext) (+ (cdr xext) border-width))
                  yext
                  0)
                 border-color)
                empty-stencil)
            (if (and (not open-on-left) (> border-width 0) (color? border-color))  ; only add stencil if set to a valid color (could also be set to ##f)
                (stencil-with-color
                 (ly:round-filled-box
                  (cons (- (car xext) border-width) (car xext))
                  yext
                  0)
                 border-color)
                empty-stencil)
            ); ly:stencil-add ...

           ) ; end of "set! stil ..."
     (ly:stencil-translate-axis stil (- offset) X)
     )
   )

#(define box-stil music-boxer-stencil)

#(add-grob-definition
  'Box
  `(
     (stencil . ,box-stil)
     (meta . ((class . Item)
              (interfaces . ())))))

#(add-grob-definition
  'MusicBoxer
  `(
     (stencil . ,music-boxer-stencil)
     (meta . ((class . Spanner)
              (interfaces . ())))))


#(define box-types
   '(
      (BoxEvent
       . ((description . "A box encompassing music at a single timestep.")
          (types . (general-music box-event music-event event))
          ))
      ))

#(define music-boxer-types
   '(
      (MusicBoxerEvent
       . ((description . "Used to signal where boxes encompassing music start and stop.")
          (types . (general-music music-boxer-event span-event event))
          ))
      ))


#(set!
  music-boxer-types
  (map (lambda (x)
         (set-object-property! (car x)
                               'music-description
                               (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
       music-boxer-types))

#(set!
  box-types
  (map (lambda (x)
         (set-object-property! (car x)
                               'music-description
                               (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
       box-types))

#(set! music-descriptions
       (append music-boxer-types music-descriptions))

#(set! music-descriptions
       (append box-types music-descriptions))

#(set! music-descriptions
       (sort music-descriptions alist<?))


#(define (add-bound-item spanner item)
   (if (null? (ly:spanner-bound spanner LEFT))
       (ly:spanner-set-bound! spanner LEFT item)
       (ly:spanner-set-bound! spanner RIGHT item)))

musicBoxerEngraver =
#(lambda (context)
   (let ((span '())
         (finished '())
         (current-event '())
         (event-start '())
         (event-stop '())
         )

     `((listeners
        (music-boxer-event .
                           ,(lambda (engraver event)
                              (if (= START (ly:event-property event 'span-direction))
                                  (set! event-start event)
                                  (set! event-stop event)))))

       (acknowledgers
        (note-column-interface .
                               ,(lambda (engraver grob source-engraver)
                                  (if (ly:spanner? span)
                                      (begin
                                       (ly:pointer-group-interface::add-grob span 'elements grob)
                                       (add-bound-item span grob)))
                                  (if (ly:spanner? finished)
                                      (begin
                                       (ly:pointer-group-interface::add-grob finished 'elements grob)
                                       (add-bound-item finished grob)))))

        (inline-accidental-interface .
                                     ,(lambda (engraver grob source-engraver)
                                        (if (ly:spanner? span)
                                            (begin
                                             (ly:pointer-group-interface::add-grob span 'elements grob)))
                                        (if (ly:spanner? finished)
                                            (ly:pointer-group-interface::add-grob finished 'elements grob))))

        (dots-interface .
                        ,(lambda (engraver grob source-engraver)
                           (if (ly:spanner? span)
                               (begin
                                (ly:pointer-group-interface::add-grob span 'elements grob)))
                           (if (ly:spanner? finished)
                               (ly:pointer-group-interface::add-grob finished 'elements grob))))

        (ledger-line-spanner-interface .
                                       ,(lambda (engraver grob source-engraver)
                                          (if (ly:spanner? span)
                                              (begin
                                               (ly:pointer-group-interface::add-grob span 'elements grob)))
                                          (if (ly:spanner? finished)
                                              (ly:pointer-group-interface::add-grob finished 'elements grob))))

        (script-interface .
                          ,(lambda (engraver grob source-engraver)
                             (if (and (ly:spanner? span)
                                      (eq? #t (ly:grob-property span 'acknowledge-script-interface)))
                                 (begin
                                  (ly:pointer-group-interface::add-grob span 'elements grob)))
                             (if (and (ly:spanner? finished)
                                      (eq? #t (ly:grob-property finished 'acknowledge-script-interface)))
                                 (ly:pointer-group-interface::add-grob finished 'elements grob))))

        (finger-interface .
                          ,(lambda (engraver grob source-engraver)
                             (if (and (ly:spanner? span)
                                      (eq? #t (ly:grob-property span 'acknowledge-finger-interface)))
                                 (begin
                                  (ly:pointer-group-interface::add-grob span 'elements grob)))
                             (if (and (ly:spanner? finished)
                                      (eq? #t (ly:grob-property finished 'acknowledge-finger-interface)))
                                 (ly:pointer-group-interface::add-grob finished 'elements grob))))

        ;; add additional interfaces to acknowledge here

        )

       (process-music .
                      ,(lambda (trans)
                         (if (ly:stream-event? event-stop)
                             (if (null? span)
                                 (ly:warning "No start to this box.")
                                 (begin
                                  (set! finished span)
                                  (ly:engraver-announce-end-grob trans finished event-start)
                                  (set! span '())
                                  (set! event-stop '()))))
                         (if (ly:stream-event? event-start)
                             (begin
                              (set! span (ly:engraver-make-grob trans 'MusicBoxer event-start))
                              (set! event-start '())))))

       (stop-translation-timestep .
                                  ,(lambda (trans)
                                     (if (and (ly:spanner? span)
                                              (null? (ly:spanner-bound span LEFT)))
                                         (ly:spanner-set-bound! span LEFT
                                                                (ly:context-property context 'currentMusicalColumn)))
                                     (if (ly:spanner? finished)
                                         (begin
                                          (if (null? (ly:spanner-bound finished RIGHT))
                                              (ly:spanner-set-bound! finished RIGHT
                                                                     (ly:context-property context 'currentMusicalColumn)))
                                          (set! finished '())
                                          (set! event-start '())
                                          (set! event-stop '())))))

       (finalize
        (lambda (trans)
          (if (ly:spanner? finished)
              (begin
               (if (null? (ly:spanner-bound finished RIGHT))
                   (set! (ly:spanner-bound finished RIGHT)
                         (ly:context-property context 'currentMusicalColumn)))
               (set! finished '())))
          (if (ly:spanner? span)
              (begin
               (ly:warning "unterminated box :-(")
               (ly:grob-suicide! span)
               (set! span '())))
          )))))


boxEngraver =
#(lambda (context)
   (let ((box '())
         (ev '()))

     `((listeners
        (box-event .
                   ,(lambda (engraver event)
                      (set! ev event))))

       (acknowledgers
        (note-column-interface .
                               ,(lambda (engraver grob source-engraver)
                                  (if (ly:grob? box)
                                      (begin
                                       ; (set! (ly:grob-parent box X) grob) ;; ??
                                       (set! (ly:grob-parent box Y) grob)
                                       (ly:pointer-group-interface::add-grob box 'elements grob)))))

        (inline-accidental-interface .
                                     ,(lambda (engraver grob source-engraver)
                                        (if (ly:item? box)
                                            (ly:pointer-group-interface::add-grob box 'elements grob))))

        (dots-interface .
                        ,(lambda (engraver grob source-engraver)
                           (if (ly:item? box)
                               (ly:pointer-group-interface::add-grob box 'elements grob))))

        (ledger-line-spanner-interface .
                                       ,(lambda (engraver grob source-engraver)
                                          (if (ly:item? box)
                                              (ly:pointer-group-interface::add-grob box 'elements grob))))

        (script-interface .
                          ,(lambda (engraver grob source-engraver)
                             (if (and (ly:item? box) (eq? #t (ly:grob-property box 'acknowledge-script-interface)))
                                 (ly:pointer-group-interface::add-grob box 'elements grob))))

        (finger-interface .
                          ,(lambda (engraver grob source-engraver)
                             (if (and (ly:item? box) (eq? #t (ly:grob-property box 'acknowledge-finger-interface)))
                                 (ly:pointer-group-interface::add-grob box 'elements grob))))

        ;; add additional interfaces to acknowledge here

        )

       (process-music .
                      ,(lambda (trans)
                         (if (ly:stream-event? ev)
                             (begin
                              (set! box (ly:engraver-make-grob trans 'Box ev))
                              (set! ev '())))))
       (stop-translation-timestep .
                                  ,(lambda (trans)
                                     (set! box '()))))))

musicBoxerStart =
#(make-span-event 'MusicBoxerEvent START)

musicBoxerEnd =
#(make-span-event 'MusicBoxerEvent STOP)

box = #(make-music 'BoxEvent)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\header {
  tagline = ##f
}

melody = \relative c' {
  \override Score.MusicBoxer.layer = -10
  \override Score.MusicBoxer.filled = ##t
  \override Score.MusicBoxer.border-width = 0.5
  \override Score.MusicBoxer.border-color = ##f
  \override Score.MusicBoxer.color = #(rgb-color 1 0.8 0.8)

  \time 3/4

  \musicBoxerStart
  e4 e d
  \musicBoxerEnd
  c2.

  \override Score.MusicBoxer.border-color = #red
  \override Score.MusicBoxer.color = ##f
  \musicBoxerStart
  e4 e d
  \musicBoxerEnd
  c2.
  \override Score.MusicBoxer.border-color = #blue
  \override Score.MusicBoxer.color = #(rgb-color 0.8 0.8 1)
  \musicBoxerStart
  e4 f g

  g4 f8 e \musicBoxerEnd f4

  \musicBoxerStart
  d4 e f
  \break
  f4 e8 d \musicBoxerEnd e4
  \override Score.MusicBoxer.border-color = #(rgb-color 1 0.4 0.0)
  \override Score.MusicBoxer.color = #(rgb-color 1 0.9 0.8)
  \musicBoxerStart
  e4 e f
  \musicBoxerEnd
  g2.
  \override Score.MusicBoxer.border-color = #red
  \override Score.MusicBoxer.color = #(rgb-color 1 0.8 0.8)
  \musicBoxerStart
  e4 e d
  \musicBoxerEnd
  c2.
}

another = \relative c' {
  \set fingeringOrientations = #'(left)
  \override Score.Box.layer = -10
  \override Score.Box.filled = ##t
  \override Score.Box.border-width = 0.5
  \override Score.Box.border-color = #(rgb-color 0.0 0.9 0.0)
  \override Score.Box.color = #(rgb-color 0.9 1 0.8)

  \time 4/4
  \musicBoxerStart c4\f e g \musicBoxerEnd c
  \once \override Score.MusicBoxer.acknowledge-script-interface = ##t
  \musicBoxerStart c,4\f e g \musicBoxerEnd c
  \musicBoxerStart c,4-1 e-2 g-3 \musicBoxerEnd c-5
  \once \override Score.MusicBoxer.acknowledge-finger-interface = ##t
  \musicBoxerStart c,4-1 e-2 g-3 \musicBoxerEnd c-5
  \box <c-5 g-3 e-2 c-1>1  \f \fermata
  \once \override Score.Box.acknowledge-script-interface = ##t
  \once \override Score.Box.acknowledge-finger-interface = ##t
  \box <c-5 g-3 e-2 c-1>1  \f \fermata
}

\score {
  \new Staff { \melody \another }
}

\layout {
  \context {
    \Global
    \grobdescriptions #all-grob-descriptions
  }
  \context {
    \Score
    \consists \musicBoxerEngraver % for spans
    \consists \boxEngraver
  }
}



