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
     (filled ,boolean? "Should we fill in this box?")
     (fill-color ,color? "Background color for filling the rectangle")
     (acknowledge-finger-interface ,boolean? "Include fingerings in box?")
     (acknowledge-script-interface ,boolean? "Include scripts in box?")
     ; add more properties here
     ))


#(define (music-boxer-stencil grob)
   (let* (
           (elts (ly:grob-object grob 'elements))
           (padding (ly:grob-property grob 'padding 0.3))
           (refp-X (ly:grob-common-refpoint-of-array grob elts X))
           (xext (interval-widen (ly:relative-group-extent elts refp-X X) padding))
           (refp-Y (ly:grob-common-refpoint-of-array grob elts Y))
           (yext (interval-widen (ly:relative-group-extent elts refp-Y Y) padding))
           (thick (ly:grob-property grob 'thickness 0.1))
           (filled (ly:grob-property grob 'filled #f))
           (fill-color (ly:grob-property grob 'fill-color grey))
           (offset (ly:grob-relative-coordinate grob refp-X X))

           (open-on-left
            (and (ly:spanner? grob)
                 (= 1 (ly:item-break-dir (ly:spanner-bound grob LEFT)))))
           (open-on-right
            (and (ly:spanner? grob)
                 (= -1 (ly:item-break-dir (ly:spanner-bound grob RIGHT)))))

           (stil empty-stencil)
           )
     (set! stil
           (ly:stencil-add
            (if filled
                (ly:make-stencil (list 'color fill-color
                                       (list 'round-filled-box
                                             (- (- (car xext) thick)) (+ (cdr xext) thick)
                                             (- (car yext)) (cdr yext)
                                             0.0)
                                       xext yext))
                empty-stencil)
            (if (> thick 0)
                (make-filled-box-stencil
                 (cons (- (car xext) thick) (+ (cdr xext) thick))
                 (cons (- (car yext) thick) (car yext)))
                empty-stencil)
            (if (> thick 0)
                (make-filled-box-stencil
                 (cons (- (car xext) thick) (+ (cdr xext) thick))
                 (cons (cdr yext) (+ (cdr yext) thick)))
                empty-stencil)
            (if (and (not open-on-right) (> thick 0))
                (make-filled-box-stencil
                 (cons (cdr xext) (+ (cdr xext) thick))
                 yext)
                empty-stencil)
            (if (and (not open-on-left) (> thick 0))
                (make-filled-box-stencil
                 (cons (- (car xext) thick) (car xext))
                 yext)
                empty-stencil)
            ) ; ly:stencil-add ...

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
  \override Score.MusicBoxer.thickness = 0.5
  \override Score.MusicBoxer.color = #red
  \override Score.MusicBoxer.fill-color = #(rgb-color 1 0.8 0.8)

  \time 3/4

  \musicBoxerStart
  e4 e d
  \musicBoxerEnd
  c2.
  \musicBoxerStart
  e4 e d
  \musicBoxerEnd
  c2.
  \override Score.MusicBoxer.color = #blue
  \override Score.MusicBoxer.fill-color = #(rgb-color 0.8 0.8 1)
  \musicBoxerStart
  e4 f g

  g4 f8 e \musicBoxerEnd f4

  \musicBoxerStart
  d4 e f
  \break
  f4 e8 d \musicBoxerEnd e4
  \override Score.MusicBoxer.color = #(rgb-color 1 0.4 0.0)
  \override Score.MusicBoxer.fill-color = #(rgb-color 1 0.9 0.8)
  \musicBoxerStart
  e4 e f
  \musicBoxerEnd
  g2.
  \override Score.MusicBoxer.color = #red
  \override Score.MusicBoxer.fill-color = #(rgb-color 1 0.8 0.8)
  \musicBoxerStart
  e4 e d
  \musicBoxerEnd
  c2.
}

another = \relative c' {
  \set fingeringOrientations = #'(left)
  \override Score.Box.layer = -10
  \override Score.Box.filled = ##t
  \override Score.Box.thickness = 0.5
  \override Score.Box.color = #(rgb-color 0.0 0.9 0.0)
  \override Score.Box.fill-color = #(rgb-color 0.9 1 0.8)

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



