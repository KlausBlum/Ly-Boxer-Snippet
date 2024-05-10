\version "2.24.3"

% Necessary predicates
#(define (color-or-false? obj)
   (or (color? obj) (eq? obj #f)))

% #(define (number-or-pair? obj)
%    (or (number-pair? obj) (number? obj)))

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
     (y-upper ,number-or-pair? "")
     (y-lower ,number-or-pair? "")
     (l-zigzag-width ,number? "")
     (r-zigzag-width ,number? "")
     (open-on-bottom ,boolean? "")
     (open-on-top ,boolean? "")
     ; (hide ,hide-target? "")   ; TODO?
     (angle ,number? "")
     (layer ,number? "")
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
   (let*
    ((elts (ly:grob-object grob 'elements))
     (refp-X (ly:grob-common-refpoint-of-array grob elts X))
     (refp-Y (ly:grob-common-refpoint-of-array grob elts Y))
     (offset (ly:grob-relative-coordinate grob refp-X X))

     (padding (ly:grob-property grob 'padding 0.3))
     (shorten-pair (ly:grob-property grob 'shorten-pair (cons 0 0)))
     (y-upper (ly:grob-property grob 'y-upper 0))
     (y-lower (ly:grob-property grob 'y-lower 0))
     (frame-X-extent (interval-widen (ly:relative-group-extent elts refp-X X) padding))
     (frame-X-extent (cons (+ (car frame-X-extent) (car shorten-pair)) (- (cdr frame-X-extent) (cdr shorten-pair))) )
     (border-width (ly:grob-property grob 'border-width 0.25))
     (yext (interval-widen (ly:relative-group-extent elts refp-Y Y) (+ padding border-width)))
     ; unlike the anaLYsis version with user-defined yext, border-width should make the boxes grow OUTward
     (color (ly:grob-property grob 'color (rgb-color 0.8  0.8  1.0)))
     (border-color (ly:grob-property grob 'border-color (rgb-color 0.3  0.3  0.9)))
     (bb-pad (ly:grob-property grob 'broken-bound-padding 4))
     (border-radius (ly:grob-property grob 'border-radius 0))
     ; (y-lower (car yext))
     ; (y-upper (cdr yext))
     (l-zigzag-width (ly:grob-property grob 'l-zigzag-width 0))
     (r-zigzag-width (ly:grob-property grob 'r-zigzag-width 0))
     (open-on-bottom (ly:grob-property grob 'open-on-bottom #f))
     (open-on-top    (ly:grob-property grob 'open-on-top #f))
     ; (hide (ly:grob-property grob 'hide "none"))   ; TODO?
     (frame-angle (ly:grob-property grob 'angle 0))
     (layer (ly:grob-property grob 'layer -10))
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
     (y-l-lower (+ (car yext) (if (number? y-lower) y-lower (car y-lower))))
     (y-r-lower (+ (car yext) (if (number? y-lower) y-lower (cdr y-lower))))
     (y-l-upper (+ (cdr yext) (if (number? y-upper) y-upper (car y-upper))))
     (y-r-upper (+ (cdr yext) (if (number? y-upper) y-upper (cdr y-upper))))
     (open-on-left
      (and (ly:spanner? grob)
           (= 1 (ly:item-break-dir (ly:spanner-bound grob LEFT)))))
     (open-on-right
      (and (ly:spanner? grob)
           (= -1 (ly:item-break-dir (ly:spanner-bound grob RIGHT)))))
     (stil empty-stencil)

     (layout (ly:grob-layout grob))
     (caption-props (ly:grob-alist-chain grob (ly:output-def-lookup layout 'text-font-defaults)))
     (caption-stencil empty-stencil)
     (caption-markup empty-markup)
     (caption-x 0)
     (caption-y 0)
     (caption-width 0)
     (caption-height 0)
     (y-with-descender 0)
     (y-without-descender 0)
     (descender-height 0)
     (temp-value 0)
     (caption-left-edge 0)
     (caption-right-edge 0)
     (caption-lower-edge 0)
     (caption-upper-edge 0)
     (caption-mid-x 0)
     (caption-angle 0)
     (caption-angle-rad 0)

     ; for rounding zigzag widths to nearest sensible value:
     (dist-y (- y-l-upper y-l-lower))
     (cnt (if (= 0 l-zigzag-width) 0 (round (/ dist-y l-zigzag-width))))
     (l-zigzag-width (if (= cnt 0) 0 (/ dist-y cnt)))
     (dist-y (- y-r-upper y-r-lower))
     (cnt (if (= 0 r-zigzag-width) 0 (round (/ dist-y r-zigzag-width))))
     (r-zigzag-width (if (= cnt 0) 0 (/ dist-y cnt)))

     ;; store polygon points.
     ;; retrieve list of all inner or outer points
     ;; pass either one out of the four point lists or the result of invoking all-points
     (inner-points
      (lambda (side)
        (if (null? side) '()
            (map car side))))
     (outer-points
      (lambda (side)
        (if (null? side) '()
            (map cdr side))))
     ;; add a pair of inner/outer points to the pts list
     (add-points (lambda (side pts) (set! side (append side (list pts)))))
     (add-corner (lambda (p side h-dir v-dir diag)
                   (let*
                    ((x-fact (if diag (* border-width (sqrt 2)) border-width))
                     (outer-point
                      (cons
                       (+ (car p) (* x-fact h-dir))
                       (+ (cdr p) v-dir))))
                    (add-points side (cons p outer-point)))))

     ;; start calculations
     (h-border-width (* border-width (sqrt 2)))  ; X-distance between left and right edges of inner and outer polygon. Must be "border-width" * sqrt 2  (Pythagoras)
     (l-width (* l-zigzag-width  0.5))   ; X-distance of zigzag corners
     (r-width (* r-zigzag-width 0.5))
     (Y-ext (cons 0 0))  ; dummy, needed for ly:stencil-expr  (is there a way without it?)
     (stencil-ext (cons (cons #f #f) (cons #f #f)))  ; will be used to set the stencil's dimensions
     ;                     ( x-lo x-hi ) ( y-lo y-hi )
     (X-ext (cons
             (if (> l-zigzag-width 0)    ; left edge has zigzag shape
                 (- (+ (car frame-X-extent) (/ l-width 2)) h-border-width)  ; Half of the zigzag space will be taken from inside, other half from the outside. Frame space taken from outside.
                 (if open-on-left  (- (car frame-X-extent) h-border-width) (- (car frame-X-extent) border-width))
                 )
             (if (> r-zigzag-width 0)   ; right edge has zigzag shape
                 (+ (- (cdr frame-X-extent) (/ r-width 2)) h-border-width)
                 (if open-on-right (+ (cdr frame-X-extent) h-border-width) (+ (cdr frame-X-extent) border-width))
                 )))
     (X-ext (cons
             (if open-on-left  (- (- (car X-ext) bb-pad) (/ l-width 2)) (car X-ext))     ; shorten/lengthen by broken-bound-bb-padding if spanner is broken
             (if open-on-right (+ (+ (cdr X-ext) bb-pad) (/ r-width 2)) (cdr X-ext))))
     ; Now X-ext represents the overall X-extent WITHOUT the zigzag attachments
     (frame-X-extent (cons
                      (- (- (car X-ext) (/ border-radius 2)) l-width)
                      (+ (+ (cdr X-ext) (/ border-radius 2)) r-width)
                      ))
     ; Now frame-X-extent represents the overall X-extent including everything...
     (points-up (list))    ; will contain coordinates for upper edge polygon
     (points-lo (list))    ; will contain coordinates for lower edge polygon
     (points-l (list))     ; will contain coordinates for left  edge polygon
     (points-r (list))     ; will contain coordinates for right edge polygon
     (points-i (list))     ; will contain coordinates for inner polygon
     (slope-upper (/ (- y-r-upper y-l-upper) (- (cdr X-ext) (car X-ext))))  ; slope of the polygon's upper edge

     (slope-lower (/ (- y-r-lower y-l-lower) (- (cdr X-ext) (car X-ext))))  ; slope of the polygon's lower edge
     (d-upper (if open-on-top    0  (* border-width (sqrt (+ (expt slope-upper 2) 1)))))  ; (Pythagoras)
     ; Y-distance between upper edges of inner and outer polygon. Equal to "border-width" if upper edge is horizontal.
     ; Increases as the upper edge's slope increases.
     (d-lower (if open-on-bottom 0  (* border-width (sqrt (+ (expt slope-lower 2) 1)))))  ; same for lower edge
     ; Where to find the center points for rotation:
     (rotation-center-x (/ (- (cdr X-ext) (car X-ext)) 2))
     (rotation-center-y (/ (+ y-l-upper y-r-upper y-l-lower y-r-lower) 4))
     (caption-left (car X-ext))
     (caption-right (cdr X-ext))
     (caption-space-factor 1)
     (caption-x-deficit 0)

     ; stuff for later calculations:
     (xtemp 0)
     (yLowerLimit 0)
     (yUpperLimit 0)
     (xp 0)
     (yp 0)
     (jumps 0)
     (need-upper-polygon (and (and (> border-width 0) (not open-on-top))    (color? border-color)))
     (need-lower-polygon (and (and (> border-width 0) (not open-on-bottom)) (color? border-color)))
     (need-left-polygon  (and (and (> border-width 0) (not open-on-left))   (color? border-color)))
     (need-right-polygon (and (and (> border-width 0) (not open-on-right))  (color? border-color)))
     (need-inner-polygon (color? color))
     (need-caption (markup? caption))

     ;; stencils to be placed on the topmost/leftmost/... border (ugly hack to set the actual X-extent):
     (top-edge-stencil empty-stencil)
     (bottom-edge-stencil empty-stencil)
     (left-edge-stencil empty-stencil)
     (right-edge-stencil empty-stencil)

     )  ; let* definitions

    ;; set grob properties that can be set from within the stencil callback
    (ly:grob-set-property! grob 'layer layer)
    (ly:grob-set-property! grob 'Y-offset 0)

    ; (calculate outer polygon's borders: )

    ; start calculating left edge borders:
    ; lower-left corner:
    (if need-left-polygon
        (begin
         (set! points-l (list (cons (car X-ext) y-l-lower)))

         ; calculate coordinates for left (outer) zigzag border:
         (if (and (> l-zigzag-width 0) (not open-on-left))
             (let loop ((cnt y-l-lower))
               (if (< cnt y-l-upper)
                   (begin
                    (if (and (< cnt y-l-upper) (> cnt y-l-lower))  ; only add to list if point is inside the given Y-range
                        (set! points-l (cons (cons    (car X-ext)             cnt                 ) points-l)))
                    (if (and (< (+ cnt (/ l-zigzag-width 2)) y-l-upper) (> (+ cnt (/ l-zigzag-width 2)) y-l-lower))
                        (set! points-l (cons (cons (- (car X-ext) l-width) (+ cnt (/ l-zigzag-width 2)) ) points-l)))
                    (loop (+ cnt l-zigzag-width))))))

         ; upper-left corner:
         (set! points-l (cons
                         (cons (car X-ext) y-l-upper)
                         points-l ))
         ))
    ; start calculating right edge borders:
    ; upper-right corner:
    (if need-right-polygon
        (begin
         (set! points-r (cons
                         (cons (cdr X-ext) y-r-upper)
                         points-r ))
         ; right outer zigzag border:
         (if (and (> r-zigzag-width 0) (not open-on-right))
             (let loop ((cnt y-r-upper))
               (if (> cnt y-r-lower)
                   (begin
                    (if (and (< cnt y-r-upper) (> cnt y-r-lower))
                        (set! points-r (cons (cons    (cdr X-ext)             cnt                  ) points-r)))
                    (if (and (< (- cnt (/ r-zigzag-width 2)) y-r-upper) (> (- cnt (/ r-zigzag-width 2)) y-r-lower))
                        (set! points-r (cons (cons (+ (cdr X-ext) r-width) (- cnt (/ r-zigzag-width 2)) ) points-r)))
                    (loop (- cnt r-zigzag-width))))))

         ; lower-right corner:
         (set! points-r (cons
                         (cons (cdr X-ext) y-r-lower)
                         points-r ))
         ))

    ; calculate lower edge borders:

    (if need-lower-polygon
        (begin
         ; lower-left corner:
         (set! points-lo (list (cons (car X-ext) y-l-lower)))
         ; upper-left corner:
         (set! points-lo (cons (cons (car X-ext) (+ y-l-lower border-width)) points-lo))
         ; upper-right corner:
         (set! points-lo (cons (cons (cdr X-ext) (+ y-r-lower border-width)) points-lo))
         ; lower-right corner:
         (set! points-lo (cons (cons (cdr X-ext) y-r-lower) points-lo))
         ))


    ; calculate upper edge borders:

    (if need-upper-polygon
        (begin
         ; lower-left corner:
         (set! points-up (list (cons (car X-ext) (- y-l-upper border-width) )))
         ; upper-left corner:
         (set! points-up (cons (cons (car X-ext) y-l-upper) points-up))
         ; upper-right corner:
         (set! points-up (cons (cons (cdr X-ext) y-r-upper) points-up))
         ; lower-right corner:
         (set! points-up (cons (cons (cdr X-ext) (- y-r-upper border-width) ) points-up))
         ))

    ; shrink X-ext for use with inner stuff:
    (if (not open-on-left)
        (if (> l-zigzag-width 0)
            (set! X-ext (cons (+ (car X-ext) h-border-width) (cdr X-ext)))
            (set! X-ext (cons (+ (car X-ext)   border-width) (cdr X-ext)))
            )
        )
    (if (not open-on-right)
        (if (> r-zigzag-width 0)
            (set! X-ext (cons (car X-ext) (- (cdr X-ext) h-border-width)))
            (set! X-ext (cons (car X-ext) (- (cdr X-ext)   border-width)))
            )
        )
    ; Now X-ext represents INNER polygon's width WITHOUT the zigzag corners.

    ; Now, finish left-edge and right-edge polygons.
    ; Use the same points to build the inner polygon.
    ; xp and yp will be the coordinates of the corner currently being calculated

    ; continue calculating left edge coordinates:

    (set! yLowerLimit y-l-lower)
    (set! yUpperLimit y-l-upper)

    ; calculate upper-left corner:
    ; (LEFT border of inner polygon = RIGHT border of left-edge polygon)
    (if open-on-left
        (begin
         (set! xp (car X-ext))
         (set! yp (- y-l-upper d-upper))
         )
        (if (> l-zigzag-width 0)
            (if (not (eq? slope-upper 1))
                (begin
                 (set! jumps 0)
                 (while (<
                         (+ (- (* slope-upper h-border-width) d-upper) (* jumps l-zigzag-width))
                         (- l-zigzag-width))
                        (set! jumps (+ jumps 1)))
                 (set! xtemp (/ (- d-upper (+ h-border-width (* jumps l-zigzag-width))) (- slope-upper 1)))
                 (if (< xtemp (- h-border-width (/ l-zigzag-width 2)))
                     (if (= -1 slope-upper)
                         (set! xtemp h-border-width)
                         (set! xtemp
                               (/ (- (- (* l-zigzag-width (+ 1 jumps)) d-upper) h-border-width) (- (- 1) slope-upper)))
                         )
                     )
                 (set! xp (+ (- (car X-ext) h-border-width) xtemp))
                 (set! yp (- (+ y-l-upper (* slope-upper xtemp)) d-upper))
                 )
                )
            (begin
             (set! xp (car X-ext))
             (set! yp (- (+ y-l-upper (* border-width slope-upper)) d-upper))
             )
            )
        )

    ; insert upper-left corner's coordinates into list:
    (if (not
         (and (and (not open-on-left) (> l-zigzag-width 0)) (eq? slope-upper 1))
         )
        (begin
         (set! points-l (cons (cons xp yp) points-l))
         (set! points-i (cons (cons xp yp) points-i))
         (set! yUpperLimit yp))
        )

    ; calculate lower-left corner:
    (if open-on-left
        (begin
         (set! xp (car X-ext))
         (set! yp (+ y-l-lower d-lower))
         )
        (if (> l-zigzag-width 0)
            (if (not (eq? slope-lower -1))
                (begin
                 (set! jumps 0)
                 (while (> (- (+ (* slope-lower h-border-width) d-lower) (* jumps l-zigzag-width)) l-zigzag-width)
                        (set! jumps (+ 1 jumps)))
                 (set! xtemp (/ (- (+ h-border-width (* jumps l-zigzag-width)) d-lower) (+ slope-lower 1)))
                 ; results from the solution for a system of two equations. Forgive me, I'm a maths teacher :-)
                 (if (< xtemp (- h-border-width (/ l-zigzag-width 2)))
                     (if (= 1 slope-lower)
                         (set! xtemp h-border-width)
                         (set! xtemp
                               (/ (+ (- d-lower (* l-zigzag-width (+ 1 jumps))) h-border-width) (- 1 slope-lower)))))  ; another system of 2 equations...
                 (set! xp (+ (- (car X-ext) h-border-width) xtemp))
                 (set! yp (+ (+ y-l-lower (* slope-lower xtemp)) d-lower))
                 )
                )
            (begin
             (set! xp (car X-ext))
             (set! yp (+ (+ y-l-lower (* border-width slope-lower)) d-lower))
             )
            )
        )

    (if (not (and (and (not open-on-left) (> l-zigzag-width 0)) (eq? slope-lower -1)))
        (set! yLowerLimit yp)
        )

    ; left (inner) zigzag:
    (if (and (> l-zigzag-width 0) (not open-on-left))
        (begin
         (let loop ((cnt y-l-upper))
           (if (> cnt y-l-lower)
               (begin
                (if (and (> cnt yLowerLimit) (< cnt yUpperLimit))
                    (begin
                     (set! points-l (cons (cons    (car X-ext)             cnt                 ) points-l))
                     (set! points-i (cons (cons    (car X-ext)             cnt                 ) points-i))
                     ))
                (if (and (> (- cnt (/ l-zigzag-width 2)) yLowerLimit) (< (- cnt (/ l-zigzag-width 2)) yUpperLimit))
                    (begin
                     (set! points-l (cons (cons (- (car X-ext) l-width) (- cnt (/ l-zigzag-width 2)) ) points-l))
                     (set! points-i (cons (cons (- (car X-ext) l-width) (- cnt (/ l-zigzag-width 2)) ) points-i))
                     ))
                (loop (- cnt l-zigzag-width))
                )
               )
           )
         )
        )

    ; insert lower-left corner (yes, AFTER the zigzag points, so all the points will be given in clockwise order):
    (if (not (and (and (not open-on-left) (> l-zigzag-width 0)) (eq? slope-lower -1)))
        (begin
         (set! points-l (cons (cons xp yp) points-l))
         (set! points-i (cons (cons xp yp) points-i))
         ))

    ; continue calculating right edge borders:

    (set! yLowerLimit y-r-lower)
    (set! yUpperLimit y-r-upper)

    ; calculate lower-right corner:
    ; (RIGHT border of inner polygon = LEFT border of right-edge polygon)
    (if open-on-right
        (begin
         (set! xp (cdr X-ext))
         (set! yp (+ y-r-lower d-lower))
         )
        (if (> r-zigzag-width 0)
            (if (not (eq? slope-lower 1))
                (begin
                 (set! jumps 0)
                 (while (> (- (- d-lower (* slope-lower h-border-width)) (* jumps r-zigzag-width)) r-zigzag-width)
                        (set! jumps (+ 1 jumps)))
                 (set! xtemp (/ (- (+ h-border-width (* jumps r-zigzag-width)) d-lower) (- slope-lower 1)))
                 (if (> xtemp (- (/ r-zigzag-width 2) h-border-width)   )
                     (if (= -1 slope-lower)
                         (set! xtemp (- h-border-width))
                         (set! xtemp
                               (/ (+ (- d-lower (* r-zigzag-width (+ 1 jumps))) h-border-width) (- -1 slope-lower)))))
                 (set! xp (+ (+ (cdr X-ext) h-border-width) xtemp))
                 (set! yp (+ (+ y-r-lower (* slope-lower xtemp)) d-lower))
                 )
                )
            (begin
             (set! xp (cdr X-ext))
             (set! yp (+ (- y-r-lower (* border-width slope-lower)) d-lower))
             )
            )
        )

    ; insert lower-right corner:
    (if (not (and (and (not open-on-right) (> r-zigzag-width 0)) (eq? slope-lower 1)))
        (begin
         (set! yLowerLimit yp)
         (set! points-r (cons (cons xp yp) points-r))
         (set! points-i (cons (cons xp yp) points-i))
         ))


    ; calculate upper-right corner:
    (if open-on-right
        (begin
         (set! xp (cdr X-ext))
         (set! yp (- y-r-upper d-upper))
         )
        (if (> r-zigzag-width 0)
            (if (not (eq? slope-upper -1))
                (begin
                 (set! jumps 0)
                 (while (<
                         (+ (- (* slope-upper (- h-border-width)) d-upper) (* jumps r-zigzag-width))
                         (- r-zigzag-width))
                        (set! jumps (+ jumps 1)))
                 (set! xtemp (/ (- d-upper (+ h-border-width (* jumps r-zigzag-width))) (+ slope-upper 1)))
                 (if (> xtemp (- (/ r-zigzag-width 2) h-border-width  ))
                     (if (= 1 slope-upper)
                         (set! xtemp (- h-border-width))
                         (set! xtemp
                               (/ (- (- (* r-zigzag-width (+ 1 jumps)) d-upper) h-border-width) (- 1 slope-upper)))
                         )
                     )
                 (set! xp (+ (+ (cdr X-ext) h-border-width) xtemp))
                 (set! yp (- (+ y-r-upper (* slope-upper xtemp)) d-upper))
                 )
                )
            (begin
             (set! xp (cdr X-ext))
             (set! yp (- (- y-r-upper (* border-width slope-upper)) d-upper))
             )
            )
        )

    (if (not
         (and (and (not open-on-right) (> r-zigzag-width 0)) (eq? slope-upper -1)))
        (set! yUpperLimit yp))

    ; right zigzag:
    (if (and (> r-zigzag-width 0) (not open-on-right))
        (begin
         (let loop ((cnt y-r-lower))
           (if (< cnt y-r-upper)
               (begin
                (if (and (> cnt yLowerLimit) (< cnt yUpperLimit))
                    (begin
                     (set! points-r (cons (cons    (cdr X-ext)             cnt                  ) points-r))
                     (set! points-i (cons (cons    (cdr X-ext)             cnt                  ) points-i))
                     ))
                (if (and (> (+ cnt (/ r-zigzag-width 2)) yLowerLimit) (< (+ cnt (/ r-zigzag-width 2)) yUpperLimit))
                    (begin
                     (set! points-r (cons (cons (+ (cdr X-ext) r-width) (+ cnt (/ r-zigzag-width 2)) ) points-r))
                     (set! points-i (cons (cons (+ (cdr X-ext) r-width) (+ cnt (/ r-zigzag-width 2)) ) points-i))
                     ))
                (loop (+ cnt r-zigzag-width))
                )
               )
           )
         )
        )

    ; insert upper-right corner:
    (if (not
         (and (and (not open-on-right) (> r-zigzag-width 0)) (eq? slope-upper -1)))
        (begin
         (set! points-r (cons (cons xp yp) points-r))
         (set! points-i (cons (cons xp yp) points-i))
         ))

    ; Edge polygons are finished now.

    (if need-caption
        (begin
         (set! caption-stencil (interpret-markup layout caption-props (markup "j")))
         (set! y-with-descender    (car (ly:stencil-extent caption-stencil Y)) )
         (set! caption-stencil (interpret-markup layout caption-props (markup "i")))
         (set! y-without-descender (car (ly:stencil-extent caption-stencil Y)) )
         (set! descender-height (- y-without-descender y-with-descender))

         (set! caption-markup
               (markup #:on-box caption-radius (if (color? caption-color) caption-color border-color)
                       #:pad-markup caption-padding
                       (if caption-keep-y
                           caption
                           (markup
                            #:combine caption
                            #:transparent
                            #:scale (cons 0.1 1)
                            #:combine "É" "j"
                            )
                           )
                       ))
         (set! caption-stencil (interpret-markup layout caption-props caption-markup))
         (set! caption-width  (- (cdr (ly:stencil-extent caption-stencil X)) (car (ly:stencil-extent caption-stencil X)) ))
         (set! caption-height (- (cdr (ly:stencil-extent caption-stencil Y)) (car (ly:stencil-extent caption-stencil Y)) ))
         (set! caption-space-factor
               (/
                (+
                 caption-right
                 (- caption-left)
                 (- (* caption-width (cos (atan (if caption-align-bottom slope-lower slope-upper))))))
                (- caption-right caption-left)
                )
               )
         (set! caption-x-deficit (* 0.5 caption-width (- 1 (cos (atan (if caption-align-bottom slope-lower slope-upper))))))
         (set! caption-x    ; cross-fade between left and right position:
               (+
                (* (/ (- 1 caption-halign) 2)   ; factor between 1 and 0  (caption-halign is between -1=left and 1=right)
                   (+ caption-left caption-padding (- (/ border-radius 2)) (- caption-x-deficit))  ; left-edge position
                   )
                (* (/ (+ 1 caption-halign) 2)   ; factor between 0 and 1
                   (+ caption-right caption-padding (/ border-radius 2) (- caption-width) caption-x-deficit)  ; right-edge position
                   )
                caption-translate-x
                )
               )
         (set! caption-y
               (+
                (* (+
                    (/ (- 1 (* caption-halign caption-space-factor)) 2)   ; factor between 1 and 0  (caption-halign is between -1=left and 1=right)
                    (/ caption-translate-x (- caption-left caption-right))
                    )
                   (if caption-align-bottom y-l-lower y-l-upper)  ; left-edge position
                   )
                (* (+
                    (/ (+ 1 (* caption-halign caption-space-factor)) 2)   ; factor between 0 and 1
                    (/ caption-translate-x (- caption-right caption-left))
                    )
                   (if caption-align-bottom y-r-lower y-r-upper)  ; right-edge position
                   )
                )
               )
         (if caption-align-bottom
             (set! caption-y (+ (- 0.04) caption-y caption-padding border-width (- (/ border-radius 2)) (- caption-height) descender-height))
             (set! caption-y (+ 0.04 caption-y caption-padding (- border-width) (/ border-radius 2) descender-height))
             )
         ; (set! caption-stencil (ly:stencil-translate caption-stencil (cons caption-x caption-y)))
         (set! caption-markup (markup #:translate (cons caption-x caption-y) caption-markup))
         (set! caption-stencil (interpret-markup layout caption-props caption-markup))

         (set! caption-left-edge  (car (ly:stencil-extent caption-stencil X)))
         (set! caption-right-edge (cdr (ly:stencil-extent caption-stencil X)))
         (set! caption-lower-edge (car (ly:stencil-extent caption-stencil Y)))
         (set! caption-upper-edge (cdr (ly:stencil-extent caption-stencil Y)))
         (set! caption-mid-x (/ (+ caption-left-edge caption-right-edge) 2))
         (set! caption-angle-rad (atan (if caption-align-bottom slope-lower slope-upper)))
         (set! caption-angle (* caption-angle-rad (/ 180 3.141592653589)))

         #!
         (set! caption-stencil (ly:stencil-rotate
                                caption-stencil
                                caption-angle
                                0
                                (if caption-align-bottom 1 -1)
                                ))
         !#
         ; ----- replaced by:
         (set! caption-markup
               (markup #:translate
                       (if caption-align-bottom
                           (cons
                            (* (sin caption-angle-rad) (/ caption-height 2))
                            (* (- 1 (cos caption-angle-rad)) (/ caption-height 2))
                            )
                           (cons
                            (- 0 (* (sin caption-angle-rad) (/ caption-height 2)))
                            (- 0 (* (- 1 (cos caption-angle-rad)) (/ caption-height 2)))
                            )
                           )
                       (markup #:rotate caption-angle caption-markup)))
         (set! caption-stencil (interpret-markup layout caption-props caption-markup))
         ; -----

         ; determine overall stencil-extent
         ; test caption corners: (top-left)
         (set! stencil-ext
               (expand-range stencil-ext
                             (rotate-point
                              (rotate-point
                               (cons caption-left-edge caption-upper-edge)
                               caption-angle caption-mid-x (if caption-align-bottom caption-upper-edge caption-lower-edge))
                              frame-angle rotation-center-x rotation-center-y)))
         ; bottom-left corner:
         (set! stencil-ext
               (expand-range stencil-ext
                             (rotate-point
                              (rotate-point
                               (cons caption-left-edge caption-lower-edge)
                               caption-angle caption-mid-x (if caption-align-bottom caption-upper-edge caption-lower-edge))
                              frame-angle rotation-center-x rotation-center-y)))
         ; top-right corner:
         (set! stencil-ext
               (expand-range stencil-ext
                             (rotate-point
                              (rotate-point
                               (cons caption-right-edge caption-upper-edge)
                               caption-angle caption-mid-x (if caption-align-bottom caption-upper-edge caption-lower-edge))
                              frame-angle rotation-center-x rotation-center-y)))
         ; bottom-right corner:
         (set! stencil-ext
               (expand-range stencil-ext
                             (rotate-point
                              (rotate-point
                               (cons caption-right-edge caption-lower-edge)
                               caption-angle caption-mid-x (if caption-align-bottom caption-upper-edge caption-lower-edge))
                              frame-angle rotation-center-x rotation-center-y)))

         #!
    (set! caption-stencil
          (ly:stencil-rotate-absolute
           caption-stencil
           frame-angle rotation-center-x rotation-center-y))
         !#
         ; ----- replaced by:
         ;   re-use caption-angle-rad:
         (set! caption-angle-rad (* frame-angle (/ 3.141592653589 180)))
         ;   re-use caption-x and caption-y as current caption center:
         (set! caption-x (/ (+ (car (ly:stencil-extent caption-stencil X)) (cdr (ly:stencil-extent caption-stencil X))) 2))
         (set! caption-y (/ (+ (car (ly:stencil-extent caption-stencil Y)) (cdr (ly:stencil-extent caption-stencil Y))) 2))

         (set! caption-markup
               (markup
                #:translate
                (cons
                 (+
                  (* (- rotation-center-x caption-x) (- 1 (cos caption-angle-rad)))
                  (* (- rotation-center-y caption-y) (sin caption-angle-rad))
                  )
                 (+
                  (* (- caption-x rotation-center-x) (sin caption-angle-rad))
                  (* (- rotation-center-y caption-y) (- 1 (cos caption-angle-rad)))
                  )
                 )
                #:rotate frame-angle caption-markup))

         (if (not set-caption-extent)
             (set! caption-markup (markup #:with-dimensions (cons 0 0) (cons 0 0) caption-markup)))

         (set! caption-stencil (interpret-markup layout caption-props caption-markup))
         ))
    ; -----

    ; determine overall stencil-extent
    ; start with frame's top-left corner:
    (set! stencil-ext
          (expand-range stencil-ext
                        (rotate-point
                         (cons (car frame-X-extent) (+ y-l-upper (/ border-radius 2)))
                         frame-angle rotation-center-x rotation-center-y)))
    ; bottom-left corner:
    (set! stencil-ext
          (expand-range stencil-ext
                        (rotate-point
                         (cons (car frame-X-extent) (- y-l-lower (/ border-radius 2)))
                         frame-angle rotation-center-x rotation-center-y)))
    ; top-right corner:
    (set! stencil-ext
          (expand-range stencil-ext
                        (rotate-point
                         (cons (cdr frame-X-extent) (+ y-r-upper (/ border-radius 2)))
                         frame-angle rotation-center-x rotation-center-y)))
    ; bottom-right corner:
    (set! stencil-ext
          (expand-range stencil-ext
                        (rotate-point
                         (cons (cdr frame-X-extent) (- y-r-lower (/ border-radius 2)))
                         frame-angle rotation-center-x rotation-center-y)))

    ; (display stencil-ext)
    ; (display "\n")

    ;; (ly:grob-set-property! grob 'X-extent (car stencil-ext))
    ;; (ly:grob-set-property! grob 'Y-extent (cdr stencil-ext))

    (set! top-edge-stencil
          (ly:stencil-translate
           (interpret-markup layout caption-props (markup #:with-dimensions (cons 0 0) (cons 0 0) " "))
           (cons 0 (cdr (cdr stencil-ext)))
           )
          )
    (set! bottom-edge-stencil
          (ly:stencil-translate
           (interpret-markup layout caption-props (markup #:with-dimensions (cons 0 0) (cons 0 0) " "))
           (cons 0 (car (cdr stencil-ext)))
           )
          )
    (set! left-edge-stencil
          (ly:stencil-translate
           (interpret-markup layout caption-props (markup #:with-dimensions (cons 0 0) (cons 0 0) " "))
           (cons (car (car stencil-ext)) 0)
           )
          )
    (set! right-edge-stencil
          (ly:stencil-translate
           (interpret-markup layout caption-props (markup #:with-dimensions (cons 0 0) (cons 0 0) " "))
           (cons (cdr (car stencil-ext)) 0)
           )
          )


    (set! stil
          (ly:stencil-add
           ; draw upper edge:
           (if need-upper-polygon
               (ly:make-stencil (list 'color border-color
                                      (ly:stencil-expr (ly:stencil-rotate-absolute
                                                        (ly:round-polygon points-up border-radius 0 #t)
                                                        frame-angle rotation-center-x rotation-center-y))
                                      X-ext Y-ext))
               empty-stencil)
           ; draw lower edge:
           (if need-lower-polygon
               (ly:make-stencil (list 'color border-color
                                      (ly:stencil-expr (ly:stencil-rotate-absolute
                                                        (ly:round-polygon points-lo border-radius 0 #t)
                                                        frame-angle rotation-center-x rotation-center-y))
                                      X-ext Y-ext))
               empty-stencil)
           ; draw left edge:
           (if need-left-polygon
               (ly:make-stencil (list 'color border-color
                                      (ly:stencil-expr (ly:stencil-rotate-absolute
                                                        (ly:round-polygon points-l  border-radius 0 #t)
                                                        frame-angle rotation-center-x rotation-center-y))
                                      X-ext Y-ext))
               empty-stencil)
           ; draw right edge:
           (if need-right-polygon
               (ly:make-stencil (list 'color border-color
                                      (ly:stencil-expr (ly:stencil-rotate-absolute
                                                        (ly:round-polygon points-r  border-radius 0 #t)
                                                        frame-angle rotation-center-x rotation-center-y))
                                      X-ext Y-ext))
               empty-stencil)
           ; draw inner polygon:
           (if need-inner-polygon
               (ly:make-stencil (list 'color color
                                      (ly:stencil-expr (ly:stencil-rotate-absolute
                                                        (ly:round-polygon points-i  border-radius 0 #t)
                                                        frame-angle rotation-center-x rotation-center-y))
                                      X-ext Y-ext))
               empty-stencil)
           ; draw caption:
           (if need-caption caption-stencil empty-stencil)
           ; invisible null-dimension markups to set stencil extent:
           (if set-top-edge top-edge-stencil empty-stencil)
           (if set-bottom-edge bottom-edge-stencil empty-stencil)
           (if set-left-edge left-edge-stencil empty-stencil)
           (if set-right-edge right-edge-stencil empty-stencil)
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
  \override Score.MusicBoxer.broken-bound-padding = 5
  \override Score.MusicBoxer.layer = -10
  \override Score.MusicBoxer.padding = 0.3
  \override Score.MusicBoxer.border-width = 0.3
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

  \once \override Score.MusicBoxer.caption = "line break"
  \once \override Score.MusicBoxer.caption-align-bottom = ##t
  \musicBoxerStart
  d4 e f
  \break
  f4 e8 d \musicBoxerEnd e4
  \override Score.MusicBoxer.border-color = #(rgb-color 1 0.4 0.0)
  \override Score.MusicBoxer.color = #(rgb-color 1 0.9 0.8)
  \once \override Score.MusicBoxer.caption = "inversion"
  \musicBoxerStart
  e4 e f
  \musicBoxerEnd
  g2.
  \override Score.MusicBoxer.border-color = #red
  \override Score.MusicBoxer.color = #(rgb-color 1 0.8 0.8)
  \once \override Score.MusicBoxer.caption = "motif"
  \musicBoxerStart
  e4 e d
  \musicBoxerEnd
  c2.
  \section
  \once \override Score.MusicBoxer.r-zigzag-width = 1.0
  % \once
  \once \override Score.MusicBoxer.shorten-pair = #'(0 . 0.2)
  \musicBoxerStart
  e4
  \musicBoxerEnd
  e16 f g e
  \once \override Score.MusicBoxer.l-zigzag-width = 1.0
  \once \override Score.MusicBoxer.shorten-pair = #'(0.2 . 0)
  \musicBoxerStart
  d4
  \musicBoxerEnd
  c2.
  \section
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
  \once \override Score.MusicBoxer.acknowledge-finger-interface = ##t
  \once \override Score.MusicBoxer.border-radius = #1
  
  \musicBoxerStart c,4-1 e-2 g-3 \musicBoxerEnd c-5
  \box <c-5 g-3 e-2 c-1>1  \f \fermata
  \once \override Score.Box.acknowledge-script-interface = ##t
  \once \override Score.Box.acknowledge-finger-interface = ##t
  \box <c-5 g-3 e-2 c-1>1  \f \fermata
  
  \break
  
  \time 3/4
  \override Score.MusicBoxer.border-color = #(rgb-color 1 0.4 0.0)
  \override Score.MusicBoxer.color = #(rgb-color 1 0.9 0.8)
  \once \override Score.MusicBoxer.caption = "up:"
  \once \override Score.MusicBoxer.y-upper = #'(-1 . 0)
  \once \override Score.MusicBoxer.y-lower = #'(0  . 1)
  \once \override Score.MusicBoxer.caption-translate-x = #0.5
  \musicBoxerStart
  e,4^\markup \column { " "    "manually moving corners to indicate melody direction:"    " "     " " }
  e f
  \musicBoxerEnd
  g2.
  \override Score.MusicBoxer.border-color = #red
  \override Score.MusicBoxer.color = #(rgb-color 1 0.8 0.8)
  \once \override Score.MusicBoxer.caption = "down:"
  \once \override Score.MusicBoxer.y-upper = #'(0 . -1)
  \once \override Score.MusicBoxer.y-lower = #'(1 .  0)
  \musicBoxerStart
  e4 e d
  \musicBoxerEnd
  c2.
  R2.*3
}

\score {
  \new Staff { \melody \break \another }
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



