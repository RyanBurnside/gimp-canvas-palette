;;; Ryan Burnside 2015
;;; This is a module to draw a smooth gradient that may be used
;;; as a palette to select from when shading from one color to another
;;; It will be done on the active image and fill the current selection 
;;; rectangle

;; The user is expected to use the eyedropper to grab what they need
;; directly from the image


;; Find at File/Create/Gradient_Palette

(script-fu-register
 "script-fu-make-palette"                                    ;func name
 "Make Palette"                                              ;menu label
 "Uses selection to make a smooth palette between 4 colors"
 "Ryan Burnside"                                             ;author
 "No copyright protection"                                   ;copyright
 "October 20, 2015"                                          ;date
 ""                                                          ;image type

;; Widgets and top level parameters
 SF-IMAGE       "Image"          0    
 SF-DRAWABLE    "Drawable"       0
 SF-COLOR       "Upper Left"    '(255 0 0)
 SF-COLOR       "Upper Right"   '(0 255 255)
 SF-COLOR       "Lower Left"    '(0 255 0)
 SF-COLOR       "Lower Right"   '(0 0 255)
 SF-ADJUSTMENT  "Steps Across"  '(7 2 16 1 1 0 SF-SLIDER)
 SF-ADJUSTMENT  "Steps Down"    '(7 2 16 1 1 0 SF-SLIDER)
 SF-TOGGLE      "Perception Correction" 0
 SF-OPTION      "Interpolation" '("linear" "cosine" "smoothstep" "custom-1"))

;; Register the main function
(script-fu-menu-register "script-fu-make-palette" 
			 "<Image>/File/Create/Gradient_Palette")

;; Main function
(define (script-fu-make-palette the-image the-drawable color-1 color-2
				color-3 color-4 num-steps-across
				num-steps-down use-correction interpolation)


  ;; The following 2 functions are for corrective (proper) color averaging
  (define (square-color color)
    (map (lambda (n) (* n n)) color))

  (define (square-root-color color)
    (map sqrt color))

  ;;Function to blend two numbers with a percent like .5 etc
  (define (linear-interpolation start end percent)
    (let ((range (- end start)))
      (+ start (* range percent))))


  (define (custom-1-interpolation start end percent)
    (let* ((pi (* 4.0 (atan 1.0)))
	   (new-percent (cos (* percent pi))))
      (linear-interpolation start end new-percent)))
  
  ;; TODO check this
  (define (cosine-interpolation start end percent)
    (let* ((pi (* 4 (atan 1.0)))
	   (angle (* pi percent))
	   (numer (- (cos angle)))
	   (frac (/ numer 2.0))
	   (new-percent (+ frac .5)))
      (linear-interpolation start end new-percent)))

  ;; Smoothstep algorithm 
  (define (smoothstep-interpolation start end percent)
    (let* ((inner (- 3 (* 2.0 percent)))
	   (new-percent (expt percent 2)))
      (linear-interpolation start end new-percent)))
	   
  ;; Function to interpolate colors
  (define (color-interpolate col1 col2 percent func)
    ; Provide for the 2 ways of averaging color, mathy way, corrective way
    (if (= use-correction 0)
	; No correction (crude way)
	(list
	 (func (car   col1) (car   col2) percent)
	 (func (cadr  col1) (cadr  col2) percent)
	 (func (caddr col1) (caddr col2) percent))
	; Squaring products then square rooting result
	(list
	 (sqrt (func (expt (car   col1) 2) (expt (car   col2) 2) percent))
	 (sqrt (func (expt (cadr  col1) 2) (expt (cadr  col2) 2) percent))
	 (sqrt (func (expt (caddr col1) 2) (expt (caddr col2) 2) percent)))))
    
  ;; Function to draw a single box, messes with context color and selection
  (define (draw-box x y width height color)
    (let ((rounded-color (map round color)))
      (gimp-context-push)
      (gimp-image-select-rectangle the-image 2 x y width height)
      (gimp-context-set-foreground rounded-color)
      (gimp-edit-fill the-drawable 0)
      (gimp-context-pop)))

  ;; Use recursion to draw boxes for the palette
  (define (draw-boxes-down x y width height color color2 num-boxes func
			   . current-number)
    (let ((n (if (null? current-number) 0 (car current-number))))
      (cond ((< n num-boxes)
	     (let* ((p (/ n (- num-boxes 1)))
		    (c (color-interpolate color color2 p func)))
	       (draw-box x y width height c)
	       (draw-boxes-down x 
				(+ y height)
				width 
				height 
				color 
				color2 
				num-boxes
				func
				(+ n 1)))))))

  ;; Draw the columns which call draw-boxes-down
  (define (draw-columns x y width height func . current-column)
    (let* ((c (if (null? current-column) 0 (car current-column)))
	   (percent (/ c (- num-steps-across 1)))
	   (top-color (color-interpolate color-1 color-2 percent func))
	   (bottom-color (color-interpolate color-3 color-4 percent func)))
      (cond ((< c num-steps-across)
	     (draw-boxes-down x
			      y
			      width
			      height
			      top-color
			      bottom-color
			      num-steps-down
			      func)
	     (draw-columns (+ x width) y width height func (+ c 1))))))

  ;; Now some local bindings and the main call to draw-boxes
    ;; Catch and adjust interpolation given what was selected on the menu
  (cond ((= interpolation 0)
	 (set! interpolation linear-interpolation))
	((= interpolation 1)
	 (set! interpolation cosine-interpolation))
	((= interpolation 2)
	 (set! interpolation smoothstep-interpolation))
	((= interpolation 3)
	 (set! interpolation custom-1-interpolation)))

  (let* ((bounds (gimp-selection-bounds the-image))
	 (selected? (car bounds)) ; Super gross, should not be number?
	 (x (cadr bounds))
	 (y (caddr bounds))
	 (x2 (cadddr bounds))
	 (y2 (cadr (cdddr bounds)))
	 (width (- x2 x))
	 (height (- y2 y))
	 (x-step (floor (/ width  num-steps-across 1)))
	 (y-step (floor (/ height num-steps-down 1)))
	 (can-continue #t))
    
    (cond ((or (< width num-steps-across) (< height num-steps-down))
	   (gimp-message "Your have more slices than rectangle space!")
	   (set! can-continue #f)))

    (cond ((= selected? 0)
	   (gimp-message "You have not selected an area!")
	   (set! can-continue #f)))

    (cond (can-continue
	   ;; Group all the fussing around into 1 undo group
	   (gimp-image-undo-group-start the-image)
	   (draw-columns x y x-step y-step interpolation)
	   (gimp-selection-none the-image)
	   (gimp-displays-flush)
	   (gimp-image-undo-group-end the-image)))))

