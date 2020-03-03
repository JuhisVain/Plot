(ql:quickload :lispbuilder-sdl)

(defun gaussian (x height center width)
  (expt (* height
	   (exp 1.d0)) ; aka. "e"
	(- (/ (expt (- x center)
		    2)
	      (* 2 (expt width 2))))))

(defun gauss (x)
  (gaussian x 1 0 150))

(defun testfun (x) ; breaks from 0 to 0.1
  (if (<= x 400)
      (+ 0.5 (/ (cos (/ x 127.324)) 2))
      0))

;; It would be cool if methods could specialize on ftypes
(defun get-arg-count (func)
  "Returns count of number arguments that FUNC accepts,
one of (1 2 NIL)"
  (declare (optimize (safety 3)))
  (handler-case
      (progn
	(funcall func 13)
	1)
    (simple-condition ()
      (handler-case
	  (progn
	    (funcall func 13 13)
	    2)
	(simple-condition () NIL)))))

(defun draw-pixel (x y surface color)
  (sdl:draw-pixel-* x (- (sdl:height surface) y)
		    :surface surface
		    :color color))

(defun draw-circle (x y radius surface color)
  (sdl:draw-circle-* x (- (sdl:height surface) y) radius
		     :surface surface
		     :color color))

(defun draw-horizontal (y color surface &key (mark nil))
  (let ((translated-y (- (sdl:height surface) y)))
    
    ;; If zero axis too for away sdl:draw-line might crap out:
    (unless (< -1 translated-y (sdl:height surface))
      (return-from draw-horizontal))
    
    (sdl:draw-line-* 0 translated-y
		     (sdl:width surface) translated-y
		     :surface surface
		     :color color)
    (typecase mark
      (string (sdl:draw-string-solid-* mark 1 (+ translated-y 2)
				       :surface surface
				       :color color)))))

(defun draw-vertical (x color surface &key (mark nil))
  
  (unless (< -1 x (sdl:width surface))
    (return-from draw-vertical))
  
  (sdl:draw-line-* x 0
		   x (sdl:height surface)
		   :surface surface
		   :color color)
  (typecase mark
    (string (sdl:draw-string-solid-* mark (+ x 2) (- (sdl:height surface) 8)
				     :surface surface
				     :color color))))

(defun mark-lines (range)
  "Returns something by gut feeling to be used as multiplier of grid lines."
  (if (zerop range)
      1
      ;; decrement rounded order of magnitude and get its value:
      (expt 10 (1- (round (log range 10))))))

(defun rgb-to-hue (red green blue)
  "Extracts hue from RGB values.
In hue circle red is at 0 and going clockwise next is green and then blue."
  (declare ((integer 0 255) red green blue))
  (atan (* (sqrt 3)
	   (- green blue))
	(- (* 2 red)
	   green
	   blue)))

;; Looks like this works on negative values too
(defun hue-to-rgb (hue)
  "Returns RGB plist based on HUE in radians when saturation
and value are both at max."
  (declare (float hue))
  (flet ((voodoo (n)
	   (let ((k (mod (+ n (/ hue (/ pi 3)))
			 6)))
	     (round
	      (* 255
		 (- 1 (max
		       0
		       (min
			k
			(- 4 k)
			1))))))))
    (list :r (voodoo 5)
	  :g (voodoo 3)
	  :b (voodoo 1))))

(defun generate-colors (red green blue count)
  (let ((start-hue (rgb-to-hue red green blue))
	(hue-step (/ (* 2 pi)
		     count))
	(colors nil))
    (dotimes (iter count)
      (push (hue-to-rgb
	     (+ start-hue (* iter hue-step)))
	    colors))
    (reverse colors)))

(defstruct plotfunc
  (function) ; master function
  (subs)) ; keys, accessors or whatever to be called with master's value

(defun plotfunc-evaluate (plotfunc x)
  (let ((fvalue (funcall (plotfunc-function plotfunc) x)))
    (mapcar #'(lambda (sub)
		(plotcall sub fvalue))
	    (plotfunc-subs plotfunc))))

(defun plottable-length (func-list)
  "Counts the amount of functions to be plotted in FUNC-LIST."
  (labels ((rec-plot-len (flist sum)
	     (etypecase (car flist)
	       (null sum)
	       (plotfunc (rec-plot-len (plotfunc-subs (car flist)) sum))
	       (function (rec-plot-len (cdr flist) (1+ sum))))))
    (rec-plot-len func-list 0)))

(defun abs-max (number)
  "Get greatest value component of NUMBER."
  (declare (number number))
  (if (complexp number)
      (max (imagpart number)
	   (realpart number))
      number))

(defun abs-min (number)
  "Get least value component of NUMBER."
  (declare (number number))
  (if (complexp number)
      (min (imagpart number)
	   (realpart number))
      number))

(defun draw-value (x-coord value y-scale slack screen-y0 surface color-set)
  (typecase value
    (real (draw-pixel (round x-coord)
		      (round (- (+ (* value y-scale)
				   slack)
				screen-y0))
		      surface (car color-set)))
    ;;the components of a complex must be real:
    (complex (draw-value x-coord
			 (realpart value)
			 y-scale slack screen-y0 surface (cdr color-set))
	     (draw-value x-coord
			 (imagpart value)
			 y-scale slack screen-y0 surface (cddr color-set)))
    (list (dolist (sub value)
	    (draw-value x-coord
			sub
			y-scale slack screen-y0 surface color-set)))
    (t (draw-vertical x-coord ; bad value, most likely zero div
		      (sdl:color :r 100 :g 0 :b 0)
		      surface)))
  NIL)

(defun plotcall (function &rest arguments)
  "Funcall with handlers etc. for plottable data."
  (handler-case
      (apply function arguments)
    (division-by-zero () 'ZERO-DIVISION)))

(defun draw-function (func-list
		      min-x max-x
		      slack
		      &optional
			(surface sdl:*default-display*))
  "Graphs functions in FUNC-LIST from MIN-X to MAX-X, y-scaling is
dynamic based on extreme values on X's range."
  
  (let* ((color-list
	  (mapcar #'(lambda (rgb)
		      (list
		       ;; default color for normie numbers:
		       (apply #'sdl:color rgb)
		       ;; complex realpart color:
		       (apply #'sdl:color
			      (mapcar #'(lambda (x);lighten
					  (if (numberp x)
					      (round (+ x 255) 2)
					      x))
				      rgb))
		       ;; complex imagpart color:
		       (apply #'sdl:color
			      (mapcar #'(lambda (x);darken
					  (if (numberp x)
					      (round x 2)
					      x))
				      rgb))
		       ))
		  (generate-colors 0 0 0 (length func-list))))
	 (win-width (sdl:width surface))
	 (win-height (sdl:height surface))
	 (x-range (- max-x min-x))
	 (x-grid-step (mark-lines x-range)) ; Used for grid lines
	 (x-scale (/ win-width x-range))
	 (x-step (/ x-range win-width)) ; rational, used to iterate arguments
	 (screen-x0 (* min-x (/ win-width x-range)))
	 )

    (multiple-value-bind (max-y
			  min-y
			  y-values)
	(loop
	   for x from min-x upto max-x by x-step
	   with max-y = most-negative-fixnum
	   and min-y = most-positive-fixnum
	   collect

	     (loop for y in
		  (mapcar #'(lambda (func)
			      (if (plotfunc-p func)
				  (plotfunc-evaluate func x)
				  (plotcall func x)))
			  func-list)
		if (listp y) ; now this is functional programming!
		do (setf max-y (apply #'max
				      (cons max-y
					    (mapcar #'abs-max
						    (remove-if #'symbolp y))))
			 min-y (apply #'min
				      (cons min-y
					    (mapcar #'abs-min
						    (remove-if #'symbolp y)))))
		else if (numberp y)
		do (setf max-y (max max-y (abs-max y))
			 min-y (min min-y (abs-min y)))
		collect y into step-y-values
		  
		finally (return step-y-values))
	   into y-values
	   finally (return (values max-y min-y y-values)))

      (format t "max ~a min ~a, first: ~a~%" max-y min-y (car y-values))

      (let* ((pre-y-range (- max-y min-y)) ; range in value
	     (slack-mod (* pre-y-range slack)) ; total visible range in value
	     (y-range (+ pre-y-range slack-mod))
	     (y-grid-step (mark-lines y-range))
	     ;;handle funcs which always return the same value within range:
	     (y-scale (if (zerop y-range)
			  100
			  (/ win-height y-range)))
	     (slack-pixels (* 1/2 slack-mod y-scale)) ; pixels to add at y-extremes
	     ;; screen-y0 is the location of actual y=0 line in relation to low
	     ;; border of window and inverted.
	     ;; If func produces 0 ...-> negative numbers and window is 500
	     ;; tall, screen-y0 will be -500 etc..
	     (screen-y0 (* min-y
			   y-scale)))

	(when (zerop y-range)
	  (setf screen-y0 (/ win-height -2)))

	;;debug:
	(format t "y-range ~a, ~a~%slack-mod ~a, ~a~%y-scale ~a~%
screen-y0 ~a and x0 ~a, x-scale: ~a~%"
		pre-y-range
		y-range
		slack-mod
		(* slack-mod y-scale)
		y-scale
		screen-y0
		screen-x0
		x-scale)

;;; Draw horizontal grid:
	
	(loop for y from (- min-y (rem min-y y-grid-step))
	   ;; range increased by one line so grid
	   ;; extends to all values even with slack:
	   to (+ max-y y-grid-step) by y-grid-step
	   do (draw-horizontal (round (+ (* y y-scale)
					 (- screen-y0)
					 slack-pixels))
			       (sdl:color :r 50 :g 50 :b 50)
			       surface
			       :mark (format nil "~a" (float y))))

;;; Draw vertical grid:

	(loop for x from (- min-x (rem min-x x-grid-step))
	   to max-x by x-grid-step
	   do (draw-vertical (round (- (* x x-scale)
				       screen-x0))
			     (sdl:color :r 50 :g 50 :b 50)
			     surface
			     :mark (format nil "~a" (float x))))
	
	(draw-horizontal (round (+ (- screen-y0)
				   slack-pixels
				   ))
			 (sdl:color :r 150 :g 150 :b 150)
			 surface
			 :mark "0")

	(draw-vertical (round (- screen-x0))
		       (sdl:color :r 150 :g 150 :b 150)
		       surface
		       :mark "0")

	
	;; Draw the function:
	(loop for x from 0 below win-width
	   for y-list in y-values
	     
	   do (loop for y in y-list
		 for color-set in color-list

		 do (draw-value x y y-scale slack-pixels screen-y0
				surface color-set)
		   ))))))


;; Let's go with elements in func-list as (func key-list) or just func
;; key-list is list of functions to be applied to func's result
;;for example:
;; (plot (list
;;         (list #'sqrt #'imagpart #'realpart)
;;         (list #'log #'imagpart #'realpart))
;;       :from -11 :to -1)
(defun plot (func-list &key (from 0) (to 100) (slack 1/20) (window-width 500) (window-height 500))
  (declare ((rational 0 1) slack))
  (sdl:initialise-default-font)
  (sdl:with-init()
    (sdl:window window-width window-height
		:title-caption "plot"
		:sw t)
    ;;(setf (sdl:frame-rate) 30)

    (draw-function
     (mapcar #'(lambda (func)
		 (if (listp func)
		     (make-plotfunc :function (car func)
				    :subs (cdr func))
		     func))
	     func-list)
     from to slack)

    (sdl:update-display)
    
    (sdl:with-events (:poll)
      (:quit-event
       () t)

      (:idle
       ()
       ))))
