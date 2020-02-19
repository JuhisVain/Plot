(ql:quickload :lispbuilder-sdl)

(defun gaussian (x height center width)
  (expt (* height
	   (exp 1.d0)) ; aka. "e"
	(- (/ (expt (- x center)
		    2)
	      (* 2 (expt width 2))))))

(defun gauss (x)
  (gaussian x 1 0 150))

;; It would be cool if methods could specialize on ftypes
(defun get-arg-count (func)
  "Returns count of number arguments that FUNC accepts,
one of (1 2 NIL)"
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
  "Returns something by gut feeling to be used a multiplier of grid lines."
  ;; decrement rounded order of magnitude and get its value:
  (expt 10 (1- (round (log range 10)))))

(defun draw-function (func
		      min-x max-x
		      slack
		      &optional
			(color sdl:*white*) 
			(surface sdl:*default-display*))
  "Graphs (function (real) real) FUNC from MIN-X to MAX-X, y-scaling is dynamic based on
extreme values on X's range."
  (declare (function func))
  
  (when (or (>= min-x max-x)
	    (/= (get-arg-count func)
		1))
    (error "Invalid args"))

  (let* ((win-width (sdl:width surface))
	 (win-height (sdl:height surface))
	 (x-range (- max-x min-x))
	 (x-scale (/ win-width x-range))
	 (x-step (/ x-range win-width)) ; rational
	 (screen-x0 (* min-x (/ win-width x-range)))
	 )
    (multiple-value-bind
	  (max-y min-y x-values y-values)
	(loop
	   for x from min-x upto max-x by x-step
	   for y = (handler-case
		       (funcall func x)
		     (division-by-zero () 'ZERO-DIVISION))
    ;;; NOTE: these are extremes in drawable dataset, not actual values on range
	   if (realp y)
	   maximize y into max-y
	   and minimize y into min-y
	   else if (complexp y)
	   maximize (max (realpart y) (imagpart y)) into max-y
	   and minimize (min (realpart y) (imagpart y)) into min-y
	   collect x into x-values
	   collect y into y-values
	   finally (return (values max-y min-y x-values y-values)))

      (format t "max ~a min ~a~%" max-y min-y)

      (let* ((pre-y-range (- max-y min-y)) ; range in value
	     (slack-mod (* pre-y-range slack)) ; total visible range in value
	     (y-range (+ pre-y-range slack-mod))
	     (y-scale (/ win-height y-range))
	     (slack-pixels (* 1/2 slack-mod y-scale)) ; pixels to add at y-extremes
	     ;; screen-y0 is the location of actual y=0 line in relation to low
	     ;; border of window and inverted.
	     ;; If func produces 0 ...-> negative numbers and window is 500
	     ;; tall, screen-y0 will be -500 etc..
	     (screen-y0 (* min-y
			   y-scale)
			   ))

	;;debug:
	(format t "y-range ~a, ~a~%slack-mod ~a, ~a~%y-scale ~a~%screen-y0 ~a and x0 ~a~%"
		pre-y-range
		y-range
		slack-mod
		(* slack-mod y-scale)
		y-scale
		screen-y0
		screen-x0)

	(format t "looping for y from 0 to ~a by ~a~%"
		(+ max-y (mark-lines y-range))
		(mark-lines y-range))

	;; draw horizontal grid, first positives then negatives:
	(loop for y from 0
	   ;; range increased by one line so grid
	   ;; extends to all values even with slack:
	   to (+ max-y (mark-lines y-range)) by (mark-lines y-range)
	   do (draw-horizontal (round (+ (* y y-scale)
					 (- screen-y0)
					 slack-pixels))
			       (sdl:color :r 50 :g 50 :b 50)
			       surface
			       :mark (format nil "~a" (float y))
			       ))

	(loop for y from 0
	   downto (- min-y (mark-lines y-range)) by (mark-lines y-range)
	   do (draw-horizontal (round (+ (* y y-scale)
					 (- screen-y0)
					 slack-pixels))
			       (sdl:color :r 50 :g 50 :b 50)
			       surface
			       :mark (format nil "~a" (float y))
			       ))

	;; draw vertical grid:
	(loop for x from 0
	   to max-x by (mark-lines x-range)
	   do (draw-vertical (round (- (* x x-scale)
				       screen-x0))
			       (sdl:color :r 50 :g 50 :b 50)
			       surface
			       :mark (format nil "~a" (float x))))

	(loop for x from 0
	   downto min-x by (mark-lines x-range)
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
	   for y in y-values
	   
	   
	   if (realp y)
	   do (draw-pixel (round x)
			  (round (- (+ (* y y-scale)
				       slack-pixels)
				    screen-y0))
			  surface color)
	   else
	   if (complexp y)
	   do (draw-pixel (round x)
			  (round (- (+ (* (realpart y) y-scale)
				       slack-pixels)
				    screen-y0))
			  surface sdl:*blue*)
	     (draw-pixel (round x)
			 (round (- (+ (* (imagpart y) y-scale)
				       slack-pixels)
				    screen-y0))
			  surface sdl:*red*)
	   else
	   if (eq y 'ZERO-DIVISION)
	   do (draw-circle (round x)
			   ;; The following value is usually outside view:
			   (round
			    (- (+ (* y-scale
				     (funcall
				      func
				      (+ x LEAST-POSITIVE-DOUBLE-FLOAT)))
				  slack-pixels)
			       screen-y0))
			   2 ; circle radius
			   surface color)
	     
	   )))))


(defun plot (func &key (from 0) (to 100) (slack 1/20) (window-width 500) (window-height 500))
  (declare ((function (number) number) func)
	   ((rational 0 1) slack))
  (sdl:initialise-default-font)
  (sdl:with-init()
    (sdl:window window-width window-height
		:title-caption "plot"
		:sw t)
    ;;(setf (sdl:frame-rate) 30)

    (draw-function func from to slack sdl:*white*)

    (sdl:update-display)
    
    (sdl:with-events (:poll)
      (:quit-event
       () t)

      (:idle
       ()
       ))))
