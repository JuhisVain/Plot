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
	 (x-step (/ x-range win-width)) ; rational
	 (screen-x0 (* min-x (/ win-width x-range)))
	 )
    (multiple-value-bind
	  (max-y min-y x-values y-values)
	(loop
	   for x from min-x upto max-x by x-step
	   for y = (funcall func x)
	   maximize y into max-y
	   minimize y into min-y
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

	(format t "y-range ~a, ~a~%slack-mod ~a, ~a~%y-scale ~a~%screen-y0 ~a~%"
		pre-y-range
		y-range
		slack-mod
		(* slack-mod y-scale)
		y-scale
		screen-y0)

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
	
	(loop for x from 0 below win-width
	   for y in y-values
	   do
	     (draw-pixel (round x)
			 (round (- (+ (* y y-scale)
				      slack-pixels)
				   screen-y0))
			 surface color))))))


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
