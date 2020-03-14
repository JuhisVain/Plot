(ql:quickload :lispbuilder-sdl)

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
		 (- 1 (max 0 (min k
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

(defstruct funcdata
  (function nil :type function)
  (color-real)
  (color-realpart)
  (color-imagpart)
  (label nil :type string))

(defun aux-colors (rgb-plist)
  "Returns sdl:colors to be used to draw reals, realparts and imagparts.
RGB-PLIST shoould be a property list like (:r x :g x :b x),
where the Xs are (integer 0 255)."
  (values (apply #'sdl:color rgb-plist)
	  (apply #'sdl:color
		 (mapcar #'(lambda (x) (typecase x
					 (number (round (+ x 255) 2))
					 (t x)))
			 rgb-plist))
	  (apply #'sdl:color
		 (mapcar #'(lambda (x) (typecase x
					 (number (round x 2))
					 (t x)))
			 rgb-plist))))

(defun to-plotfunc (funcdata-list)
  "Stores multipart functions' parts into plotfunc structures."
  (mapcar #'(lambda (fdata)
	      (typecase fdata
		(funcdata fdata)
		(list
		 (make-plotfunc
		  :function (car fdata)
		  :subs (to-plotfunc (cdr fdata))))))
	  funcdata-list))

;;feed to above
(defun to-funcdata (input-func-list)
  (let ((color-stack
	 (generate-colors
	  255 0 0
	  (plottable-count input-func-list))))
    (labels ((funcdata-gen (list id &key (sub-of nil) (is-master nil))
	       (mapcar #'(lambda (func)
			   (prog1 ; don't return incremented id
			       (typecase func
				 (function
				  (multiple-value-bind
					(real realpart imagpart)
				      (if is-master
					  (values nil nil nil)
					  (aux-colors (pop color-stack)))
				    (make-funcdata
				     :function func
				     :color-real real
				     :color-realpart realpart
				     :color-imagpart imagpart
				     :label (concatenate 'string
							 sub-of
							 (when sub-of "-")
							 (format nil "~a" id)))))
				 (symbol
				  (multiple-value-bind
					(real realpart imagpart)
				      (if is-master
					  (values nil nil nil)
					  (aux-colors (pop color-stack)))
				    (make-funcdata
				     :function (symbol-function func)
				     :color-real real
				     :color-realpart realpart
				     :color-imagpart imagpart
				     :label (concatenate 'string
							 sub-of
							 (when sub-of "-")
							 (symbol-name func)))))
				 (list
				  (append
				   (funcdata-gen (list (car func))
						 id
						 :sub-of sub-of
						 :is-master t)
				   (funcdata-gen
				    (cdr func) 0
				    :sub-of
				    (concatenate 'string ; master's name
						 sub-of
						 (when sub-of "-")
						 (if (functionp (car func))
						     (format nil "~a" id)
						     (symbol-name (car func))))))))
			     (incf id)))
		       list)))
      (funcdata-gen input-func-list 0))))

(defun plotcall (function &rest arguments)
  "Funcall with handlers etc. for plottable data."
  (handler-case
      (apply (funcdata-function function) arguments)
    (division-by-zero () 'ZERO-DIVISION)))

(defun plotfunc-evaluate (plotfunc x)
  (let ((fvalue (plotcall (plotfunc-function plotfunc) x)))
    ;; plotcall might produce a symbol to represent an error
    ;; which the user might want to handle himself...
    (mapcar #'(lambda (sub)
		(etypecase sub
		  (funcdata
		   (plotcall sub fvalue))
		  (plotfunc (plotfunc-evaluate sub x))))
	    (plotfunc-subs plotfunc))))

(defun plottable-count (func-list)
  (labels ((rec-plot-len (flist sum)
	     (typecase (car flist)
	       (null sum)
	       (list 
		(rec-plot-len
		 (cdr flist)
		 (rec-plot-len (cdar flist) sum)))
	       (symbol
		(if (fboundp (car flist))
		    (rec-plot-len (cdr flist) (1+ sum))
		    (rec-plot-len (cdr flist)
				  (rec-plot-len
				   (list (symbol-value (car flist)))
				   sum))))
	       (function (rec-plot-len (cdr flist) (1+ sum))))))
    (rec-plot-len func-list 0)))

(defun plottable-length (func-list)
  "Counts the amount of functions to be plotted in FUNC-LIST."
  (labels ((rec-plot-len (flist sum)
	     (typecase (car flist)
	       (null sum)
	       (plotfunc
		(rec-plot-len
		 (cdr flist)
		 (rec-plot-len (plotfunc-subs (car flist)) sum)))
	       (function (rec-plot-len (cdr flist) (1+ sum))))))
    (rec-plot-len func-list 0)))

(defun extract-numbers (tree)
  "Get flat list of numbers in TREE."
  (loop for element in tree
     append (cond ((listp element)
		   (extract-numbers element))
		  ((numberp element)
		   (list element)))))

(defun complex-max (&rest numbers)
  "Like max, but works on complex numbers returning greatest component."
  (declare (ftype (function (&rest number) real)))
  (apply #'max
	 (mapcar #'(lambda (num)
		     (if (complexp num)
			 (max (imagpart num)
			      (realpart num))
			 num))
		 numbers)))

(defun complex-min (&rest numbers)
  "Like min, but works on complex numbers returning least component."
  (declare (ftype (function (&rest number) real)))
  (apply #'min
	 (mapcar #'(lambda (num)
		     (if (complexp num)
			 (min (imagpart num)
			      (realpart num))
			 num))
		 numbers)))

(defun draw-value (x-coord value y-scale slack screen-y0 surface pfunc)
  (typecase value
    (real (draw-pixel (round x-coord)
		      (round (- (+ (* value y-scale)
				   slack)
				screen-y0))
		      surface
		      (typecase pfunc
			(funcdata
			 (funcdata-color-real pfunc))
			(sdl:color
			 pfunc))))
    ;;the components of a complex must be real:
    (complex (draw-value x-coord
			 (realpart value)
			 y-scale slack screen-y0 surface
			 (funcdata-color-realpart pfunc))
	     (draw-value x-coord
			 (imagpart value)
			 y-scale slack screen-y0 surface
			 (funcdata-color-imagpart pfunc)))
    (list
     (do ((value-head value (cdr value-head))
	  (sub-pfunc-head (plotfunc-subs pfunc) (cdr sub-pfunc-head))
	  ;;(color-set-head color-set (cdr color-set-head))
	  )
	 ((null value-head))
       (draw-value x-coord
		   (car value-head)
		   y-scale slack screen-y0 surface (car sub-pfunc-head))))
    (t (draw-vertical x-coord ; bad value, most likely zero div
		      (sdl:color :r 100 :g 0 :b 0)
		      surface)))
  NIL)

(defun draw-function (input-func-list
		      min-x max-x
		      slack
		      &optional
			(surface sdl:*default-display*))
  "Graphs functions in FUNC-LIST from MIN-X to MAX-X, y-scaling is
dynamic based on extreme values on X's range."
  
  (let* ((pfunc-list (to-plotfunc (to-funcdata input-func-list)))
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
			  pfunc-list)
		if (listp y) ; now this is functional programming!
		do (setf max-y (apply #'complex-max
				      (cons max-y (extract-numbers y)))
			 min-y (apply #'complex-min
				      (cons min-y (extract-numbers y))))
		else if (numberp y)
		do (setf max-y (complex-max max-y y)
			 min-y (complex-min min-y y))
		collect y into step-y-values
		  
		finally (return step-y-values))
	   into y-values
	   finally (return (values
			    (cond ((or (floatp max-y)
				       (floatp min-y))
				   (return
				     (values (coerce max-y 'double-float)
					     (coerce min-y 'double-float)
					     y-values)))
				  (t (return
				       (values max-y min-y y-values)))))))
      
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
			       :mark (format nil "~a" y)))

;;; Draw vertical grid:

	(loop for x from (- min-x (rem min-x x-grid-step))
	   to max-x by x-grid-step
	   do (draw-vertical (round (- (* x x-scale)
				       screen-x0))
			     (sdl:color :r 50 :g 50 :b 50)
			     surface
			     :mark (format nil "~a" x)))
	
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
		 for pfunc in pfunc-list
		 do (draw-value x y y-scale slack-pixels screen-y0
				surface pfunc)
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
     ;;     (mapcar #'(lambda (func)
     ;;		 (if (listp func)
     ;;		     (make-plotfunc :function (car func)
     ;;				    :subs (cdr func))
     ;;		     func))
     ;;	     func-list)
     func-list
     from to slack)

    (sdl:update-display)
    
    (sdl:with-events (:poll)
      (:quit-event
       () t)

      (:idle
       ()
       ))))
