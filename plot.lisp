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
	  :b (voodoo 1)
	  :a 255)))

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
  (label nil :type string)
  (data nil :type (or array null))
  (render))

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
(defun to-funcdata (input-func-list resolution-width)
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
							 (format nil "~a" id))
				     :data (make-array resolution-width))))
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
							 (symbol-name func))
				     :data (make-array resolution-width))))
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

(defun plotcall (function index &rest arguments
		 &aux (lindex (if (listp index)
				  index
				  (list index))))
  "Funcall with handlers etc. for plottable data,
stored into array in funcdata FUNCTION's data slot at aref INDEX."
  (setf ;;;setfing an applied aref is used as example in the hyperspec!
   (apply #'aref (funcdata-data function) lindex)
   (handler-case
       (apply (funcdata-function function) arguments)
     (division-by-zero () 'ZERO-DIVISION)
     (type-error () nil))))
  
(defun plotfunc-evaluate (plotfunc index &rest arguments)
  (let ((fvalue (apply #'plotcall (plotfunc-function plotfunc) index arguments)))
    ;; plotcall might produce a symbol to represent an error
    ;; which the user might want to handle himself...
    (mapcar #'(lambda (sub)
		(etypecase sub
		  (funcdata
		   (plotcall sub index fvalue))
		  (plotfunc
		   (apply #'plotfunc-evaluate sub index arguments))))
	    (plotfunc-subs plotfunc))))

(defun extract-numbers (tree)
  "Get flat list of numbers in TREE."
  (loop for element in tree
     append (cond ((listp element)
		   (extract-numbers element))
		  ((numberp element)
		   (list element)))))

(defun get-numbers (arg)
  (etypecase arg
    (number (list arg))
    (list (extract-numbers arg))))

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

(defun render-func-list (func-list surface)
  (dolist (func func-list)
    (typecase func
      (plotfunc
       (render-func-list (plotfunc-subs func) surface))
      (funcdata
       (sdl:blit-surface (funcdata-render func) surface)))))

(defun draw-grid (min-y max-y y-range y-scale screen-y0
		  min-x max-x x-range x-scale screen-x0
		  slack-pixels surface)
  (let ((y-grid-step (mark-lines y-range))
	(x-grid-step (mark-lines x-range)))
    
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

;;; Draw zeroes
    (draw-horizontal (round (+ (- screen-y0)
			       slack-pixels
			       ))
		     (sdl:color :r 150 :g 150 :b 150)
		     surface
		     :mark "0")

    (draw-vertical (round (- screen-x0))
		   (sdl:color :r 150 :g 150 :b 150)
		   surface
		   :mark "0")))

(defun compute-2d-data (function min-x max-x x-step)
  "Populates funcdata FUNCTION's (and FUNCTION's subs) data slot's array with
results from applying FUNCTION on values of x from MIN-X to MAX-X by X-STEP.
Returns cons of maximum and minimum results on range."
  (let ((max-y)
	(min-y))
    (loop
       for x from min-x below max-x by x-step ;; TODO: check below or to
       for i from 0
       do (loop for value in (get-numbers
			      (if (plotfunc-p function)
				  (plotfunc-evaluate function i x)
				  (plotcall function i x)))
	     do (cond ((null max-y)
		       (setf max-y value
			     min-y value))
		      (t
		       (setf max-y (complex-max max-y value)
			     min-y (complex-min min-y value)))))
       finally (return (cons max-y min-y)))))

(defun compute-2d-tree (func-list min-x max-x x-step)
  "Computes data for all funcdatas in FUNC-LIST."
  (let* ((max-min (mapcar #'(lambda (func)
			      (compute-2d-data func min-x max-x x-step))
			  func-list))
	 (max (caar max-min))
	 (min (cdar max-min)))
    (dolist (mm max-min)
      (setf max (max max (car mm))
	    min (min min (cdr mm))))
    (values max min)))

(defparameter *transparency* (sdl:color :r 0 :g 0 :b 0 :a 0))

(defun render-2d-data (function y-scale slack-pixels screen-y0 surface)
  (declare (funcdata function)
	   (sdl:surface surface))

  (setf (funcdata-render function) surface)
  (sdl:fill-surface *transparency* :surface surface :update t)
  
  (loop for x from 0 below (sdl:width surface)
     for y across (funcdata-data function)
     do (draw-value x y y-scale slack-pixels screen-y0
		    surface function)))

(defun render-2d-tree (func-list y-scale slack-pixels screen-y0 width height)
  (dolist (func func-list)
    (etypecase func
      (plotfunc (render-2d-tree
		 (plotfunc-subs func)
		 y-scale slack-pixels screen-y0 width height))
      (funcdata (render-2d-data
		 func y-scale slack-pixels screen-y0
		 (sdl:create-surface width height :pixel-alpha 255))))))

(defun draw-function (input-func-list
		      min-x max-x
		      slack
		      &optional
			(surface sdl:*default-display*))
  "Graphs functions in FUNC-LIST from MIN-X to MAX-X, y-scaling is
dynamic based on extreme values on X's range."
  
  (let* ((pfunc-list (to-plotfunc (to-funcdata input-func-list (sdl:width surface))))
	 (win-width (sdl:width surface))
	 (win-height (sdl:height surface))
	 (x-range (- max-x min-x))
	 (x-scale (/ win-width x-range))
	 (x-step (/ x-range win-width)) ; rational, used to iterate arguments
	 (screen-x0 (* min-x (/ win-width x-range)))
	 )

    (multiple-value-bind (max-y min-y)
	(compute-2d-tree pfunc-list min-x max-x x-step)
      
      (format t "max ~a min ~a~%" max-y min-y)

      (let* ((pre-y-range (- max-y min-y)) ; range in value
	     (slack-mod (* pre-y-range slack)) ; total visible range in value
	     (y-range (+ pre-y-range slack-mod))
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

	;;draw grid
	(draw-grid min-y max-y y-range y-scale screen-y0
		   min-x max-x x-range x-scale screen-x0
		   slack-pixels surface)

	(render-2d-tree pfunc-list y-scale slack-pixels screen-y0
			win-width win-height)

	(render-func-list pfunc-list surface)
	))))


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
		:hw t
		:bpp 32)
    ;;(setf (sdl:frame-rate) 30)

    (draw-function
     func-list
     from to slack)

    (sdl:update-display)
    
    (sdl:with-events (:poll)
      (:quit-event
       () t)

      (:idle
       ()
       ))))
