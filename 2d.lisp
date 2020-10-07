(defun draw-grid (min-y max-y y-range y-scale screen-y0
		  min-x max-x x-range x-scale screen-x0
		  slack-pixels surface)
  (let ((y-grid-step (mark-lines y-range))
	(x-grid-step (mark-lines x-range)))
    
;;; Draw horizontal grid:
    (loop for y from (- min-y (rem min-y y-grid-step)
			y-grid-step) ; add one line to bottom
       ;; range increased by one line so grid
       ;; extends to all values even with slack:
       to (+ max-y y-grid-step) by y-grid-step
       do (draw-horizontal (round (+ (* y y-scale)
				     (- screen-y0)
				     slack-pixels))
			   *grid-color*
			   surface
			   :mark (format nil "~a" y)))

;;; Draw vertical grid:
    (loop for x from (- min-x (rem min-x x-grid-step))
       to max-x by x-grid-step
       do (draw-vertical (round (- (* x x-scale)
				   screen-x0))
			 *grid-color*
			 surface
			 :mark (format nil "~a" x)))

;;; Draw zeroes
    (draw-horizontal (round (+ (- screen-y0)
			       slack-pixels
			       ))
		     *grid-origin-color*
		     surface
		     :mark "0")

    (draw-vertical (round (- screen-x0))
		   *grid-origin-color*
		   surface
		   :mark "0")))

(defun render-2d-dots (function state
		       &optional (surface (surface state)))
  (loop for x from 0 below (sdl:width surface)
     by (/ 1 (data-per-pixel function))
     for y across (data function)
     do (draw-value (floor x) y (y-scale state) (slack-pixels state)
		    (screen-y0 state) surface function)))

(defun scale-y (y y-scale slack screen-y0)
  "Transforms value Y into plot's scale.
Result will still need to be inverted before drawing."
  (typecase y
    (real
     (round (- (+ (* y y-scale)
		  slack)
	       screen-y0)))
    (complex
     (complex (round (- (+ (* (realpart y) y-scale)
			   slack)
			screen-y0))
	      (round (- (+ (* (imagpart y) y-scale)
			   slack)
			screen-y0))))
    (t y))); come again! Pass through for non number values

(defun render-2d-lineplot (function state
			   &optional (surface (surface state)))
  (let ((x-pixel 0))
    (map
     NIL
     #'(lambda (from to)
	 (draw-line (floor x-pixel (data-per-pixel function))
		    (scale-y from (y-scale state)
			     (slack-pixels state) (screen-y0 state))
		    (floor (incf x-pixel) (data-per-pixel function))
		    (scale-y to (y-scale state)
			     (slack-pixels state) (screen-y0 state))
		    function
		    surface
		    ))
     (data function)
     (subseq (data function) 1))))

(defun render-2d-label (function state)
  (when (and (draw-labels state)
	     (< (label-position state)
		(/ (length (data function))
		   (data-per-pixel function))))
    (draw-string (label function)
		 (label-position state)
		 ;;TODO: Simplify:
		 (+ (sdl:char-height sdl:*default-font*)
		    (round
		     (realpart
		      (- (+ (slack-pixels state)
			    (handler-case
				(let* ((pre-x0 (floor ;index of low bound
						(* (data-per-pixel function)
						   (label-position state))))
				       (pre-x1 (ceiling ;index of high bound
						(* (data-per-pixel function)
						   (label-position state))))
				       (x0 (/ ;pixel of low bound
					    pre-x0
					    (data-per-pixel function)))
				       (x1 (/ ;pixel of high bound
					    pre-x1
					    (data-per-pixel function)))
				       (y0 (* (y-scale state) ; pixel value
					      (aref (data function)
						    pre-x0)))
				       (y1 (* (y-scale state)
					      (aref (data function)
						    pre-x1)))
				       (x (label-position state)))

				  (if (= x0 x1) ; comparing floats
				      y0
				      ;; y-value of X:
				      (+ y0
					 (* (/ (- y1 y0)
					       (- x1 x0))
					    (- x x0)))))
			      
			      (type-error () 0)))
			 
			 (screen-y0 state)))))
		 (surface state)
		 :color (color-real function))
    (incf (label-position state)
		   (* (sdl:char-width sdl:*default-font*)
		      (length (label function))))))

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

(defun draw-value (x-coord value y-scale slack screen-y0 surface pfunc)
  (typecase value
    (real (draw-pixel (round x-coord)
		      (round (- (+ (* value y-scale)
				   slack)
				screen-y0))
		      surface
		      (color-real pfunc)))
    ;;the components of a complex must be real:
    (complex (draw-value x-coord
			 (realpart value)
			 y-scale slack screen-y0 surface
			 (color-realpart pfunc))
	     (draw-value x-coord
			 (imagpart value)
			 y-scale slack screen-y0 surface
			 (color-imagpart pfunc)))
    (t (draw-vertical x-coord ; bad value, most likely zero div
		      *bad-color*
		      surface)))
  NIL)

