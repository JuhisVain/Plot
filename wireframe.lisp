(deftype screen-limits (type)
  `(,type ,(coerce -10000 type) ,(coerce 10000 type))) ; that's a lot of pixels

(defun 3d-crd-scr (x z y state &optional
				 (shift (screen-y0 state)))
  "Translate 3 dimensional coordinates to wireframe screen surface
((0,0) being upper left) coordinates.
Floating indexes X and Z should be floats between 0.0 and 1.0
Y refers to value times value scaler."
  (declare (optimize (speed 3))
	   ((single-float 0.0 1.0) x z)
	   ;; DANGER ZONE! Only fun-crd-scr guards this from the ZERODIV symbol:
	   ((or single-float (complex single-float)) y)
	   (type wireframe state)
	   (single-float shift))
  (let ((width (width state))
	(height (height state))
	(yaw (yaw state))
	(pitch (pitch state)))
    (declare (fixnum width height)
	     (single-float yaw pitch))
    (let ((radvec-length (* (cos (/ +sf-pi+ 4)) ; aka. (sin (/ pi 4))
			    (the single-float (render-radius state))))
	  (half-width (/ width 2.0))
	  (half-height (/ height 2.0)))
      (let ((gra-x (* (/ radvec-length
			 half-width)
		      (- half-width
			 (* x width))))
	    (gra-z (* (/ radvec-length
			 half-height)
		      (- half-height
			 (* z height)))))
	(let ((hypotenuse (sqrt (+ (expt gra-x 2)
				   (expt gra-z 2))))
	      (angle (atan gra-z gra-x)))
	  (let* ((gra-yaw (+ yaw angle))
		 (cos-yaw (cos gra-yaw))
		 (sin-yaw (sin gra-yaw))
		 (cos-pitch (cos pitch))
		 (sin-pitch (sin pitch)))
	    (cons (round
		   (the (screen-limits single-float)
			(+ half-width
			   (* hypotenuse
			      cos-yaw))))

		  (if (floatp y)
		      (round
		       (the (screen-limits single-float)
			    (+ shift
			       (* cos-pitch
				  y)
			       (* sin-pitch
				  (* hypotenuse
				     sin-yaw)))))
		      ;; if complex:
		      (let ((adder (+ shift (* sin-pitch
					       (* hypotenuse
						  sin-yaw)))))
			(complex (round (the (screen-limits single-float)
					     (+ adder
						(* cos-pitch
						   (realpart y)))))
				 (round (the (screen-limits single-float)
					     (+ adder
						(* cos-pitch
						   (imagpart y)))))))))))))))

(defun fun-crd-scr (x z func state &optional
				 (scaler (y-scale state))
				 (shift (screen-y0 state)))
  (declare ((float 0.0 1.0) x z)
	   (drawn func)
	   (wireframe state))
  (let ((value (3d-dataref func x z)))
    (typecase value
      (symbol '(0 . zero-division)) ; Hack to get a do nothing draw-line method
      (real
       (3d-crd-scr x z
		   (float (* value scaler) 1.0)
		   state
		   shift))
      (complex
       (3d-crd-scr x z
		   (complex (float (* (realpart value) scaler) 1.0)
			    (float (* (imagpart value) scaler) 1.0))
		   state
		   shift)))))

(defun render-wireframe-grid (state &optional (scaler (y-scale state)))
  (declare (wireframe state)
	   (real scaler))
  (flet ((draw-grid-line (xy0 xy1 color)
	   (draw-free-line (car xy0) (cdr xy0) (car xy1) (cdr xy1)
			   color (surface state)))
	 (backwall-indexes (corner)
	   (let ((max-width 1.0)
		 (max-height 1.0))
	     (ecase corner
	       ((min-min-min min-min-max)
		(list (list max-width 0.0)
		      (list 0.0 0.0)
		      (list 0.0 max-height)))
	       ((min-max-min min-max-max)
		(list (list 0.0 0.0)
		      (list 0.0 max-height)
		      (list max-width max-height)))
	       ((max-min-min max-min-max)
		(list (list max-width max-height)
		      (list max-width 0.0)
		      (list 0.0 0.0)))
	       ((max-max-min max-max-max)
		(list (list 0.0 max-height)
		      (list max-width max-height)
		      (list max-width 0.0))))))
	 (backwall-corners (corner)
	   (ecase corner
	     ((min-min-min min-min-max)
	      (list (list (max-x state) (min-z state))
		    (list (min-x state) (min-z state))
		    (list (min-x state) (max-z state))))
	     ((min-max-min min-max-max)
	      (list (list (min-x state) (min-z state))
		    (list (min-x state) (max-z state))
		    (list (max-x state) (max-z state))))
	     ((max-min-min max-min-max)
	      (list (list (max-x state) (max-z state))
		    (list (max-x state) (min-z state))
		    (list (min-x state) (min-z state))))
	     ((max-max-min max-max-max)
	      (list (list (min-x state) (max-z state))
		    (list (max-x state) (max-z state))
		    (list (max-x state) (min-z state)))))))
    
    (destructuring-bind
	((left-x left-y) ; these and below are actually x and z...
	 (center-x center-y)
	 (right-x right-y))
	(backwall-indexes (far-corner state))
      (loop for height from (min-y state) to (max-y state) 
	    by (mark-lines (y-range state)); min and max y are prealigned
	    do (let ((center (3d-crd-scr center-x center-y
					 (* scaler height)
					 state)))
		 (draw-grid-line (3d-crd-scr left-x left-y
					     (* scaler height)
					     state)
				 center
				 *grid-color*)
		 (draw-grid-line center
				 (3d-crd-scr right-x right-y
					     (* scaler height)
					     state)
				 *grid-color*)
		 (draw-string (format nil "~a" height)
			      ;; center might not always be best
			      (car center)
			      (cdr center)
			      (surface state)
			      :color *grid-color*)))

      (flet ((3d-draw-vertical (x z value0 value1 color &optional mark mark-value)
	       (declare ((member low high mid NIL) mark))
	       (let ((low-val (3d-crd-scr x z (* scaler value0) state))
		     (high-val (3d-crd-scr x z (* scaler value1) state)))
		 (draw-grid-line high-val low-val color)
		 (case mark
		   (high (sdl:draw-string-solid-*
			  (format nil "~a" mark-value)
			  (car high-val) (- (height state) (cdr high-val))
			  :color color
			  :surface (surface state)))
		   (low (sdl:draw-string-solid-*
			  (format nil "~a" mark-value)
			  (car low-val) (- (height state) (cdr low-val))
			  :color color
			  :surface (surface state)))
		   (mid (sdl:draw-string-solid-*
			  (format nil "~a" mark-value)
			  (round (+ (car high-val) (car low-val)) 2)
			  (- (height state) (round (+ (car high-val) (car low-val)) 2))
			  :color color
			  :surface (surface state)))))))
		 
	
	;; Draw 3 furthest corner lines:
	(destructuring-bind
	    ((crd-left-x crd-left-z)
	     (crd-center-x crd-center-z)
	     (crd-right-x crd-right-z))
	    (backwall-corners (far-corner state))
	  (3d-draw-vertical left-x left-y (min-y state) (max-y state)
			    *grid-origin-color*
			    'low (format nil "(~a,~a)" crd-left-x crd-left-z))
	  (3d-draw-vertical center-x center-y (min-y state) (max-y state)
			    *grid-origin-color*
			    'low (format nil "(~a,~a)" crd-center-x crd-center-z))
	  (3d-draw-vertical right-x right-y (min-y state) (max-y state)
			    *grid-origin-color*
			    'low (format nil "(~a,~a)" crd-right-x crd-right-z)))

	;; In min and max alignments below:
	;; Flooring/ceilinging might produce values outside bounding square's
	;; limits (state min-x to max-x, min-z to max-z).
	;; Adding/removing a single mark-lines unit will force grid lines
	;; within range.
	(let* ((x-range-align (mark-lines (x-range state)))
	       (x-align-min (+ x-range-align ; don't draw corners here
			       (* x-range-align
				  (floor (min-x state)
					 x-range-align))))
	       (x-align-max (- (* x-range-align
				  (ceiling (max-x state)
					   x-range-align))
			       x-range-align)))
	  
	  (loop for x from x-align-min to x-align-max by x-range-align
		do (3d-draw-vertical
		    (coerce (/ (- x (min-x state))
			       (x-range state))
			    'SINGLE-FLOAT)
		    center-y ; why does this work
		    (min-y state) (max-y state) *grid-color*
		    'low x)))

	(let* ((z-range-align (mark-lines (z-range state)))
	       (z-align-min (+ z-range-align
			       (* z-range-align
				  (floor (min-z state)
					 z-range-align))))
	       (z-align-max (- (* z-range-align
				  (ceiling (max-z state)
					   z-range-align))
			       z-range-align)))
	  
	  (loop for z from z-align-min to z-align-max by z-range-align
		do (3d-draw-vertical
		    center-x ; ooga booga
		    (coerce (/ (- z (min-z state))
			       (z-range state))
			    'SINGLE-FLOAT)
		    (min-y state) (max-y state) *grid-color*
		    'low z)))

	))))

;;TODO: may want to consider some kind of FLAT symbols to take car of graphical
;; issues when pitch or state aligns with pi/2 and multipliers.
(defun far-corner (state)
  "Returns wireframe rendering area's (which is a cube) furthest corner."
  (declare (3d-state state))
  (with-slots (yaw pitch) state
    ;; Could be done with some macro or read-eval hack, but it's done like this.
    (cond ((>= (/ pi 2) yaw 0)
	   (cond ((>= (/ pi 2) pitch 0)
		  'min-min-min)
		 ((> (* 2 pi) pitch (* 3/2 pi))
		  'min-min-max)
		 ((>= (* 3/2 pi) pitch pi)
		  'max-max-max)
		 ((> pi pitch (/ pi 2))
		  'max-max-min)))
	  ((>= pi yaw (/ pi 2))
	   (cond ((>= (/ pi 2) pitch 0)
		  'min-max-min)
		 ((> (* 2 pi) pitch (* 3/2 pi))
		  'min-max-max)
		 ((>= (* 3/2 pi) pitch pi)
		  'max-min-max)
		 ((> pi pitch (/ pi 2))
		  'max-min-min)))
	  ((>= (* 3/2 pi) yaw pi)
	   (cond ((>= (/ pi 2) pitch 0)
		  'max-max-min)
		 ((> (* 2 pi) pitch (* 3/2 pi))
		  'max-max-max)
		 ((>= (* 3/2 pi) pitch pi)
		  'min-min-max)
		 ((> pi pitch (/ pi 2))
		  'min-min-min)))
	  ((>= (* 2 pi) yaw (* 3/2 pi))
	   (cond ((>= (/ pi 2) pitch 0)
		  'max-min-min)
		 ((> (* 2 pi) pitch (* 3/2 pi))
		  'max-min-max)
		 ((>= (* 3/2 pi) pitch pi)
		  'min-max-max)
		 ((> pi pitch (/ pi 2))
		  'min-max-min)))
	  (t (error "Invalid YAW = ~a~%" yaw)))))

(defun render-wireframe (state)
  (declare (wireframe state))
  (let* ((wire-density (wire-density state))
	 (value-scaler (y-scale state))
	 ;;; How far the logic center of the screen is from where the center of
	 ;; the zero value plane would be in pixels from the bottom of surface:
	 ;; f.ex: when plot values range from -1 to 1, this will always be
	 ;; half of surface height.
	 (value-shift-pixels (screen-y0 state))
	 (far-corner (far-corner state))
	 )

    (let* ((scaled-mid-value (* value-scaler
				(/ (+ (min-y state)
				      (max-y state))
				   2)))
	   (x0z0 (3d-crd-scr 0.0 0.0
			     scaled-mid-value
			     state value-shift-pixels))
	   (x1z0 (3d-crd-scr 1.0 0.0
			     scaled-mid-value
			     state value-shift-pixels))
	   (x1z1 (3d-crd-scr 1.0 1.0
			     scaled-mid-value
			     state value-shift-pixels))
	   (corner-x0 (car x0z0))
	   (corner-x1 (car x1z0))
	   (corner-x2 (car x1z1)))

      (do* ; All 'squares' of whole wireframe, with painter's algorithm
       (;; Generate list of float indexes and reverse as required for draw order
	(wire-list (loop for a from 0.0 to (/ 1 wire-density)
		      collect (* wire-density a)))
	(x-wires-full (case far-corner
			((max-max-max max-max-min min-max-max min-max-min)
			 (reverse wire-list))
			(otherwise wire-list)))
	(z-wires-full (case far-corner
			((max-max-min max-max-max max-min-max max-min-min)
			 (reverse wire-list))
			(otherwise wire-list)))
	(x-wires x-wires-full)
	(z-wires z-wires-full
		 (if (null next-z-wire)
		     (progn (setf x-wires (cdr x-wires))
			    z-wires-full)
		     (cdr z-wires)))
	;; how many pixels in screen x-vector of a complete line from max to min
	(x-xvector-pixels (abs (- corner-x0 corner-x1)))
	(z-xvector-pixels (abs (- corner-x1 corner-x2)))
	;; how many in square:
	(x-xsv-pix (* wire-density x-xvector-pixels))
	(z-xsv-pix (* wire-density z-xvector-pixels))
	;; current wires:
	(x-wire (car x-wires) (car x-wires))
	(next-x-wire (cadr x-wires) (cadr x-wires))
	(z-wire (car z-wires) (car z-wires))
	(next-z-wire (cadr z-wires) (cadr z-wires)))
       
       ((null x-wires)) ; end render
        (draw-wireframe-square x-wire next-x-wire z-xsv-pix
			       z-wire next-z-wire x-xsv-pix
			       value-scaler
			       value-shift-pixels state)
	;;This is fun:
	;(sdl:update-display)
	;(sleep 0.5)
	))))

(defun draw-wireframe-square-xwire (current-wire wire next-wire xvector-pixels
				    value-scaler value-shift-pixels state)

  (when (and next-wire)
    (loop for func in (collect-drawn (pfunc-list state))
	  do (loop for wire-crds
		     on (list-square-wire-coordinates
			 wire next-wire xvector-pixels (data-per-pixel func))
		   until (null (rest wire-crds))
		   do (let ((xz0 (fun-crd-scr (car wire-crds) current-wire
					      func state value-scaler
					      value-shift-pixels))
			    (xz1 (fun-crd-scr (cadr wire-crds) current-wire
					      func state value-scaler
					      value-shift-pixels)))
			(draw-line (car xz0) (cdr xz0)
				   (car xz1) (cdr xz1)
				   func (surface state))))))
  nil)


(defun draw-wireframe-square-zwire (current-wire wire next-wire xvector-pixels
				    value-scaler value-shift-pixels state)
  (when (and next-wire)
    (loop for func in (collect-drawn (pfunc-list state))
	  do (loop for wire-crds on
				 (list-square-wire-coordinates
				  wire next-wire xvector-pixels (data-per-pixel func))
		   until (null (rest wire-crds))
		   do (let ((xz0 (fun-crd-scr current-wire (car wire-crds)
					      func state value-scaler value-shift-pixels))
			    (xz1 (fun-crd-scr current-wire (cadr wire-crds)
					      func state value-scaler value-shift-pixels)))
			(draw-line (car xz0) (cdr xz0)
				   (car xz1) (cdr xz1)
				   func (surface state))))))
  nil)

(defun draw-wireframe-square (x-wire next-x-wire z-xsv-pix
			      z-wire next-z-wire x-xsv-pix
			      value-scaler value-shift-pixels
			      state)
  (draw-wireframe-square-xwire x-wire z-wire next-z-wire x-xsv-pix
			       value-scaler value-shift-pixels
			       state)
  (draw-wireframe-square-zwire z-wire x-wire next-x-wire z-xsv-pix
			       value-scaler value-shift-pixels
			       state))

(defun list-square-wire-coordinates (current next step-divisor resolution)
  "Produces list starting at CURRENT and ending at NEXT."
  (declare ((or (float 0.0 1.0)
		(rational 0 1))
	    current next)
	   ((rational 0 *) step-divisor))
  (let ((preliminary-list
	 (if (< current next)
	     (loop
		for crd from current to next
		by (/ (abs (- next current))
		      (* resolution step-divisor))
		collect crd)
	     (loop
		for crd from current downto next
		by (/ (abs (- next current))
		      (* resolution step-divisor))
		collect crd))))
    ;;; Enforce list ending with NEXT:
    ;; without this there will be gaps in the wires
    (if (/= (car (last preliminary-list)) next)
	(append preliminary-list (cons next nil))
	preliminary-list)))
