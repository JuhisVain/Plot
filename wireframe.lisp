(defun 3d-crd-scr (x z y state &optional
				 (shift (screen-y0 state)))
  "Translate 3 dimensional coordinates to wireframe screen surface
((0,0) being upper left) coordinates.
Floating indexes X and Z refer to array dimension 0 1 respectively
and Y to value times value scaler."
  (declare ((float 0.0 *) x z)
	   (wireframe state))
  (let ((radvec-length (* (cos (/ pi 4)) ; aka. (sin (/ pi 4))
			  (render-radius state)))
	(half-width (/ (width state) 2))
	(half-height (/ (height state) 2)))
    (let ((gra-x (* (/ radvec-length
		       half-width)
		    (- half-width
		       x)))
	  (gra-z (* (/ radvec-length
		       half-height)
		    (- half-height
		       z))))
      (let((hypotenuse (sqrt (+ (expt gra-x 2)
				(expt gra-z 2))))
	   (angle (atan gra-z gra-x)))
	
	(cons (round (+ half-width
			(* hypotenuse
			   (cos (+ (yaw state)
				   angle)))))
	      (round
	       (+ shift
		  (* (cos (pitch state))
		     y)
		  (* (sin (pitch state))
		     (* hypotenuse
			(sin (+ (yaw state)
				angle)))))))))))

(defun fun-crd-scr (x z func state &optional
				 (scaler (y-scale state))
				 (shift (screen-y0 state)))
  (declare ((float 0.0 1.0) x z)
	   (drawn func)
	   (wireframe state))

  (3d-crd-scr (* (1- (array-dimension (data func) 0))
		 (/ (data-per-pixel func))
		 x)
	      (* (1- (array-dimension (data func) 1))
		 (/ (data-per-pixel func))
		 z)
	      (* (3d-dataref func x z) ;; TODO: bad values, once more
		 scaler)
	      state
	      shift))

(defun render-wireframe-grid-front (state)
  ;; TODO
  ;; note: is it of any value to draw stuff in front of the plot?
  ;; maybe make generic so user can do what he likes..
  ;; that can be done with a function too though
  (format t "Quack!~%"))

(defun render-wireframe-grid (state &optional (scaler (y-scale state)))
  (declare (wireframe state)
	   (real scaler))
  (flet ((draw-grid-line (xy0 xy1 color)
	   (sdl:draw-line-* (car xy0) (- (height state) (cdr xy0))
			    (car xy1) (- (height state) (cdr xy1))
			    :surface (surface state)
			    :color color))
	 (backwall-coordinates (corner)
	   (let ((max-width (* (width state) 1.0))
		 (max-height (* (height state) 1.0)))
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
		      (list max-width 0.0)))))))
	 
    (destructuring-bind
	((left-x left-y) ; these and below are actually x and z...
	 (center-x center-y)
	 (right-x right-y))
	(backwall-coordinates (far-corner state))
      (loop for height from (min-y state) to (max-y state) 
	    by (mark-lines (y-range state)); min and max y are prealigned
	    do (draw-grid-line (3d-crd-scr left-x left-y
					   (* scaler height)
					   state)
			       (3d-crd-scr center-x center-y
					   (* scaler height)
					   state)
			       *grid-color*)
	       (draw-grid-line (3d-crd-scr center-x center-y
					   (* scaler height)
					   state)
			       (3d-crd-scr right-x right-y
					   (* scaler height)
					   state)
			       *grid-color*))

      (flet ((draw-vertical (x z value0 value1 color &optional mark mark-value)
	       (declare ((member low high mid NIL) mark))
	       (let ((low-val (3d-crd-scr x z (* scaler value0) state))
		     (high-val (3d-crd-scr x z (* scaler value1) state)))
		 (draw-grid-line high-val low-val color)
		 (case mark
		   (high (sdl:draw-string-solid-*
			  (format nil "~a" mark-value)
			  (car high-val) (- (height state) (cdr high-val))
			  :color *grid-color*
			  :surface (surface state)))
		   (low (sdl:draw-string-solid-*
			  (format nil "~a" mark-value)
			  (car low-val) (- (height state) (cdr low-val))
			  :color *grid-color*
			  :surface (surface state)))
		   (mid (sdl:draw-string-solid-*
			  (format nil "~a" mark-value)
			  (round (+ (car high-val) (car low-val)) 2)
			  (- (height state) (round (+ (car high-val) (car low-val)) 2))
			  :color *grid-color*
			  :surface (surface state)))))))
		 
	
	;; Draw 3 furthest corner lines:
	(draw-vertical left-x left-y (min-y state) (max-y state)
		       *grid-origin-color*)
	(draw-vertical center-x center-y (min-y state) (max-y state)
		       *grid-origin-color*)
	(draw-vertical right-x right-y (min-y state) (max-y state)
		       *grid-origin-color*)

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
		do (draw-vertical
		    (* (/ (- x (min-x state))
			  (x-range state))
		       1.0 ; coerce float
		       (width state))
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
		do (draw-vertical
		    left-y ; ooga booga
		    (* (/ (- z (min-z state))
			  (z-range state))
		       1.0 ; coerce float
		       (width state))
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
  (declare (wireframe state)
	   ;;(optimize speed)
	   )
  (let* ((wire-density (wire-density state))
	 (effective-diameter (min (width state)
				  (height state)))
	 (log-centre-x (+ (min-x state)
			  (/ (- (max-x state)
				(min-x state))
			     2)))
	 (log-centre-z (+ (min-z state)
			  (/ (- (max-z state)
				(min-z state))
			     2)))
	 (gra-centre-x (/ effective-diameter
			  2))
	 (gra-centre-z (/ effective-diameter
			  2))
	 (gra-render-radius (/ (- (min (width state)
				       (height state))
				  (* 2 (margin state)))
			       2))

	 (value-scaler (y-scale state))
	 ;;; How far the logic center of the screen is from where the center of
	 ;; the zero value plane would be in pixels from the bottom of surface:
	 ;; f.ex: when plot values range from -1 to 1, this will always be
	 ;; half of surface height.
	 (value-shift-pixels (screen-y0 state))
	 (far-corner (far-corner state))
	 )

    (let ((ccc (3d-crd-scr
		(* (width state)
		   1
		   0.0)
		(* (height state)
		   1
		   1.0)
		(* (min-y state)
		   value-scaler)
		state value-shift-pixels)))
      (sdl:draw-string-solid-* "xxx" (round (car ccc)) (- (height state) (round (cdr ccc)))
			       :surface (surface state)))

    (let* ((scaled-mid-value (* value-scaler
				(/ (+ (min-y state)
				      (max-y state))
				   2)))
	   (x0z0 (3d-crd-scr 0.0 0.0
			     scaled-mid-value
			     state value-shift-pixels))
	   (x1z0 (3d-crd-scr (* 1.0 (width state)) 0.0
			     scaled-mid-value
			     state value-shift-pixels))
	   (x0z1 (3d-crd-scr 0.0 (* 1.0 (height state))
			     scaled-mid-value
			     state value-shift-pixels))
	   (x1z1 (3d-crd-scr (* 1.0 (width state)) (* 1.0 (height state))
			     scaled-mid-value
			     state value-shift-pixels))
	   (corner-x0 (car x0z0))
	   (corner-y0 (cdr x0z0))
	   (corner-x1 (car x1z0))
	   (corner-y1 (cdr x1z0))
	   (corner-x3 (car x0z1))
	   (corner-y3 (cdr x0z1))
	   (corner-x2 (car x1z1))
	   (corner-y2 (cdr x1z1)))

      (render-wireframe-grid state value-scaler)

      (do* ; All 'squares' of whole wireframe, with painter's algorithm
       ((x-dimension (min (width state)
			  (height state)))
	(z-dimension (min (width state)
			  (height state)))
	;; Generate list of float indexes and reverse as required for draw order
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
	(next-z-wire (cadr z-wires) (cadr z-wires))

	

	)
       ((null x-wires)) ; end render
	(draw-wireframe-square x-wire next-x-wire z-xsv-pix
			       z-wire next-z-wire x-xsv-pix
			       gra-centre-x x-dimension
			       gra-centre-z z-dimension
			       gra-render-radius value-scaler
			       value-shift-pixels state)
	
	)
      (render-wireframe-grid-front state)
      )))

(defun round-complex (number) ; obsolete?
  "Round with complex number support."
  (declare (number number))
  (complex (round (realpart number))
	   (round (imagpart number))))

(defun Xwire-compute-X (gra-rel wire-constant unit-multiplier state)
  (round
   ;; shift coord right:
   (+ (/ (width state) 2) 
      (*
       ;; unit multiplier:
       unit-multiplier
       ;; unit circle position as determined by state's yaw:
       (cos (+ (atan wire-constant gra-rel)
	       (yaw state)))))))

(defun Xwire-compute-Z (value gra-rel
			wire-constant value-shift-pixels
			value-scaler unit-multiplier state)
  (flet ((internal-compute (value)
	   (declare (real value))
	   (round
	    (+ value-shift-pixels
	       (* (cos (pitch state))
		  (* value-scaler
		     value))
	       (* (sin (pitch state))
		  (*
		   unit-multiplier
		   (sin (+ (atan wire-constant gra-rel)
			   (yaw state)))))))))
    (if (complexp value)
	(complex (internal-compute (realpart value))
		 (internal-compute (imagpart value)))
	(internal-compute value))))

(defun Zwire-compute-X (gra-rel wire-constant unit-multiplier state)
  (round
   ;; shift coord right:
   (+ (/ (width state) 2) 
      (*
       ;; unit multiplier:
       unit-multiplier
       ;; unit circle position as determined by state's yaw:
       (cos (+ (atan gra-rel wire-constant)
	       (yaw state)))))))

(defun Zwire-compute-Z (value gra-rel
			wire-constant value-shift-pixels
			value-scaler unit-multiplier state)
  (flet ((internal-compute (value)
	   (declare (real value))
	   (round
	    (+ value-shift-pixels
	       (* (cos (pitch state))
		  (* value-scaler
		     value))
	       (* (sin (pitch state))
		  (*
		   unit-multiplier
		   (sin (+ (atan gra-rel wire-constant)
			   (yaw state)))))))))
    (if (complexp value)
	(complex (internal-compute (realpart value))
		 (internal-compute (imagpart value)))
	(internal-compute value))))

(defun draw-wireframe-square-xwire (current-wire wire next-wire xvector-pixels
				    gra-rel-wire gra-centre gra-render-radius
				    dimension value-scaler value-shift-pixels
				    state)

  (when (and next-wire)
    (loop for func in (collect-drawn (pfunc-list state))
       do (loop for wire-crds on
	       (list-square-wire-coordinates
		wire next-wire xvector-pixels (data-per-pixel func))
	     until (null (rest wire-crds))
	     for gra-rel = (* (/ (* (cos (/ pi 4))
				    gra-render-radius)
				 gra-centre)
			      (- gra-centre
				 (* (car wire-crds) dimension)))
	     and gra-rel-next = (* (/ (* (cos (/ pi 4))
					 gra-render-radius)
				      gra-centre)
				   (- gra-centre
				      (* (cadr wire-crds)
					 dimension)))
	     for unit-multiplier = (sqrt (+ (expt gra-rel-wire 2)
					    (expt gra-rel 2)))
	     and next-unit-multiplier = (sqrt (+ (expt gra-rel-wire 2)
						 (expt gra-rel-next 2)))
	     and value = (3d-dataref func (car wire-crds) current-wire)
	     and next-value = (3d-dataref func (cadr wire-crds) current-wire)
	     do
	       (cond ((and (numberp value) (numberp next-value))
		      (let* ((xz0 (fun-crd-scr (car wire-crds) current-wire
					       func state value-scaler value-shift-pixels))
			     (xz1 (fun-crd-scr (cadr wire-crds) current-wire
					       func state value-scaler value-shift-pixels))
			     (x0 (car xz0))
			     (z0 (cdr xz0))
			     (x1 (car xz1))
			     (z1 (cdr xz1))
			     )
			(draw-line x0 z0 x1 z1
				   func (surface state))))
		     ((symbolp value)
		      NIL)
		     ((symbolp next-value) NIL)))))
  nil)


(defun draw-wireframe-square-zwire (current-wire wire next-wire xvector-pixels
				    gra-rel-wire gra-centre gra-render-radius
				    dimension value-scaler value-shift-pixels
				    state)
  (when (and next-wire)
    (loop for func in (collect-drawn (pfunc-list state))
       do (loop for wire-crds on
	       (list-square-wire-coordinates
		wire next-wire xvector-pixels (data-per-pixel func))
	     until (null (rest wire-crds))
	     for gra-rel = (* (/ (* (cos (/ pi 4))
				    gra-render-radius)
				 gra-centre)
			      (- gra-centre
				 (* (car wire-crds) dimension)))
	     and gra-rel-next = (* (/ (* (cos (/ pi 4))
					 gra-render-radius)
				      gra-centre)
				   (- gra-centre
				      (* (cadr wire-crds)
					 dimension)))
	     for unit-multiplier = (sqrt (+ (expt gra-rel-wire 2)
					    (expt gra-rel 2)))
	     and next-unit-multiplier = (sqrt (+ (expt gra-rel-wire 2)
						 (expt gra-rel-next 2)))
	     and value = (3d-dataref func current-wire (car wire-crds))
	     and next-value = (3d-dataref func current-wire (cadr wire-crds))
	     do
	       (cond ((and (numberp value) (numberp next-value))
		      (let* ((xz0 (fun-crd-scr current-wire (car wire-crds)
					       func state value-scaler value-shift-pixels))
			     (xz1 (fun-crd-scr current-wire (cadr wire-crds)
					       func state value-scaler value-shift-pixels))
			     (x0 (car xz0))
			     (z0 (cdr xz0))
			     (x1 (car xz1))
			     (z1 (cdr xz1))
			     )
			(draw-line x0 z0 x1 z1
				   func (surface state))))
		     ((symbolp value)
		      NIL)
		     ((symbolp next-value) NIL)))))
  nil)

(defun draw-wireframe-square (x-wire next-x-wire z-xsv-pix
			      z-wire next-z-wire x-xsv-pix
			      gra-centre-x x-dimension
			      gra-centre-z z-dimension
			      gra-render-radius value-scaler value-shift-pixels
			      state)
  (let ((gra-rel-x
	 (* (/ (* (cos (/ pi 4))
		  gra-render-radius)
	       gra-centre-x)
	    (- gra-centre-x
	       (* x-wire x-dimension))))
	(gra-rel-z
	 (* (/ (* (cos (/ pi 4))
		  gra-render-radius)
	       gra-centre-z)
	    (- gra-centre-z
	       (* z-wire z-dimension)))))
    (draw-wireframe-square-xwire x-wire z-wire next-z-wire x-xsv-pix
				 gra-rel-x gra-centre-z gra-render-radius
				 z-dimension value-scaler value-shift-pixels
				 state)
    (draw-wireframe-square-zwire z-wire x-wire next-x-wire z-xsv-pix
				 gra-rel-z gra-centre-x gra-render-radius
				 x-dimension value-scaler value-shift-pixels
				 state)
    ))

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
