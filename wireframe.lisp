(let (f-x0 f-y0 f-x1 f-y1 f-x2 f-y2) ;wireframe front base crds

  (defun render-wireframe-grid (state value-scaler x0 y0 x1 y1 x2 y2 x3 y3)
    (flet ((grid-line (x0 y0 x1 y1 y)
	     (sdl:draw-line-* x0 (- (height state)
				    (round
				     (+ (* (cos (pitch state)) y) y0)))
			      x1 (- (height state)
				    (round
				     (+ (* (cos (pitch state)) y) y1)))
			      :color *grid-color*
			      :surface (surface state))))
      (let ((grid-line-delta (* value-scaler
				(mark-lines (- (max-y state)
					       (min-y state)))))
	    (far-corner (far-corner state)))
	(loop
	   for y from (- (* value-scaler (min-y state))
			 (* value-scaler (/ (+ (max-y state)
					       (min-y state))
					    2)))
	   to (- (* value-scaler (max-y state))
		 (* value-scaler (/ (+ (max-y state)
				       (min-y state))
				    2)))
	   by grid-line-delta
	   do
	     (case far-corner
	       ((min-min-max min-min-min)
		(grid-line x0 y0 x1 y1 y)
		(grid-line x3 y3 x0 y0 y))
	       ((min-max-max min-max-min)
		(grid-line x3 y3 x0 y0 y)
		(grid-line x2 y2 x3 y3 y))
	       ((max-max-max max-max-min)
		(grid-line x2 y2 x3 y3 y)
		(grid-line x1 y1 x2 y2 y))
	       ((max-min-max max-min-min)
		(grid-line x1 y1 x2 y2 y)
		(grid-line x0 y0 x1 y1 y))))
	
	(case far-corner
	  ((min-min-max min-min-min)
	   (sdl:draw-line-* x0 (- (height state) y0)
			    x1 (- (height state) y1)
			    :color *grid-origin-color*
			    :surface (surface state))
	   (sdl:draw-line-* x3 (- (height state) y3)
			    x0 (- (height state) y0)
			    :color *grid-origin-color*
			    :surface (surface state))
	   (setf f-x0 x1 f-y0 y1
		 f-x1 x2 f-y1 y2
		 f-x2 x3 f-y2 y3))
	  ((min-max-max min-max-min)
	   (sdl:draw-line-* x2 (- (height state) y2)
			    x3 (- (height state) y3)
			    :color *grid-origin-color*
			    :surface (surface state))
	   (sdl:draw-line-* x3 (- (height state) y3)
			    x0 (- (height state) y0)
			    :color *grid-origin-color*
			    :surface (surface state))
	   (setf f-x0 x0 f-y0 y0
		 f-x1 x1 f-y1 y1
		 f-x2 x2 f-y2 y2))
	  ((max-max-max max-max-min)
	   (sdl:draw-line-* x1 (- (height state) y1)
			    x2 (- (height state) y2)
			    :color *grid-origin-color*
			    :surface (surface state))
	   (sdl:draw-line-* x2 (- (height state) y2)
			    x3 (- (height state) y3)
			    :color *grid-origin-color*
			    :surface (surface state))
	   (setf f-x0 x3 f-y0 y3
		 f-x1 x0 f-y1 y0
		 f-x2 x1 f-y2 y1))
	  ((max-min-max max-min-min)
	   (sdl:draw-line-* x0 (- (height state) y0)
			    x1 (- (height state) y1)
			    :color *grid-origin-color*
			    :surface (surface state))
	   (sdl:draw-line-* x1 (- (height state) y1)
			    x2 (- (height state) y2)
			    :color *grid-origin-color*
			    :surface (surface state))
	   (setf f-x0 x2 f-y0 y2
		 f-x1 x3 f-y1 y3
		 f-x2 x0 f-y2 y0))))

      ;; TODO: move to front function
      (let ((middle-value (/ (- (max-y state) (min-y state))
			     2)))
	(sdl:draw-string-solid-*
	 (format nil "(~a,~a,~a)"
		 (min-x state) (min-z state) middle-value)
	 x0 (- (height state) y0)
	 :color *grid-color*
	 :surface (surface state))

	(sdl:draw-string-solid-*
	 (format nil "(~a,~a,~a)"
		 (max-x state) (min-z state) middle-value)
	 x1 (- (height state) y1)
	 :color *grid-color*
	 :surface (surface state))

	(sdl:draw-string-solid-*
	 (format nil "(~a,~a,~a)"
		 (max-x state) (max-z state) middle-value)
	 x2 (- (height state) y2)
	 :color *grid-color*
	 :surface (surface state))

	(sdl:draw-string-solid-*
	 (format nil "(~a,~a,~a)"
		 (min-x state) (max-z state) middle-value)
	 x3 (- (height state) y3)
	 :color *grid-color*
	 :surface (surface state))
	
	)))

  (defun render-wireframe-grid-front (state)
    (sdl:draw-line-* f-x0 (- (height state) f-y0)
		     f-x1 (- (height state) f-y1)
		     :color *grid-origin-color*
		     :surface (surface state))
    (sdl:draw-line-* f-x1 (- (height state) f-y1)
		     f-x2 (- (height state) f-y2)
		     :color *grid-origin-color*
		     :surface (surface state))))

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
  (declare (3d-state state)
	   ;;(optimize speed)
	   )
  (let* ((wire-density 1/50)
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

	 (value-scaler (/ (/ (- (height state) ; drawing area
				(* 2 (margin state))) 
			     (- (max-y state) (min-y state))) ; spread of range
			  2))
	 (value-shift-pixels (+ (/ (height state) 2) ; move to surface center
				;; correct for centre
				(* (cos (pitch state))
				   (-
				    (* value-scaler
				       (/ (+ (min-y state)
					     (max-y state))
					  2))))))
	 (far-corner (far-corner state))
	 )

    (let ((corner-x0
	   (round (+ (/ (width state) 2)
		     (* gra-render-radius
			(cos (+ (atan 1 1)
				(yaw state)))))))
	  (corner-y0
	   (round
	    (+ (/ (height state) 2)
	       (* (sin (pitch state))
		  (* gra-render-radius
		     (sin (+ (atan 1 1)
			     (yaw state))))))))
	  (corner-x1
	   (round (+ (/ (width state) 2)
		     (* gra-render-radius
			(cos (+ (atan 1 -1)
				(yaw state)))))))
	  (corner-y1
	   (round
	    (+ (/ (height state) 2)
	       (* (sin (pitch state))
		  (* gra-render-radius
		     (sin (+ (atan 1 -1)
			     (yaw state))))))))
	  (corner-x2
	   (round (+ (/ (width state) 2)
		     (* gra-render-radius
			(cos (+ (atan -1 -1)
				(yaw state)))))))
	  (corner-y2
	   (round
	    (+ (/ (height state) 2)
	       (* (sin (pitch state))
		  (* gra-render-radius
		     (sin (+ (atan -1 -1)
			     (yaw state))))))))
	  (corner-x3
	   (round (+ (/ (width state) 2)
		     (* gra-render-radius
			(cos (+ (atan -1 1)
				(yaw state)))))))
	  (corner-y3
	   (round
	    (+ (/ (height state) 2)
	       (* (sin (pitch state))
		  (* gra-render-radius
		     (sin (+ (atan -1 1)
			     (yaw state)))))))))

      (render-wireframe-grid state value-scaler
			     corner-x0 corner-y0
			     corner-x1 corner-y1
			     corner-x2 corner-y2
			     corner-x3 corner-y3)
      
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
		      (let* ((x0 (xwire-compute-x gra-rel gra-rel-wire
						  unit-multiplier state))
			     (z0 (xwire-compute-z value gra-rel
						  gra-rel-wire value-shift-pixels
						  value-scaler unit-multiplier state))
			     (x1 (xwire-compute-x gra-rel-next gra-rel-wire
						  next-unit-multiplier state))
			     (z1 (xwire-compute-z next-value gra-rel-next
						  gra-rel-wire value-shift-pixels
						  value-scaler next-unit-multiplier state)))
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
		      (let* ((x0 (zwire-compute-x gra-rel gra-rel-wire
						  unit-multiplier state))
			     (z0 (zwire-compute-z value gra-rel
						  gra-rel-wire value-shift-pixels
						  value-scaler unit-multiplier state))
			     (x1 (zwire-compute-x gra-rel-next gra-rel-wire
						  next-unit-multiplier state))
			     (z1 (zwire-compute-z next-value gra-rel-next
						  gra-rel-wire value-shift-pixels
						  value-scaler next-unit-multiplier state)))
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
