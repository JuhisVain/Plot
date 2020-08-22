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
					   (min-y state))))))
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
	 (case (far-corner state)
	   (min-min
	    (grid-line x0 y0 x1 y1 y)
	    (grid-line x3 y3 x0 y0 y))
	   (min-max
	    (grid-line x3 y3 x0 y0 y)
	    (grid-line x2 y2 x3 y3 y))
	   (max-max
	    (grid-line x2 y2 x3 y3 y)
	    (grid-line x1 y1 x2 y2 y))
	   (max-min
	    (grid-line x1 y1 x2 y2 y)
	    (grid-line x0 y0 x1 y1 y))))

    (sdl:draw-line-* x0 (- (height state) y0)
		     x1 (- (height state) y1)
		     :color *grid-origin-color*
		     :surface (surface state))
    (sdl:draw-line-* x1 (- (height state) y1)
		     x2 (- (height state) y2)
		     :color *grid-origin-color*
		     :surface (surface state))
    
    (sdl:draw-line-* x2 (- (height state) y2)
		     x3 (- (height state) y3)
		     :color *grid-origin-color*
		     :surface (surface state))

    (sdl:draw-line-* x3 (- (height state) y3)
		     x0 (- (height state) y0)
		     :color *grid-origin-color*
		     :surface (surface state)))

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

(defun far-corner (state)
  "Returns wireframe render's horizontal plane's furthest corner."
  (declare (3d-state state))
  (with-slots (yaw pitch) state
    (cond ((>= (/ pi 2) yaw 0)
	   'min-min)
	  ((>= pi yaw (/ pi 2))
	   'min-max)
	  ((>= (* 3/2 pi) yaw pi)
	   'max-max)
	  ((>= (* 2 pi) yaw (* 3/2 pi))
	   'max-min)
	  (t (error "Invalid YAW = ~a~%" yaw)))))

(defun render-wireframe (state)
  (declare (3d-state state)
	   ;;(optimize speed)
	   )
  (let* ((wire-density 1/50)
	 (log-centre-x (+ (min-x state)
			  (/ (- (max-x state)
				(min-x state))
			     2)))
	 (log-centre-z (+ (min-z state)
			  (/ (- (max-z state)
				(min-z state))
			     2)))
	 (gra-centre-x (/ (width state) 2))
	 (gra-centre-z (/ (height state) 2))
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
			((max-max min-max) (reverse wire-list))
			(otherwise wire-list)))
	(z-wires-full (case far-corner
			((max-max max-min) (reverse wire-list))
			(otherwise wire-list)))
	(x-wires x-wires-full)
	(z-wires z-wires-full
		 (if (null next-z-wire)
		     (progn (setf x-wires (cdr x-wires))
			    ;(format t "x-wires: ~a~%" x-wires)
			    z-wires-full)
		     (progn
		       ;(format t "z-wires: ~a~%" z-wires)
		       (cdr z-wires))))
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
      )))

(defun draw-wireframe-square-wire (current-wire wire next-wire xvector-pixels
				   gra-rel-wire gra-centre gra-render-radius
				   dimension value-scaler value-shift-pixels
				   state)
  (when (and next-wire)
    (do*
     ((wire-crds
       (list-square-wire-coordinates wire next-wire xvector-pixels)
       (cdr wire-crds)))
     ((null (cdr wire-crds))) ; last coord used manually
      (let ((gra-rel (* (/ (* (cos (/ pi 4))
				gra-render-radius)
			     gra-centre)
			  (- gra-centre
			     (* (car wire-crds) dimension))))
	    (gra-rel-next (* (/ (* (cos (/ pi 4))
				     gra-render-radius)
				  gra-centre)
			       (- gra-centre
				  (* (cadr wire-crds)
				     dimension)))))

	(loop for func in (pfunc-list state)
	   do
	     (let* ((x0 (round
			 ;; shift coord right:
			 (+ (/ (width state) 2) 
			    (*
			     ;; unit multiplier:
			     (sqrt (+ (expt gra-rel-wire 2)
				      (expt gra-rel 2)))
			     ;; unit circle position as determined by state's yaw:
			     (cos (+ (atan gra-rel-wire gra-rel)
				     (yaw state)))))))
		    (z0 (round
			 (+ value-shift-pixels
			    ;; shift by value, modified by state's pitch:
			    (* (cos (pitch state))
			       (* value-scaler
				  (3d-dataref func
					      (car wire-crds)
					      current-wire)))
			    (* (sin (pitch state))
			       (*
				(sqrt (+ (expt gra-rel-wire 2)
					 (expt gra-rel 2)))
				(sin (+ (atan gra-rel-wire gra-rel)
					(yaw state))))))))
		    
		    (x1 (round
			 (+ (/ (width state) 2)
			    (* 
			     (sqrt (+ (expt gra-rel-wire 2)
				      (expt gra-rel-next 2)))
			     (cos (+ (atan gra-rel-wire gra-rel-next)
				     (yaw state)))))))
		    (z1  (round
			  (+ value-shift-pixels
			     (* (cos (pitch state))
				(* value-scaler
				   (3d-dataref func
					       (cadr wire-crds)
					       current-wire)))
			     (* (sin (pitch state))
				(*
				 (sqrt (+ (expt gra-rel-wire 2)
					  (expt gra-rel-next 2)))
				 (sin (+ (atan gra-rel-wire gra-rel-next)
					 (yaw state)))))))))
	       (draw-line x0 z0 x1 z1
			  func (surface state)))
	     )))))


;;;; Modified from above:
;; inverted gra-rel and gra-rel-wire in coordinate ATANs
;; inverted dataref args
;;;; TODO: ??? could pass lambdas for both but eh
(defun xxxdraw-wireframe-square-wire (current-wire wire next-wire xvector-pixels
				      gra-rel-wire gra-centre gra-render-radius
				      dimension value-scaler value-shift-pixels
				      state)
  (when (and next-wire)
    (do*
     ((wire-crds
       (list-square-wire-coordinates wire next-wire xvector-pixels)
       (cdr wire-crds)))
     ((null (cdr wire-crds))) ; last coord used manually
      (let ((gra-rel (* (/ (* (cos (/ pi 4))
			      gra-render-radius)
			   gra-centre)
			(- gra-centre
			   (* (car wire-crds) dimension))))
	    (gra-rel-next (* (/ (* (cos (/ pi 4))
				   gra-render-radius)
				gra-centre)
			     (- gra-centre
				(* (cadr wire-crds)
				   dimension)))))

	(loop for func in (pfunc-list state)
	   do
	     (let* ((x0 (round
			 ;; shift coord right:
			 (+ (/ (width state) 2) 
			    (*
			     ;; unit multiplier:
			     (sqrt (+ (expt gra-rel-wire 2)
				      (expt gra-rel 2)))
			     ;; unit circle position as determined by state's yaw:
			     (cos (+ (atan gra-rel gra-rel-wire)
				     (yaw state)))))))
		    (z0 (round
			 (+ value-shift-pixels
			    ;; shift by value, modified by state's pitch:
			    (* (cos (pitch state))
			       (* value-scaler
				  (3d-dataref func
					      current-wire
					      (car wire-crds)
					      ;current-wire
					      )))
			    (* (sin (pitch state))
			       (*
				(sqrt (+ (expt gra-rel-wire 2)
					 (expt gra-rel 2)))
				(sin (+ (atan gra-rel gra-rel-wire)
					(yaw state))))))))
		    
		    (x1 (round
			 (+ (/ (width state) 2)
			    (* 
			     (sqrt (+ (expt gra-rel-wire 2)
				      (expt gra-rel-next 2)))
			     (cos (+ (atan gra-rel-next gra-rel-wire)
				     (yaw state)))))))
		    (z1  (round
			  (+ value-shift-pixels
			     (* (cos (pitch state))
				(* value-scaler
				   (3d-dataref func
					       current-wire
					       (cadr wire-crds)
					       ;current-wire
					       )))
			     (* (sin (pitch state))
				(*
				 (sqrt (+ (expt gra-rel-wire 2)
					  (expt gra-rel-next 2)))
				 (sin (+ (atan gra-rel-next gra-rel-wire)
					 (yaw state)))))))))
	       (draw-line x0 z0 x1 z1
			  func (surface state)))
	     )))))

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
    (draw-wireframe-square-wire x-wire z-wire next-z-wire x-xsv-pix
				gra-rel-x gra-centre-z gra-render-radius
				z-dimension value-scaler value-shift-pixels
				state)
    ;;TODO: rename
    (xxxdraw-wireframe-square-wire z-wire x-wire next-x-wire z-xsv-pix
				gra-rel-z gra-centre-x gra-render-radius
				x-dimension value-scaler value-shift-pixels
				state)
    ))

(defun list-square-wire-coordinates (current next step-divisor)
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
		      step-divisor)
		collect crd)
	     (loop
		for crd from current downto next
		by (/ (abs (- next current))
		      step-divisor)
		collect crd))))
    ;;; Enforce list ending with NEXT:
    ;; without this there will be gaps in the wires
    (if (/= (car (last preliminary-list)) next)
	(append preliminary-list (cons next nil))
	preliminary-list)))
