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

      (loop
	 for x in
	   (let ((xcrd (loop for a from 0.0 to (/ 1 wire-density)
			  collect (* wire-density a)))) ;build lists to avoid float addition
	     (case far-corner
	       ((max-max min-max) (reverse xcrd))
	       (otherwise xcrd)))
	 with x-vector-pixels = (abs (- corner-x0 corner-x1)) ;width in pixels of line
	 do (loop
	       for z in
		 (let ((zcrd (loop for a from 0.0 to x-vector-pixels
				collect (/ a x-vector-pixels))))
		   (case far-corner
		     ((max-max max-min) (reverse zcrd))
		     (otherwise zcrd)))
	       with gra-rel-x = (* (/ (* (cos (/ pi 4))
					 gra-render-radius)
				      gra-centre-x)
				   (- gra-centre-x
				      (* x (array-dimension
					    (data (car (pfunc-list state)))
					    0))))
	       do (loop for func in (pfunc-list state)
		     with gra-rel-z = (* (/ (* (cos (/ pi 4))
					       gra-render-radius)
					    gra-centre-z)
					 (- gra-centre-z
					    (* z (array-dimension
						  (data (car (pfunc-list state)))
						  1))))

		     and gra-rel-z-next = (* (/ (* (cos (/ pi 4))
						   gra-render-radius)
						gra-centre-z)
					     (- gra-centre-z
						(* (+ (/ 1 x-vector-pixels)
						      z)
						   (array-dimension
						    (data (car (pfunc-list state)))
						    1))))
		     do
		       (let* ((x0 (round
				   ;; shift coord right:
				   (+ (/ (width state) 2) 
				      (*
				       ;; unit multiplier:
				       (sqrt (+ (expt gra-rel-x 2)
						(expt gra-rel-z 2)))
				       ;; unit circle position as determined by state's yaw:
				       (cos (+ (atan gra-rel-x gra-rel-z)
					       (yaw state)))))))
			      (z0 (round
				   (+ value-shift-pixels
				      ;; shift by value, modified by state's pitch:
				      (* (cos (pitch state))
					 (* value-scaler
					    (3d-dataref func z x)))
				      (* (sin (pitch state))
					 (*
					  (sqrt (+ (expt gra-rel-x 2)
						   (expt gra-rel-z 2)))
					  (sin (+ (atan gra-rel-x gra-rel-z)
						  (yaw state))))))))
			      
			      (x1 (round
				   (+ (/ (width state) 2)
				      (* 
				       (sqrt (+ (expt gra-rel-x 2)
						(expt gra-rel-z-next 2)))
				       (cos (+ (atan gra-rel-x gra-rel-z-next)
					       (yaw state)))))))
			      (z1  (round
				    (+ value-shift-pixels
				       (* (cos (pitch state))
					  (* value-scaler
					     (3d-dataref func
							 (+ z (/ 1 x-vector-pixels))
							 x)))
				       (* (sin (pitch state))
					  (*
					   (sqrt (+ (expt gra-rel-x 2)
						    (expt gra-rel-z-next 2)))
					   (sin (+ (atan gra-rel-x gra-rel-z-next)
						   (yaw state)))))))))
			 
			 (draw-line x0 z0 x1 z1
				    func (surface state))))
		 )
	   ))))
