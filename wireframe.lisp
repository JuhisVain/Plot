(defun render-wireframe-grid (state value-scaler x0 y0 x1 y1 x2 y2 x3 y3)
  (let ((grid-line-delta (* value-scaler
			    (mark-lines (- (max-y state)
					   (min-y state)))))
        )
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
	 (sdl:draw-line-* x0(- (height state)
			       (round
				(+ (* (cos (pitch state)) y) y0)))
			  x1 (- (height state)
				(round
				 (+ (* (cos (pitch state)) y) y1)))
			  :color *grid-color*
			  :surface (surface state))
	 (sdl:draw-line-* x1 (- (height state)
				(round
				 (+ (* (cos (pitch state)) y) y1)))
			  x2 (- (height state)
				(round
				 (+ (* (cos (pitch state)) y) y2)))
			  :color *grid-color*
			  :surface (surface state))
	 
	 (sdl:draw-line-* x2 (- (height state)
				(round
				 (+ (* (cos (pitch state)) y) y2)))
			  x3 (- (height state)
				(round
				 (+ (* (cos (pitch state)) y) y3)))
			  :color *grid-color*
			  :surface (surface state))

	 (sdl:draw-line-* x3 (- (height state)
				(round
				 (+ (* (cos (pitch state)) y) y3)))
			  x0 (- (height state)
				(round
				 (+ (* (cos (pitch state)) y) y0)))
			  :color *grid-color*
			  :surface (surface state)))

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
    
    ))

(defun render-wireframe (state)
  (declare (3d-state state)
	   ;(optimize speed)
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
	 )

    (format t "value-shift-pixels ~a~%value-scaler ~a~%"
	    value-shift-pixels
	    value-scaler)

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

      (let* (;;the surface X vector length of a continuous line:
	     (x-vector-pixels (abs (- corner-x0 corner-x1)))
	     ;;how much to increment data array reference point:
	     (data-z-step (/ (array-dimension
			      (data (car (pfunc-list state))) 1)
			     x-vector-pixels))
	     (data-x-step (round (* wire-density ; 0 1 2 3 
				    (array-dimension
				     (data (car (pfunc-list state))) 0)))))
	
	(dotimes (x-wire (/ 1 wire-density))
	  
	  (dotimes (z (1- x-vector-pixels)) ; 0 1 2 3
	    (dolist (func (pfunc-list state))
	      
	      (let* ((array-x-point (* x-wire data-x-step))
		     (gra-z-coord (* z data-z-step))
		     (gra-rel-x (* (/ (* (cos (/ pi 4))
					 gra-render-radius)
				      (/ (width state) 2))
				   (- gra-centre-x array-x-point)))
		     (gra-rel-z (* (/ (* (cos (/ pi 4))
					 gra-render-radius)
				      (/ (height state) 2))
				   (- gra-centre-z gra-z-coord)))
		     (gra-rel-z-next (* (/ (* (cos (/ pi 4))
					      gra-render-radius)
					   (/ (height state) 2))
					(- gra-centre-z
					   (+ data-z-step gra-z-coord))))

		     (x0 (round
			  (+ (/ (width state) 2) ;; shift coord right
			     (* 
			      (sqrt (+ (expt gra-rel-x 2)
				       (expt gra-rel-z 2)))
			      (cos (+ (atan gra-rel-x gra-rel-z)
				      (yaw state)))))))
		     (z0 (round
			   (+ value-shift-pixels
			    (* (cos (pitch state))
			       (* value-scaler
				  (aref (data func)
					(round gra-z-coord)
					array-x-point))) ; shift by value
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
		     (z1 (round
			  (+ value-shift-pixels
			     (* (cos (pitch state))
				(* value-scaler
				   (aref (data func)
					 (round (* (1+ z) data-z-step))
					 array-x-point)))
			     (* (sin (pitch state))
				(*
				 (sqrt (+ (expt gra-rel-x 2)
					  (expt gra-rel-z-next 2)))
				 (sin (+ (atan gra-rel-x gra-rel-z-next)
					 (yaw state)))))))))
		

		(draw-line x0 z0 x1 z1
			   func (surface state))
		
		)))))

      
      
      )))
