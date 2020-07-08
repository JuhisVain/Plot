(defun render-wireframe-grid ()
  )

(defun render-wireframe (state)
  (declare (3d-state state)
	   ;(optimize speed)
	   )
  (let ((wire-density 1/50)
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
	(value-scaler (/
		       (- (/ (height state) 2) (margin state))
		       (max (abs (min-y state))
			    (abs (max-y state))))))

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

      '(progn
	;; TODO: wireframe grid, make own function, use smart color
	(draw-line corner-x0 corner-y0 corner-x1 corner-y1
	 (car (pfunc-list state))
	 (surface state))
	(draw-line corner-x1 corner-y1 corner-x2 corner-y2
	 (car (pfunc-list state))
	 (surface state))
	(draw-line corner-x2 corner-y2 corner-x3 corner-y3
	 (car (pfunc-list state))
	 (surface state))
	(draw-line corner-x3 corner-y3 corner-x0 corner-y0
	 (car (pfunc-list state))
	 (surface state))
	)	

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
			  (+ (/ (height state) 2) ;; shift coord up
			     (+
			      (* (cos (pitch state))
				 (* value-scaler
				    (aref (data func)
					  array-x-point (round gra-z-coord)))) ; shift by value
			      (* (sin (pitch state))
				 (* (sqrt (+ (expt gra-rel-x 2)
					     (expt gra-rel-z 2)))
				    (sin (+ (atan gra-rel-x gra-rel-z)
					    (yaw state)))))))))

		     (x1 (round
			  (+ (/ (width state) 2)
			     (* 
			      (sqrt (+ (expt gra-rel-x 2)
				       (expt gra-rel-z-next 2)))
			      (cos (+ (atan gra-rel-x gra-rel-z-next)
				      (yaw state)))))))
		     (z1 (round
			  (+ (/ (height state) 2)
			     (* (cos (pitch state))
				(+ (* value-scaler
				      (aref (data func) array-x-point
					    (round (* (1+ z) data-z-step))))))
			     (* (sin (pitch state))
				(* (sqrt (+ (expt gra-rel-x 2)
					    (expt gra-rel-z-next 2)))
				   (sin (+ (atan gra-rel-x gra-rel-z-next)
					   (yaw state)))))))))

		(draw-line x0 z0 x1 z1
			   func (surface state))
		
		)))))

      
      
      )))
