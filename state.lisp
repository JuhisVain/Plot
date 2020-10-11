;;; Y axis should always represent the return values of functions
;; X, Z etc. are the inputs.

(defclass state ()
  ((pfunc-list
    :initarg :pfunc-list
    :accessor pfunc-list)
   (drawn-list
    :initarg :drawn-list
    :accessor drawn-list)
   
   (min-x
    :initarg :min-x
    :accessor min-x)
   (max-x
    :initarg :max-x
    :accessor max-x)

   (max-y
    :initform 1
    :initarg :max-y
    :reader max-y)
   (min-y
    :initform 0
    :initarg :min-y
    :reader min-y)

   (draw-labels
    :initarg :draw-labels
    :accessor draw-labels
    :initform t)
   (label-position ; there should be a better solution than this
    :initform 0
    :accessor label-position)

   (surface
    :initarg :surface
    :accessor surface)))

(defclass 2d-state (state)
  ((slack
    :initarg :slack
    :accessor slack)))

(defclass 3d-state (state)
  ((max-z
    :initform nil
    :initarg :max-z
    :accessor max-z)
   (min-z
    :initform nil
    :initarg :min-z
    :accessor min-z)

   (yaw
    :initform (* 9/8 +sf-pi+)
    :initarg :yaw
    :accessor yaw
    :type single-float)
   (pitch
    :initform (* 1/4 +sf-pi+)
    :initarg :pitch
    :accessor pitch)
   ))

(defclass with-value-grid ()
  ())

(defclass 2d-plot-with-grid (2d-state with-value-grid)
  ())

(defclass heatmap (3d-state) ; for use with one func only
  ())

(defclass sequential-heatmap (3d-state) ; for 1 to 3 funcs
  ())

(defclass wireframe (3d-state) ; for 1 to n funcs
  ((margin
    :initform 10
    :initarg :margin
    :accessor margin)
   (wire-density
    :initform 1/50
    :initarg :wire-density
    :accessor wire-density)))

(defclass wireframe-with-grid (wireframe with-value-grid)
  ())

;;;; Auxiliary attributes:

(defmethod width ((state state))
  "Returns width of bottom surface."
  (sdl:width (surface state)))

(defmethod height ((state state))
  "Returns height of bottom surface."
  (sdl:height (surface state)))

(defmethod x-range ((state state))
  (- (max-x state) (min-x state)))

(defmethod x-scale ((state state))
  (/ (width state) (x-range state)))

(defmethod x-step ((state state))
  (/ (x-range state) (width state)))

(defmethod screen-x0 ((state 2d-state))
  (* (min-x state)
     (/ (width state)
	(x-range state))))

(defmethod z-range ((state 3d-state))
  (- (max-z state) (min-z state)))

(defmethod z-scale ((state 3d-state))
  (/ (height state) (z-range state)))

(defmethod z-step ((state 3d-state))
  (/ (z-range state) (height state)))

(defmethod y-range ((state 2d-state))
  "Range of y on grid to be displayed."
  (* (- (max-y state)
	(min-y state))
     (1+ (slack state))))

(defmethod y-range ((state 3d-state))
  (coerce 
   (- (max-y state)
      (min-y state))
   'FLOAT))

(defgeneric y-scale (state)
  (:documentation "Returns pixel/value scale."))

(defmethod y-scale ((state 2d-state))
  (if (zerop (y-range state))
      100
      (/ (height state) (y-range state))))

(defmethod y-scale ((state wireframe))
  (/ (- (height state)
	(* 2 (margin state)))
     (y-range state)
     2))

(defmethod slack-pixels ((state 2d-state))
  "Pixels from top or bottom of screen to nearest drawn value."
  (* 1/2
     (slack state)
     (- (max-y state)
	(min-y state))
     (y-scale state)))

(defgeneric screen-y0 (state)
  (:documentation
   "Distance in pixels from screen bottom to zero value line/plane."))

(defmethod screen-y0 ((state 2d-state))
  (if (zerop (y-range state))
      (/ (height state) -2)
      (* (min-y state) (y-scale state))))

(defmethod screen-y0 ((state wireframe))
  (coerce
   (+ (/ (height state) 2)
      (* (cos (pitch state))
	 (-
	  (* (y-scale state)
	     (/ (+ (min-y state)
		   (max-y state))
		2)))))
   'SINGLE-FLOAT))

(defmethod render-radius ((state wireframe))
  (coerce
   (/ (- (min (width state)
	      (height state))
	(* 2 (margin state)))
      2)
   'SINGLE-FLOAT))

(defmethod (setf pitch) :after (new (state 3d-state))
  (cond ((>= (pitch state)
	     (* 2 +sf-pi+))
	 (decf (pitch state) (* 2 +sf-pi+)))
	((< (pitch state)
	    0.0)
	 (incf (pitch state) (* 2 +sf-pi+)))
	(t (pitch state))))

(defmethod (setf yaw) :after (new (state 3d-state))
  (cond ((>= (yaw state)
	     (* 2 +sf-pi+))
	 (decf (yaw state) (* 2 +sf-pi+)))
	((< (yaw state)
	    0.0)
	 (incf (yaw state) (* 2 +sf-pi+)))
	(t (yaw state))))

(defun highest-arg-count (pfunc-list)
  (apply #'max
	 (mapcar #'(lambda (pf)
		     (max (arg-count pf)
			  (if (typep pf 'master)
			      (highest-arg-count (subs pf))
			      0)))
		 pfunc-list)))

(defun process-functree (function tree &key (do-masters t))
  "Funcalls FUNCTION on every funcdata in TREE.
Will ignore plotfunc-function if DO-MASTERS set to nil."
  (dolist (func tree)
    (typecase func
      (drawn (funcall function func))
      (t (when do-masters
	   (funcall function (funcdata-function func)))
	 (process-functree function (subs func)))
      )))

(defun functree-max (tree)
  "Returns greatest y-value to draw from tree."
  (let ((max))
    (process-functree
     #'(lambda (f)
	 (setf max
	       (if max
		   (max max (data-max f))
		   (data-max f))))
     tree
     :do-masters nil)
    max))

(defun functree-min (tree)
  "Returns smallest y-value to draw from a tree."
  (let ((min))
    (process-functree
     #'(lambda (f)
	 (setf min
	       (if min
		   (min min (data-min f))
		   (data-min f))))
     tree
     :do-masters nil)
    min))

(defgeneric check-y-extremes (state)
  )

(defmethod check-y-extremes :around ((state state))
  "Returns T when state changed, NIL if not."
  (let ((old-max (max-y state))
	(old-min (min-y state)))
    (call-next-method)
    (when (or (/= old-max (max-y state))
	      (/= old-min (min-y state)))
      t)))

(defmethod check-y-extremes ((state state))
  "Find functree dataset limits."
  (setf (slot-value state 'max-y) (functree-max (pfunc-list state))
	(slot-value state 'min-y) (functree-min (pfunc-list state)))
  
  ;; min-y and max-y should not be equal:
  (when (= (slot-value state 'min-y)
	   (slot-value state 'max-y))
    (decf (slot-value state 'min-y) 0.1)
    (incf (slot-value state 'max-y) 0.1)))

(defmethod check-y-extremes :after ((state with-value-grid))
  "Align maxima and minima with order of magnitude."
  ;;;This will force grid to be drawn with the value axis beginning and ending
  ;; at values flush with the order of magnitude of the lines of the grid.
  (let* ((old-max (max-y state))
	 (old-min (min-y state))
	 (alignment (mark-lines (- old-max old-min))))
    (setf (slot-value state 'max-y)
	  (* alignment
	     (ceiling old-max alignment)))
    (setf (slot-value state 'min-y)
	  (* alignment
	     (floor old-min alignment)))))

(defmethod render-state :around (state)
  (let ((before (get-internal-real-time))
	(result
	  (progn (sdl:clear-display sdl:*black*)
		 (call-next-method)))
	(after (get-internal-real-time)))
    (format t "Rendering time: ~a~%" (- after before))
    result))

(defmethod render-state :before ((state 2d-plot-with-grid))
  (draw-grid (min-y state) (max-y state) (y-range state)
	     (y-scale state) (screen-y0 state)
	     (min-x state) (max-x state) (x-range state)
	     (x-scale state) (screen-x0 state)
	     (slack-pixels state) (surface state)))

(defmethod render-state :before ((state wireframe-with-grid))
  (render-wireframe-grid state))

(defmethod render-state ((state state))
  (render-funcs state))

(defmethod initialize-instance :after ((state state) &key)
  (compute-tree state))

(defun collect-drawn (pfunc-list)
  (let ((drawns nil))
    (labels ((rec-col (flist)
	       (dolist (func flist)
		 (typecase func
		   (master
		    (rec-col (subs func)))
		   (drawn
		    (push func drawns))))))
      (rec-col pfunc-list)
      (reverse drawns))))

(defun generate-pfunc-colors (state)
  "Adds colors to drawn funcdatas."
  (macrolet ((set-colors (function color)
	       `(multiple-value-bind (real realpart imagpart)
		    (aux-colors ,color)
		  (setf (color-real ,function) real
			(color-realpart ,function) realpart
			(color-imagpart ,function) imagpart))))
    (let ((drawns (drawn-list state)))
      (typecase state
	(heatmap nil)
	(sequential-heatmap nil)
	((or 2d-state wireframe)
	 (loop for func in drawns
	       for color in (generate-colors 255 0 0 (length drawns))
	       do (set-colors func color)))))))

(defun make-state (pfunc-list
		   min max
		   wire-density
		   plot-type
		   slack
		   &key
		     (surface sdl:*default-display*))
  "Initialize plotting state for functions in FUNC-LIST from MIN-X to MAX-X,
y-scaling is dynamic based on extreme values on X's range."
  (let* ((state-type (ecase (highest-arg-count pfunc-list)
		       (1 '2d-state)
		       (2 '3d-state)))
	 (min-x (if (listp min) (car min) min))
	 (max-x (if (listp max) (car max) max))
	 (min-z (if (listp min) (cadr min) nil))
	 (max-z (if (listp max) (cadr max) nil))
	 (state
	  (make-instance (if (and plot-type
				  (subtypep plot-type state-type))
			     (case plot-type
			       (heatmap (if (> (length pfunc-list) 1)
					    'sequential-heatmap
					    'heatmap))
			       (t plot-type))
			     (case state-type
			       (2d-state '2d-plot-with-grid)
			       (3d-state 'wireframe-with-grid)))
			 :pfunc-list pfunc-list
			 :drawn-list (collect-drawn pfunc-list)
			 :min-x (or min-x 0) :max-x (or max-x 100)
			 :min-z (or min-z min-x) :max-z (or max-z max-x)
			 :wire-density wire-density
			 :slack slack
			 :surface surface
			 :allow-other-keys t)))
    
    (format t "max ~a min ~a~%" (max-y state) (min-y state))
    ;;debug:
    (typecase state
      (2d-state (format t "y-range ~a~%slack-pix ~a~%y-scale ~a~%
screen-y0 ~a and x0 ~a, x-scale: ~a~%"
			(y-range state)
			(slack-pixels state)
			(y-scale state)
			(screen-y0 state)
			(screen-x0 state)
			(x-scale state))))

    (generate-pfunc-colors state)
    state))
