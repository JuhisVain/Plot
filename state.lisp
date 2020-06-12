;;; Y axis should always represent the return values of functions
;; X, Z etc. are the inputs.

(defclass state ()
  ((pfunc-list
    :initarg :pfunc-list
    :accessor pfunc-list)
   
   (min-x
    :initarg :min-x
    :accessor min-x)
   (max-x
    :initarg :max-x
    :accessor max-x)

   (max-y
    :initform nil
    :initarg :max-y
    :accessor max-y)
   (min-y
    :initform nil
    :initarg :min-y
    :accessor min-y)

   (draw-labels
    :initarg :draw-labels
    :accessor draw-labels
    :initform t)
   (label-position ; there should be a better solution than this
    :initform 0
    :accessor label-position)

   (style ; for now to be used with 3d-state, either 'heatmap or ???'planes???
    :initarg :style
    :reader style
    :initform 'heatmap)

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
    :accessor min-z)))

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

(defmethod y-range ((state 2d-state))
  "Range of y on grid to be displayed."
  (* (- (max-y state)
	(min-y state))
     (1+ (slack state))))

(defgeneric y-scale (state)
  (:documentation "Returns pixel/value scale."))

(defmethod y-scale ((state 2d-state))
  (if (zerop (y-range state))
      100
      (/ (height state) (y-range state))))

(defmethod slack-pixels ((state 2d-state))
  "Pixels from top or bottom of screen to nearest drawn value."
  (* 1/2
     (slack state)
     (- (max-y state)
	(min-y state))
     (y-scale state)))

(defmethod screen-y0 ((state 2d-state))
  (if (zerop (y-range state))
      (/ (height state) -2)
      (* (min-y state) (y-scale state))))

(defun check-y-extremes (state)
  "Controlled max-y and min-y slot assignment function for state STATE.
Will return T when state changed and NIL if not."
  (let ((old-max (max-y state))
	(old-min (min-y state)))
    (setf (slot-value state 'max-y) (functree-max (pfunc-list state))
	  (slot-value state 'min-y) (functree-min (pfunc-list state)))
    (when (and old-max old-min
	       (or (/= old-max (max-y state))
		   (/= old-min (min-y state))))
      (render-tree state (pfunc-list state))
      t)))

(defmethod render-state ((state 2d-state))
  (sdl:clear-display sdl:*black*)
  (draw-grid (min-y state) (max-y state) (y-range state)
	     (y-scale state) (screen-y0 state)
	     (min-x state) (max-x state) (x-range state)
	     (x-scale state) (screen-x0 state)
	     (slack-pixels state) (surface state))

  (render-func-list (pfunc-list state) (surface state)))


(defmethod initialize-instance :after ((state 2d-state) &key)
  (compute-2d-tree state))

(defun make-state (pfunc-list
		   min max
		   slack
		   &key
		     (surface sdl:*default-display*))
  "Graphs functions in FUNC-LIST from MIN-X to MAX-X, y-scaling is
dynamic based on extreme values on X's range."
  (let* ((state-type (ecase (highest-arg-count pfunc-list)
		       (1 '2d-state)
		       (2 '3d-state)))
	 (min-x (if (listp min) (car min) min))
	 (max-x (if (listp max) (car max) max))
	 (min-z (if (listp min) (cadr min) nil))
	 (max-z (if (listp max) (cadr max) nil))
	 (state
	  (make-instance state-type
			 :pfunc-list pfunc-list
			 :min-x min-x :max-x max-x
			 :min-z min-z :max-z max-z
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

    state))
