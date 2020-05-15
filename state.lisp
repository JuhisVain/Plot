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
   (label-position
    :initform 0
    :accessor label-position)

   (surface
    :initarg :surface
    :accessor surface)))

(defclass 2d-state (state)
  ((slack
    :initarg :slack
    :accessor slack)))

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


;; TODO: what if max-y and min-y change at same time?
(defmethod (setf max-y) (new-max-y (state state))
  (let ((old (max-y state)))
    (setf (slot-value state 'max-y) new-max-y)
    (when (and old (/= new-max-y old)) ; state changed, rerender all
      (render-2d-tree state (pfunc-list state)))))

(defmethod (setf min-y) (new-min-y (state state))
  (let ((old (min-y state)))
    (setf (slot-value state 'min-y) new-min-y)
    (when (and old (/= new-min-y old)) ; state changed, rerender all
      (render-2d-tree state (pfunc-list state)))))

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
