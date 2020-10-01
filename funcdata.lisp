(defclass abstract-funcdata ()
  ((function
    :initarg :func
    :reader funcdata-function)
   (label ; should really be in class drawn, but this is good for debugging.
    :initarg :label
    :reader label)
   (data
    :initarg :data
    :accessor data)
   (arg-count ; does this belong here?
    :initarg :arg-count
    :reader arg-count)))

;; Properties:
(defclass data-res () ;; Data resolution determined on top level funcs
  ((data-per-pixel ;; Could be integer ??or ratio??
    :initform 1
    :initarg :data-per-pixel
    :accessor data-per-pixel)))

(defclass master ()
  ((subs
    :initarg :subs
    :accessor subs)))

(defclass drawn ()
  ((color-real
    :initarg :color-real
    :reader color-real)
   (color-realpart
    :initarg :color-realpart
    :reader color-realpart)
   (color-imagpart
    :initarg :color-imagpart
    :reader color-imagpart)
   (data-min
    :initform NIL
    :accessor data-min)
   (data-max
    :initform NIL
    :accessor data-max)))

(defclass sub ()
  ((master
    :initarg :master
    :reader master)))

(defclass abstract-sub-funcdata (abstract-funcdata sub)
  ())

(defclass abstract-top-funcdata (abstract-funcdata data-res)
  ())

;; Actual function containers:
(defclass top-funcdata (abstract-top-funcdata drawn)
  ())

(defclass master-funcdata (abstract-top-funcdata master)
  ())

(defclass sub-funcdata (abstract-sub-funcdata drawn)
  ())

(defclass submaster-funcdata (abstract-sub-funcdata master)
  ())

(defmethod print-object ((funcdata abstract-funcdata) stream)
  (print-unreadable-object (funcdata stream :type t)
    (let ((subs (when (typep funcdata 'master) (subs funcdata))))
      (format stream ": ~a~[~%~4t~a~;~]"
	      (label funcdata)
	      (if subs 0 1) subs))))

(defmethod data-per-pixel ((sub sub-funcdata))
  "Fetches data resolution from master-funcdata on top level."
  (data-per-pixel (master sub)))

(defmethod form-label ((funcdata abstract-sub-funcdata) id)
  (concatenate 'string
	       (label (master funcdata))
	       "-"
	       (etypecase (funcdata-function funcdata)
		 (symbol (symbol-name (funcdata-function funcdata)))
		 (function (format nil "~a" id)))))

(defmethod form-label ((funcdata abstract-top-funcdata) id)
  (etypecase (funcdata-function funcdata)
    (symbol (symbol-name (funcdata-function funcdata)))
    (function (format nil "~a" id))))

(defmethod initialize-instance :after ((funcdata abstract-funcdata) &key)
  (setf (slot-value funcdata 'label)
	(form-label funcdata (slot-value funcdata 'label)))
  (setf (slot-value funcdata 'function)
	(etypecase (funcdata-function funcdata)
	  (symbol (symbol-function (funcdata-function funcdata)))
	  (function (funcdata-function funcdata)))))

(defun make-funcdata
    (&key
       function resolution-width resolution-height label color-real color-realpart
       color-imagpart master subs (data-per-pixel 1) arg-count)
  (cond ((null (or master subs))
	 (make-instance 'top-funcdata
			:func function
			:label label
			:data (make-array
			       (cons
				(1+ (ceiling
				     (* data-per-pixel
					resolution-width)))
				(when (= 2 arg-count)
				  (list (1+ (ceiling
					     (* data-per-pixel
						resolution-height)))))))
			:data-per-pixel data-per-pixel
			:color-real color-real
			:color-realpart color-realpart
			:color-imagpart color-imagpart
			:arg-count arg-count))
	((and subs (null master))
	 (make-instance 'master-funcdata
			:func function
			:label label
			:data (make-array
			       (cons
				(1+ (ceiling
				     (* data-per-pixel
					resolution-width)))
				(when (= 2 arg-count)
				  (list (1+ (ceiling
					     (* data-per-pixel
						resolution-height)))))))
			:data-per-pixel data-per-pixel
			:subs subs
			:arg-count arg-count))
	((and master (null subs))
	 (make-instance 'sub-funcdata
			:func function
			:label label
			:data (make-array
			       (array-dimensions (data master)))
			:master master
			:color-real color-real
			:color-realpart color-realpart
			:color-imagpart color-imagpart
			:arg-count arg-count))
	((and master subs)
	 (make-instance 'submaster-funcdata
			:func function
			:label label
			:data (make-array
			       (array-dimensions (data master)))
			:master master
			:subs subs
			:arg-count arg-count))))

(defun generate-function-containers (input-func-list input-dimensions)
  (let ((color-stack (generate-colors
		      255 0 0
		      (plottable-count input-func-list)))
	(resolution-width (car input-dimensions))
	(resolution-height (cadr input-dimensions)))
    (labels
	((funcdata-generator (func-list id-counter &optional master)
	   (mapcar #'(lambda (func)
		       (let* ((plist (identify-input-token func))
			      (main-func (getf plist :function))
			      (options (getf plist :options))
			      (subs (getf plist :subs)))
			 
			 (let ((processed-func
				;; NOTE: Could declare color-stack special and
				;; pop it in (make-funcdata) to get aux colors
				(multiple-value-bind
				      (real realpart imagpart)
				    (if (null subs);aka. master
					(aux-colors (pop color-stack))
					;master funcs are not drawn:
					(values nil nil nil)) 

				  (let ((data-per-pixel
					 (or (getf options :data-per-pixel) 1))
					(arg-count
					 (or (getf options :arg-count)
					     (get-arg-count main-func))))
				    (make-funcdata
				     :function main-func
				     :resolution-width resolution-width
				     :resolution-height resolution-height
				     :label (incf id-counter)
				     :color-real real
				     :color-realpart realpart
				     :color-imagpart imagpart
				     :master master
				     :subs subs
				     :data-per-pixel data-per-pixel
				     :arg-count arg-count)))))
			   
			   (when subs
			     (setf (subs processed-func)
				   (funcdata-generator subs 0 processed-func)))
			   
			   processed-func)))
		   func-list)))
      (funcdata-generator input-func-list 0))))


(defun 2d-dataref (funcdata index)
  (declare (abstract-funcdata funcdata)
	   ((float 0.0 1.0) index))
  
  (let* ((array-index (* (array-dimension (data funcdata) 0)
			 index))
	 (f-index (floor array-index))
	 (c-index (ceiling array-index)))
    (/
     (+ (aref (data funcdata) f-index)
	(aref (data funcdata) c-index))
     2)))

(defun 2faref (2d-array x y)
  "Like aref for two dimensional array of numbers 2D-ARRAY,
but indexes X and Y should be floats between 0 and 1.

Returns some kind of weighted average of the 4 array elements forming
square within which indexes X and Y point to."
  (declare ((simple-array * 2) 2d-array) ; array might hold symbols
	   ((single-float 0.0 1.0) x y)
	   (optimize (speed 3)))
  (let* ((x-index (* x (1- (array-dimension 2d-array 0))))
	 (x-floor (floor x-index))
	 (x-ceili (ceiling x-index))
	 
	 (y-index (* y (1- (array-dimension 2d-array 1))))
	 (y-floor (floor y-index))
	 (y-ceili (ceiling y-index))
	 
	 (point-x (rem x-index 1))
	 (point-y (rem y-index 1))
	 (ip-x (- 1.0 point-x)) ; inverted point-x
	 (ip-y (- 1.0 point-y)) ; inverted point-y

	 (weight-ff (* ip-x 
		       ip-y)) 
	 (weight-cf (* point-x ; inverted inverted point-x
		       ip-y))
	 (weight-fc (* ip-x
		       point-y))
	 (weight-cc (* point-x
		       point-y)))

    (/ (+ (* (aref 2d-array x-floor y-floor)
	     weight-ff)
	  (* (aref 2d-array x-floor y-ceili)
	     weight-fc)
	  (* (aref 2d-array x-ceili y-floor)
	     weight-cf)
	  (* (aref 2d-array x-ceili y-ceili)
	     weight-cc))
       (+ weight-ff
	  weight-fc
	  weight-cf
	  weight-cc))))

(defun 3d-dataref (funcdata x y)
  (declare (abstract-funcdata funcdata)
	   (single-float x y)
	   (optimize (speed 3)))
  (handler-case
      (2faref (data funcdata)
	      (min (max x 0.0)
		   1.0)
	      (min (max y 0.0)
		   1.0))
    (type-error () 'TYPE-ERROR)));Value stored in dataset was an error sym



;;;Testing stuff:
(defun list-labels (flist)
  (dolist (f flist)
    (format t "~a," (label f))
    (when (typep f 'master)
      (list-labels (subs f)))))
