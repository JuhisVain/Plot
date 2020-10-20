(in-package :plot)

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
    :accessor color-real)
   (color-realpart
    :initarg :color-realpart
    :accessor color-realpart)
   (color-imagpart
    :initarg :color-imagpart
    :accessor color-imagpart)
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


(defmethod plotcall ((funcdata abstract-top-funcdata)
		     index &rest arguments
		     &aux (lindex (if (listp index)
				      index
				      (list index))))
  (let ((value (handler-case
		   (apply (funcdata-function funcdata) arguments)
		 (division-by-zero () 'ZERO-DIVISION)
		 (type-error () nil))))

    (setf ;;;setfing an applied aref is used as example in the hyperspec!
     (apply #'aref (data funcdata) lindex)
     value)

    (call-next-method)))

(defmethod plotcall ((funcdata abstract-sub-funcdata)
		     index &rest arguments
		     &aux (lindex (if (listp index)
				      index
				      (list index))))
  (declare (ignore arguments))
  (let ((value (handler-case
		   (apply (funcdata-function funcdata)
			  (list
			   (apply #'aref (data (master funcdata)) lindex)))
		 (division-by-zero () 'ZERO-DIVISION)
		 (type-error () nil))))
    (setf
     (apply #'aref (data funcdata) lindex)
     value)
    (call-next-method)))

(defmethod plotcall ((funcdata master)
		     index &rest arguments
		     &aux (lindex (if (listp index)
				      index
				      (list index))))
  (declare (ignore arguments lindex))
  (dolist (sub (subs funcdata))
    (plotcall sub index NIL))) ; arguments ignored

(defmethod plotcall ((funcdata drawn)
		     index &rest arguments
		     &aux (lindex (if (listp index)
				      index
				      (list index))))
  (declare (ignore arguments))
  (let ((value (apply #'aref (data funcdata) lindex)))
    (when (numberp value)
      (setf (data-max funcdata)
	    (if (data-max funcdata)
		(complex-max (data-max funcdata)
			     value)
		(complex-max value)))
      (setf (data-min funcdata)
	    (if (data-min funcdata)
		(complex-min (data-min funcdata)
			     value)
		(complex-min value))))))

(defun faref (array index)
  "Linear interpolating aref for one dimensional arrays."
  (declare ((simple-array * 1) array)
	   ((single-float 0.0 1.0) index))
  (let* ((array-index (* index (array-dimension array 0)))
	 (c-index (ceiling array-index)))
    (multiple-value-bind (f-index f-weight)
	(floor array-index)
      (+ (* (aref array f-index) (- 1 f-weight))
	 (* (aref array c-index) f-weight)))))

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
