(defclass abstract-funcdata ()
  ((function
    :initarg :func
    :reader func)
   (label ;; maybe move to class drawn?
    :initarg :label
    :reader label)
   (data
    :initarg :data
    :accessor data)
   (data-min
    :accessor data-min)
   (data-max
    :accessor data-max)
   ;; TODO: after object implementation ->
   ;;investigate whether masters / submasters should be rendered
   (render
    :accessor render)))

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
    :reader color-imagpart)))

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
	       (etypecase (func funcdata)
		 (symbol (symbol-name (func funcdata)))
		 (function (format nil "~a" id)))))

(defmethod form-label ((funcdata abstract-top-funcdata) id)
  (etypecase (func funcdata)
    (symbol (symbol-name (func funcdata)))
    (function (format nil "~a" id))))

(defmethod initialize-instance :after ((funcdata abstract-funcdata) &key)
  (setf (slot-value funcdata 'label)
	(form-label funcdata (slot-value funcdata 'label)))
  (setf (slot-value funcdata 'function)
	(etypecase (func funcdata)
	  (symbol (symbol-function (func funcdata)))
	  (function (func funcdata)))))

(defun make-funcdata
    (&key
       function data label color-real color-realpart
       color-imagpart master subs (data-per-pixel 1))
  (cond ((null (or master subs))
	 (make-instance 'top-funcdata
			:func function
			:label label
			:data data
			:data-per-pixel data-per-pixel
			:color-real color-real
			:color-realpart color-realpart
			:color-imagpart color-imagpart))
	((and subs (null master))
	 (make-instance 'master-funcdata
			:func function
			:label label
			:data data
			:data-per-pixel data-per-pixel
			:subs subs))
	((and master (null subs))
	 (make-instance 'sub-funcdata
			:func function
			:label label
			:data data
			:master master
			:color-real color-real
			:color-realpart color-realpart
			:color-imagpart color-imagpart))
	((and master subs)
	 (make-instance 'submaster-funcdata
			:func function
			:label label
			:data data
			:master master
			:subs subs))))

(defun generate-function-containers (input-func-list resolution-width)
  (let ((color-stack (generate-colors
		      255 0 0
		      (plottable-count input-func-list))))
    (labels
	((funcdata-generator (func-list id-counter &optional master)
	   (mapcar #'(lambda (func)
		       (destructuring-bind
			     (main-func &rest subs)
			   (if (listp func) func (list func))
			 (let ((processed-func
				;; NOTE: Could declare color-stack special and
				;; pop it in (make-funcdata) to get aux colors
				(multiple-value-bind
				      (real realpart imagpart)
				    (if (null subs);aka. master
					(aux-colors (pop color-stack))
					;master funcs are not drawn:
					(values nil nil nil)) 

				  (make-funcdata
				   :function main-func
				   :data (make-array resolution-width)
				   :label (incf id-counter)
				   :color-real real
				   :color-realpart realpart
				   :color-imagpart imagpart
				   :master master
				   :subs subs))))
			   
			   (when subs
			     (setf (subs processed-func)
				   (funcdata-generator subs 0 processed-func)))
			   
			   processed-func)))
		   func-list)))
      (funcdata-generator input-func-list 0))))

;;;Testing stuff:
(defun list-labels (flist)
  (dolist (f flist)
    (format t "~a," (label f))
    (when (typep f 'master)
      (list-labels (subs f)))))
