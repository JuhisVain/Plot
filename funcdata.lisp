(defclass abstract-funcdata ()
  ((function
    :initarg :func
    :reader func)
   (label ;; maybe move to class drawn?
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

;; Actual function containers:
(defclass top-funcdata (abstract-funcdata data-res drawn)
  ())

(defclass master-funcdata (top-funcdata master)
  ())

(defclass sub-funcdata (abstract-sub-funcdata drawn)
  ())

(defclass submaster-funcdata (abstract-sub-funcdata master)
  ())

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

(defmethod form-label ((funcdata top-funcdata) id)
  (etypecase (func funcdata)
    (symbol (symbol-name (func funcdata)))
    (function (format nil "~a" id))))

(defmethod initialize-instance :after
    ((funcdata abstract-funcdata) &key (label-id -1))
  (setf (slot-value funcdata 'label)
	(form-label funcdata label-id))
  (setf (slot-value funcdata 'function)
	(etypecase (func funcdata)
	  (symbol (symbol-function (func funcdata)))
	  (function (func funcdata)))))

;;;; TODO: make unified constructor (make-funcdata)

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
				(multiple-value-bind
				      (real realpart imagpart)
				    (if (null subs);aka. master
					(aux-colors (pop color-stack))
					(values nil nil nil)) ;master funcs are not drawn
				  (apply #'make-instance
					 (append
					  (cond ((null (or subs master))
						 (list 'top-funcdata
						       :color-real real
						       :color-realpart realpart
						       :color-imagpart imagpart))
						((and (null subs) master)
						 (list 'sub-funcdata
						       :master master
						       :color-real real
						       :color-realpart realpart
						       :color-imagpart imagpart))
						((and subs (null master))
						 (list 'master-funcdata))
						((and subs master)
						 (list 'submaster-funcdata
						       :master master)))
					  (list
					   :func main-func
					   :data (make-array resolution-width)
					   :label-id (incf id-counter)))))))
			   (when subs
			     (setf (subs processed-func)
				   (funcdata-generator subs 0 processed-func))
			     )
			   processed-func)
			 ))
		   func-list)))
      (funcdata-generator input-func-list 0))))

;;;Testing stuff:
(defun list-labels (flist)
  (dolist (f flist)
    (format t "~a," (label f))
    (when (typep f 'master)
      (list-labels (subs f)))))
