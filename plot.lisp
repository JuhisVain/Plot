(ql:quickload :lispbuilder-sdl)
(load "state.lisp")

(defvar *draw-labels* t
  "The default state of whether or not to write names for plotted data.")

;; There's also 'render-2d-dots:
(defparameter *render-function* 'render-2d-lineplot
  "Funcallable symbol or function, used to generate funcdata-renders.")

(defvar *bad-color* (sdl:color :r 100 :g 0 :b 0))
(defvar *grid-color* (sdl:color :r 50 :g 50 :b 50))
(defvar *grid-origin-color* (sdl:color :r 150 :g 150 :b 150))

(defparameter *draw-functions* nil
  "Storage for funcdatas currently being drawn.")

(defparameter *draw-labels* t)
(defparameter *label-position* 0)

(defstruct plotfunc
  (function) ; master function
  (subs)) ; keys, accessors or whatever to be called with master's value

(defstruct funcdata
  (function nil :type function)
  (color-real)
  (color-realpart)
  (color-imagpart)
  (label nil :type string)
  (data nil :type (or array null))
  (data-min nil :type (or real null))
  (data-max nil :type (or real null))
  (render))

(defstruct (sub-funcdata (:include funcdata))
  (master))

(defun remove-funcdata (funcdata &optional (store *draw-functions*))
  "Destroys FUNCDATA from STORE and frees SDL assets."
  (sdl:free (funcdata-color-real funcdata))
  (sdl:free (funcdata-color-realpart funcdata))
  (sdl:free (funcdata-color-imagpart funcdata))
  (sdl:free (funcdata-render funcdata))
  (setf store (delete funcdata store)))

(defun get-arg-count (func)
  "Returns count of number arguments that FUNC accepts,
one of (1 2 NIL)"
  (declare (optimize (safety 3)))
  (handler-case
      (progn
	(funcall func 13)
	1)
    (simple-condition ()
      (handler-case
	  (progn
	    (funcall func 13 13)
	    2)
	(simple-condition () NIL)))))

(defun draw-pixel (x y surface color)
  (sdl:draw-pixel-* x (- (sdl:height surface) y)
		    :surface surface
		    :color color))

(defun draw-vertical (x color surface &key (mark nil))
  
  (unless (< -1 x (sdl:width surface))
    (return-from draw-vertical))
  
  (sdl:draw-line-* x 0
		   x (sdl:height surface)
		   :surface surface
		   :color color)
  (typecase mark
    (string (sdl:draw-string-solid-* mark (+ x 2) (- (sdl:height surface) 8)
				     :surface surface
				     :color color))))

(defmethod draw-line (x0 (y0 (eql 'zero-division)) x1 y1 function
		      &optional (surface (funcdata-render function)))
  (draw-vertical x0 *bad-color* surface))

(defmethod draw-line (x0 y0 x1 (y1 (eql 'zero-division)) function
		     &optional (surface (funcdata-render function)))
  ;; This situation is already handled by previous for all other coords but last
  (draw-vertical x1 *bad-color* surface))

(defmethod draw-line (x0 (y0 (eql nil)) x1 y1 function
		      &optional (surface (funcdata-render function)))
  (draw-vertical x0 *bad-color* surface))

(defmethod draw-line (x0 y0 x1 (y1 (eql nil)) function
		     &optional (surface (funcdata-render function)))
  (draw-vertical x1 *bad-color* surface))

(defmethod draw-line (x0 (y0 real) x1 (y1 real) function
		     &optional (surface (funcdata-render function)))
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (funcdata-color-real function)))

(defmethod draw-line (x0 (y0 complex) x1 (y1 complex) function
		     &optional (surface (funcdata-render function)))
  (sdl:draw-line-* x0 (- (sdl:height surface) (realpart y0))
		   x1 (- (sdl:height surface) (realpart y1))
		   :surface surface
		   :color (funcdata-color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) (imagpart y0))
		   x1 (- (sdl:height surface) (imagpart y1))
		   :surface surface
		   :color (funcdata-color-imagpart function)))

(defmethod draw-line (x0 (y0 complex) x1 (y1 real) function
		     &optional (surface (funcdata-render function)))
  (sdl:draw-line-* x0 (- (sdl:height surface) (realpart y0))
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (funcdata-color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) (imagpart y0))
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (funcdata-color-imagpart function)))

(defmethod draw-line (x0 (y0 real) x1 (y1 complex) function
		     &optional (surface (funcdata-render function)))
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) (realpart y1))
		   :surface surface
		   :color (funcdata-color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) (imagpart y1))
		   :surface surface
		   :color (funcdata-color-imagpart function)))

(defun draw-circle (x y radius surface color)
  (sdl:draw-circle-* x (- (sdl:height surface) y) radius
		     :surface surface
		     :color color))

(defun draw-horizontal (y color surface &key (mark nil))
  (let ((translated-y (- (sdl:height surface) y)))
    
    ;; If zero axis too for away sdl:draw-line might crap out:
    (unless (< -1 translated-y (sdl:height surface))
      (return-from draw-horizontal))
    
    (sdl:draw-line-* 0 translated-y
		     (sdl:width surface) translated-y
		     :surface surface
		     :color color)
    (typecase mark
      (string (sdl:draw-string-solid-* mark 1 (+ translated-y 2)
				       :surface surface
				       :color color)))))

(defun mark-lines (range)
  "Returns something by gut feeling to be used as multiplier of grid lines."
  (if (zerop range)
      1
      ;; decrement rounded order of magnitude and get its value:
      (expt 10 (1- (round (log range 10))))))

(defun rgb-to-hue (red green blue)
  "Extracts hue from RGB values.
In hue circle red is at 0 and going clockwise next is green and then blue."
  (declare ((integer 0 255) red green blue))
  (atan (* (sqrt 3)
	   (- green blue))
	(- (* 2 red)
	   green
	   blue)))

;; Looks like this works on negative values too
(defun hue-to-rgb (hue)
  "Returns RGB plist based on HUE in radians when saturation
and value are both at max."
  (declare (float hue))
  (flet ((voodoo (n)
	   (let ((k (mod (+ n (/ hue (/ pi 3)))
			 6)))
	     (round
	      (* 255
		 (- 1 (max 0 (min k
				  (- 4 k)
				  1))))))))
    (list :r (voodoo 5)
	  :g (voodoo 3)
	  :b (voodoo 1)
	  :a 255)))

(defun generate-colors (red green blue count)
  (let ((start-hue (rgb-to-hue red green blue))
	(hue-step (/ (* 2 pi)
		     count))
	(colors nil))
    (dotimes (iter count)
      (push (hue-to-rgb
	     (+ start-hue (* iter hue-step)))
	    colors))
    (reverse colors)))

(defun aux-colors (rgb-plist)
  "Returns sdl:colors to be used to draw reals, realparts and imagparts.
RGB-PLIST shoould be a property list like (:r x :g x :b x),
where the Xs are (integer 0 255)."
  (values (apply #'sdl:color rgb-plist)
	  (apply #'sdl:color
		 (mapcar #'(lambda (x) (typecase x
					 (number (round (+ x 255) 2))
					 (t x)))
			 rgb-plist))
	  (apply #'sdl:color
		 (mapcar #'(lambda (x) (typecase x
					 (number (round x 2))
					 (t x)))
			 rgb-plist))))

(defun plottable-count (func-list)
  (labels ((rec-plot-len (flist sum)
	     (typecase (car flist)
	       (null sum)
	       (list 
		(rec-plot-len
		 (cdr flist)
		 (rec-plot-len (cdar flist) sum)))
	       (symbol
		(if (fboundp (car flist))
		    (rec-plot-len (cdr flist) (1+ sum))
		    (rec-plot-len (cdr flist)
				  (rec-plot-len
				   (list (symbol-value (car flist)))
				   sum))))
	       (function (rec-plot-len (cdr flist) (1+ sum))))))
    (rec-plot-len func-list 0)))


;;; to-plotfunc & to-funcdata are parsing functions to transform user's list of
;; symbols / functions into function data containers used by the program
(defun to-plotfunc (funcdata-list &key master)
  "Stores multipart functions' parts into plotfunc structures."
  (mapcar #'(lambda (fdata)
	      (typecase fdata
		(sub-funcdata
		 (setf (sub-funcdata-master fdata) master)
		 fdata)
		(funcdata fdata)
		(list
		 (make-plotfunc
		  :function (progn
			      (when master
				(setf (sub-funcdata-master (car fdata)) master))
			      (car fdata))
		  :subs (to-plotfunc (cdr fdata)
				     :master (car fdata))))))
	  funcdata-list))

;;feed to above
(defun to-funcdata (input-func-list resolution-width)
  (let ((color-stack
	 (generate-colors
	  255 0 0
	  (plottable-count input-func-list))))
    (labels ((funcdata-gen (list id &key (sub-of nil) (is-master nil))
	       ;; Convert list of symbols/funcs to function container list:
	       (mapcar #'(lambda (func)
			   (prog1 ; don't return incremented id
			       (typecase func
				 (function
				  (multiple-value-bind
					(real realpart imagpart)
				      (if is-master
					  (values nil nil nil)
					  (aux-colors (pop color-stack)))
				    (apply
				     (if sub-of
					 #'make-sub-funcdata
					 #'make-funcdata)
				     (list
				      :function func
				      :color-real real
				      :color-realpart realpart
				      :color-imagpart imagpart
				      :label (concatenate 'string
							  sub-of
							  (when sub-of "-")
							  (format nil "~a" id))
				      :data (make-array resolution-width)))))
				 (symbol
				  (multiple-value-bind
					(real realpart imagpart)
				      (if is-master
					  (values nil nil nil)
					  (aux-colors (pop color-stack)))
				    (apply
				     (if sub-of
					 #'make-sub-funcdata
					 #'make-funcdata)
				     (list
				      :function (symbol-function func)
				      :color-real real
				      :color-realpart realpart
				      :color-imagpart imagpart
				      :label (concatenate 'string
							  sub-of
							  (when sub-of "-")
							  (symbol-name func))
				      :data (make-array resolution-width)))))
				 (list
				  (append
				   (funcdata-gen (list (car func))
						 id
						 :sub-of sub-of
						 :is-master t)
				   (funcdata-gen
				    (cdr func) 0
				    :sub-of
				    (concatenate 'string ; master's name
						 sub-of
						 (when sub-of "-")
						 (if (functionp (car func))
						     (format nil "~a" id)
						     (symbol-name (car func))))))))
			     (incf id)))
		       list)))
      (funcdata-gen input-func-list 0))))

(defun plotcall (function index &rest arguments
		 &aux (lindex (if (listp index)
				  index
				  (list index))))
  "Funcall with handlers etc. for plottable data,
stored into array in funcdata FUNCTION's data slot at aref INDEX."
  (let ((value (handler-case
		   (apply (funcdata-function function) arguments)
		 (division-by-zero () 'ZERO-DIVISION)
		 (type-error () nil))))

    (when (numberp value)
      (setf (funcdata-data-max function)
	    (if (funcdata-data-max function)
		(complex-max (funcdata-data-max function)
			     value)
		(complex-max value)))
      (setf (funcdata-data-min function)
	    (if (funcdata-data-min function)
		(complex-min (funcdata-data-min function)
			     value)
		(complex-min value))))
    
    (setf ;;;setfing an applied aref is used as example in the hyperspec!
     (apply #'aref (funcdata-data function) lindex)
     value)))
  
(defun plotfunc-evaluate (plotfunc index &rest arguments)
  (let ((fvalue
	 (apply #'plotcall (plotfunc-function plotfunc) index arguments)))
    ;;; above: PLOTCALL is APPLIED because ARGUMENTS is -of course- a list
    ;; plotcall might produce a symbol to represent an error
    ;; which the user might want to handle himself...
    (mapcar #'(lambda (sub)
		(etypecase sub
		  (funcdata
		   (plotcall sub index fvalue))
		  (plotfunc
		   (apply #'plotfunc-evaluate sub index arguments))))
	    (plotfunc-subs plotfunc))))

(defun extract-numbers (tree)
  "Get flat list of numbers in TREE."
  (loop for element in tree
     append (cond ((listp element)
		   (extract-numbers element))
		  ((numberp element)
		   (list element)))))

(defun get-numbers (arg)
  (typecase arg
    (number (list arg))
    (list (extract-numbers arg))))

(defun complex-max (&rest numbers)
  "Like max, but works on complex numbers returning greatest component."
  (declare (ftype (function (&rest number) real)))
  (apply #'max
	 (mapcar #'(lambda (num)
		     (if (complexp num)
			 (max (imagpart num)
			      (realpart num))
			 num))
		 numbers)))

(defun complex-min (&rest numbers)
  "Like min, but works on complex numbers returning least component."
  (declare (ftype (function (&rest number) real)))
  (apply #'min
	 (mapcar #'(lambda (num)
		     (if (complexp num)
			 (min (imagpart num)
			      (realpart num))
			 num))
		 numbers)))

(defun draw-value (x-coord value y-scale slack screen-y0 surface pfunc)
  (typecase value
    (real (draw-pixel (round x-coord)
		      (round (- (+ (* value y-scale)
				   slack)
				screen-y0))
		      surface
		      (typecase pfunc
			(funcdata
			 (funcdata-color-real pfunc))
			(sdl:color
			 pfunc))))
    ;;the components of a complex must be real:
    (complex (draw-value x-coord
			 (realpart value)
			 y-scale slack screen-y0 surface
			 (funcdata-color-realpart pfunc))
	     (draw-value x-coord
			 (imagpart value)
			 y-scale slack screen-y0 surface
			 (funcdata-color-imagpart pfunc)))
    (t (draw-vertical x-coord ; bad value, most likely zero div
		      *bad-color*
		      surface)))
  NIL)

(defun render-func-list (func-list surface)
  "Blits all funcdata-renders within FUNC-LIST tree onto SURFACE."
  (dolist (func func-list)
    (typecase func
      (plotfunc
       (render-func-list (plotfunc-subs func) surface))
      (funcdata
       (sdl:blit-surface (funcdata-render func) surface))))
  surface)

(defun draw-grid (min-y max-y y-range y-scale screen-y0
		  min-x max-x x-range x-scale screen-x0
		  slack-pixels surface)
  (let ((y-grid-step (mark-lines y-range))
	(x-grid-step (mark-lines x-range)))
    
;;; Draw horizontal grid:
    (loop for y from (- min-y (rem min-y y-grid-step)
			y-grid-step) ; add one line to bottom
       ;; range increased by one line so grid
       ;; extends to all values even with slack:
       to (+ max-y y-grid-step) by y-grid-step
       do (draw-horizontal (round (+ (* y y-scale)
				     (- screen-y0)
				     slack-pixels))
			   *grid-color*
			   surface
			   :mark (format nil "~a" y)))

;;; Draw vertical grid:
    (loop for x from (- min-x (rem min-x x-grid-step))
       to max-x by x-grid-step
       do (draw-vertical (round (- (* x x-scale)
				   screen-x0))
			 *grid-color*
			 surface
			 :mark (format nil "~a" x)))

;;; Draw zeroes
    (draw-horizontal (round (+ (- screen-y0)
			       slack-pixels
			       ))
		     *grid-origin-color*
		     surface
		     :mark "0")

    (draw-vertical (round (- screen-x0))
		   *grid-origin-color*
		   surface
		   :mark "0")))

(defun compute-2d-data (function state)
  "Populates funcdata FUNCTION's (and FUNCTION's subs) data slot's array with
results from applying FUNCTION on values of x from MIN-X to MAX-X by X-STEP."
  (loop
     for x from (min-x state) by (x-step state)
     for i from 0 below (width state)
     do (if (plotfunc-p function)
	    (plotfunc-evaluate function i x)
	    (plotcall function i x))))

(defun compute-2d-tree (state)
  "Computes data for all funcdatas in FUNC-LIST."
  (mapcar #'(lambda (func)
	      (compute-2d-data func state))
	  (pfunc-list state))
  (setf (max-y state) (functree-max (pfunc-list state))
	(min-y state) (functree-min (pfunc-list state))))

(defun render-string (string color &key (color-key sdl:*black*))
  "SDL:RENDER-STRING-SOLID picks it's color key in an unpredictable way.
Renders STRING onto a new surface with color key enabled and set to COLOR-KEY
using COLOR for text."
  (sdl:draw-string-solid-*
   string 0 0
   :surface
   (sdl:create-surface (* (length string)
			  (sdl:char-width sdl:*default-font*))
		       (sdl:char-height sdl:*default-font*)
		       :color-key color-key
		       :type :hw)
   :color color))

(defun render-2d-dots (y-scale slack-pixels screen-y0 function
		       &optional (surface (funcdata-render function)))
  (loop for x from 0 below (sdl:width surface)
     for y across (funcdata-data function)
     do (draw-value x y y-scale slack-pixels screen-y0
		    surface function)))

(defun scale-y (y y-scale slack screen-y0)
  "Transforms value Y into plot's scale.
Result will still need to be inverted before drawing."
  (typecase y
    (real
     (round (- (+ (* y y-scale)
		  slack)
	       screen-y0)))
    (complex
     (complex (round (- (+ (* (realpart y) y-scale)
			   slack)
			screen-y0))
	      (round (- (+ (* (imagpart y) y-scale)
			   slack)
			screen-y0))))
    (t y))); come again! Pass through for non number values

(defun render-2d-lineplot (y-scale slack-pixels screen-y0 function
			   &optional (surface (funcdata-render function)))
  (let ((x-pixel 0))
    (map
     NIL
     #'(lambda (from to)
	 (draw-line x-pixel
		   (scale-y from y-scale slack-pixels screen-y0)
		   (incf x-pixel)
		   (scale-y to y-scale slack-pixels screen-y0)
		   function
		   surface
		   ))
     (funcdata-data function)
     (subseq (funcdata-data function) 1))))
  
(defun render-2d-data (function y-scale slack-pixels screen-y0 surface
		       &key draw-label)
  "Renders individual funcdata FUNCTION onto SURFACE, stored into
FUNCTION's render slot."
  (declare (funcdata function)
	   (sdl:surface surface)
	   (special *label-position*))

  (when (funcdata-render function) ; Free old render
    (sdl:free (funcdata-render function)))
  
  (setf (funcdata-render function) surface)

  (when (and draw-label
	     (< *label-position* (length (funcdata-data function))))
    (let ((string-render
	   (render-string
	    (funcdata-label function)
	    (funcdata-color-real function))))
      ;; Should probably write some smarter position determination...
      ;; update: this is getting ugly
      (sdl:draw-surface-at-* string-render
			     *label-position*
			     (+
			      ;; move label downwards:
			      (sdl:char-height sdl:*default-font*)
			      (- (sdl:height surface)
				 (round
				  (realpart
				   (- (+ slack-pixels
					 (handler-case
					     (* y-scale
						(aref (funcdata-data function)
						      *label-position*))
					; if trying to label zerodiv:
					   (type-error () 0)))
				      screen-y0)))))
			     :surface surface)
      (sdl:free string-render))
    
    (incf *label-position* (* (sdl:char-width sdl:*default-font*)
			      (length (funcdata-label function)))))

  (funcall *render-function* y-scale slack-pixels screen-y0 function surface))

(defun render-2d-tree (func-list y-scale slack-pixels screen-y0 width height
		       &key draw-labels)

    (dolist (func func-list)
      (etypecase func
	(plotfunc (render-2d-tree
		   (plotfunc-subs func)
		   y-scale slack-pixels screen-y0 width height
		   :draw-labels draw-labels))
	(funcdata (render-2d-data
		   func y-scale slack-pixels screen-y0
		   (sdl:create-surface width height :pixel-alpha 255)
		   :draw-label draw-labels)))))

;; Placeholder memory manager
(defun free-assets (pfunc-list)
  (let ((func (car pfunc-list)))
    (typecase func
      (null (return-from free-assets))
      (plotfunc (free-assets (plotfunc-subs func)))
      (funcdata
       (sdl:free (funcdata-color-real func))
       (sdl:free (funcdata-color-realpart func))
       (sdl:free (funcdata-color-imagpart func))
       (sdl:free (funcdata-render func))
       (setf (funcdata-color-real func) nil
	     (funcdata-color-realpart func) nil
	     (funcdata-color-imagpart func) nil
	     (funcdata-render func) nil)))
    (free-assets (cdr pfunc-list))))

(defun free-renders (pfunc-list)
  (let ((func (car pfunc-list)))
    (typecase func
      (null (return-from free-renders))
      (plotfunc (free-renders (plotfunc-subs func)))
      (funcdata
       (sdl:free (funcdata-render func))
       (setf (funcdata-render func) nil)))
    (free-renders (cdr pfunc-list))))

(defun read-input-list (input-func-list dataset-width)
  "Read a function description, store processed pfunc-list to *draw-functions*
and return it."
  (setf
   *draw-functions*
   (to-plotfunc (to-funcdata input-func-list dataset-width))))

(defun process-functree (function tree &key (do-masters t))
  "Funcalls FUNCTION on every funcdata in TREE.
Will ignore plotfunc-function if DO-MASTERS set to nil."
  (dolist (func tree)
    (etypecase func
      (plotfunc (when do-masters
		  (funcall function (plotfunc-function func)))
		(process-functree function (plotfunc-subs func)))
      (funcdata (funcall function func)))))

(defun functree-max (tree)
  "Returns greatest y-value to draw from tree."
  (let ((max))
    (process-functree
     #'(lambda (f)
	 (setf max
	       (if max
		   (max max (funcdata-data-max f))
		   (funcdata-data-max f))))
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
		   (min min (funcdata-data-min f))
		   (funcdata-data-min f))))
     tree
     :do-masters nil)
    min))

;;This is now just an instance init, todo rename or remove
(defun draw-function (pfunc-list
		      min-x max-x
		      slack
		      &optional
			(surface sdl:*default-display*))
  "Graphs functions in FUNC-LIST from MIN-X to MAX-X, y-scaling is
dynamic based on extreme values on X's range."

  
  
  (let* ((state
	  (make-instance '2d-state
			 :pfunc-list pfunc-list
			 :min-x min-x :max-x max-x
			 :slack slack
			 :surface surface)))
    
    (format t "max ~a min ~a~%" (max-y state) (min-y state))

    ;;debug:
    (format t "y-range ~a~%slack-pix ~a~%y-scale ~a~%
screen-y0 ~a and x0 ~a, x-scale: ~a~%"
	    (y-range state)
	    (slack-pixels state)
	    (y-scale state)
	    (screen-y0 state)
	    (screen-x0 state)
	    (x-scale state))

    state
    
    ))

;; Let's go with elements in func-list as (func key-list) or just func
;; key-list is list of functions to be applied to func's result
;;for example:
;; (plot (list
;;         (list #'sqrt #'imagpart #'realpart)
;;         (list #'log #'imagpart #'realpart))
;;       :from -11 :to -1)

;; Used in determining what to update with button bindings:
(defun find-containers (function containers)
  "Returns list of highest containers within tree CONTAINERS with FUNCTION
as main function."
  (let ((found nil))
    (dolist (container containers)
      (setf found
      (nconc
       found
       (typecase container
	 (plotfunc (if (eql (funcdata-function (plotfunc-function container))
			    function)
		       (list container)
		       (find-containers function (plotfunc-subs container))))
	 (funcdata (if (eql (funcdata-function container)
			    function)
		       (list container)))))))
    found))
       
(defun member-sub (function plotfunc)
  "Returns T if FUNCTION is found in funcdata-function slot in any
 of PLOTFUNC's subfunctions at any depth."
  (declare (function function)
	   (plotfunc plotfunc))
  (dolist (sub (plotfunc-subs plotfunc))
    (when
	(if (plotfunc-p sub)
	    (member-sub function sub)
	    (eq function (funcdata-function sub)))
      (return-from member-sub t))))

(defun master-funcdata (func &optional (root-level *draw-functions*))
  "Find root function containers for function FUNC in tree ROOT-LEVEL."
  (let ((masters nil))
    (dolist (master root-level)
      (typecase master
	(funcdata (when (eq func (funcdata-function master))
		    (push master masters)))
	(plotfunc (when (member-sub func master)
		    (push master masters)))))
    masters))

;;; Bindings key args should be in list of form
;;  '(increase-button
;;    decrease-button
;;    dynamic-variable
;;    {number-delta|function-delta}
;;    [(functions-to-redraw)])
;;;; for example:
;;  (q a *ding-dong* #'(lambda () (* 0.1 *ding-dong*)) '(my-func other-func))

(defun button-to-sdlkey (button-name)
  "Will transform an ASCII key into a keyword understod by lispbuilder."
  (intern
   (concatenate 'string "SDL-KEY-"
		(string-upcase (string button-name)))
   "KEYWORD"))

(defstruct binding
  (action nil :type function)
  (functions nil :type list)) ; list of function containers to recompute

(defun make-binding-hash-table (bindings state)
  (let ((hash-table
	 (make-hash-table :size (* 2 (length bindings)))))
    (dolist (binding bindings)
      (destructuring-bind
	    (inc-button dec-button dyn-var delta func-list)
	  binding
	
	(unless (boundp dyn-var)
	  (format t "Symbol ~a has not been dynamically bound!" dyn-var))
	
	(let ((to-update
	       (or ;; failsafe, in case no matches -> update everything
		;; would be smarter to only update anonymous funcs
		(reduce #'append
			(mapcar #'(lambda (fun)
				    (find-containers
				     (typecase fun
				       (function fun)
				       (symbol (symbol-function fun)))
				     (pfunc-list state)))
				func-list))
		(pfunc-list state))))
	  
	  (setf (gethash (button-to-sdlkey inc-button)
			 hash-table)
		(make-binding
		 :action
		 #'(lambda ()
		     (incf (symbol-value dyn-var)
			   (etypecase delta
			     (number delta)
			     (function (funcall delta)))))
		 :functions to-update))
	  (setf (gethash (button-to-sdlkey dec-button)
			 hash-table)
		(make-binding
		 :action
		 #'(lambda ()
		     (decf (symbol-value dyn-var)
			   (etypecase delta
			     (number delta)
			     (function (funcall delta)))))
		 :functions to-update)))))
    hash-table))

(defun call-binding (button bindings-table state)
  "Funcalls binding on button"
  (let ((binding (gethash button bindings-table)))
    (unless binding
      (format t "No bindings on ~a~%" button)
      (return-from call-binding nil))
    
    (funcall (binding-action binding))
    (dolist (to-update (binding-functions binding))
      (compute-2d-data to-update state))
    t))

(defun plot (func-list
	     &key (from 0) (to 100) (slack 1/20)
	       (window-width 500) (window-height 500)
	       (draw-labels *draw-labels*)
	       bindings)
  (declare ((rational 0 1) slack))
  (let ((processed-func-list
	 (read-input-list func-list window-width))
	(binding-hash-table nil)
	(state nil))
    (sdl:initialise-default-font)
    (sdl:with-init()
      (sdl:window window-width window-height
		  :title-caption "plot"
		  :hw t
		  :bpp 32)

      (setf state
	    (time ; might want to do some custom logging also/instead
	     (draw-function
	      processed-func-list
	      from to slack)))

      (setf binding-hash-table (make-binding-hash-table bindings state))
      (render-state state)

      (sdl:update-display)
      
      (sdl:with-events (:poll)
	(:quit-event
	 ()
	 (free-assets *draw-functions*)
	 t)

	(:key-down-event
	 (:key key)
	 (setf *label-position* 0)
	 (format t "Pressed: ~a~%" key)

	 (when (call-binding key binding-hash-table state)
	   
	   (sdl:clear-display sdl:*black*)

	   (setf (max-y state) (functree-max (pfunc-list state))
		 (min-y state) (functree-min (pfunc-list state)))

	   (format t "STATE: max-y ~a, min-y ~a~%" (max-y state) (min-y state))

	   ;; Must reset funcdata-data extremes so we'll get correct extremes
	   ;; next time when the new values are within old extremes
	   ;; I think this should be done inside call-binding
	   (process-functree ; should move to somewhere smarter
	    #'(lambda (func)
		(setf (funcdata-data-min func) nil
		      (funcdata-data-max func) nil))
	    processed-func-list)

	   (render-state state)
	   (sdl:update-display)
	   ))
	
	(:idle
	 ()
	 )))))
