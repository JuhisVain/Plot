(ql:quickload :lispbuilder-sdl)
(load "state.lisp")
(load "funcdata.lisp")

(defvar *draw-labels* t
  "The default state of whether or not to write names for plotted data.")

;; There's also 'render-2d-dots:
(defparameter *render-function* 'render-2d-lineplot
  "Funcallable symbol or function, used to generate funcdata-renders.")

(defvar *bad-color* (sdl:color :r 100 :g 0 :b 0))
(defvar *grid-color* (sdl:color :r 50 :g 50 :b 50))
(defvar *grid-origin-color* (sdl:color :r 150 :g 150 :b 150))

(defvar *transparent* (sdl:color :r 0 :g 0 :b 0 :a 0)) ; alpha 0 is transparent

(defparameter *draw-functions* nil
  "Storage for funcdatas currently being drawn.")

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
		      &optional (surface (render function)))
  (draw-vertical x0 *bad-color* surface))

(defmethod draw-line (x0 y0 x1 (y1 (eql 'zero-division)) function
		     &optional (surface (render function)))
  ;; This situation is already handled by previous for all other coords but last
  (draw-vertical x1 *bad-color* surface))

(defmethod draw-line (x0 (y0 (eql nil)) x1 y1 function
		      &optional (surface (render function)))
  (draw-vertical x0 *bad-color* surface))

(defmethod draw-line (x0 y0 x1 (y1 (eql nil)) function
		     &optional (surface (render function)))
  (draw-vertical x1 *bad-color* surface))

(defmethod draw-line (x0 (y0 real) x1 (y1 real) function
		     &optional (surface (render function)))
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (color-real function)))

(defmethod draw-line (x0 (y0 complex) x1 (y1 complex) function
		     &optional (surface (render function)))
  (sdl:draw-line-* x0 (- (sdl:height surface) (realpart y0))
		   x1 (- (sdl:height surface) (realpart y1))
		   :surface surface
		   :color (color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) (imagpart y0))
		   x1 (- (sdl:height surface) (imagpart y1))
		   :surface surface
		   :color (color-imagpart function)))

(defmethod draw-line (x0 (y0 complex) x1 (y1 real) function
		     &optional (surface (render function)))
  (sdl:draw-line-* x0 (- (sdl:height surface) (realpart y0))
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) (imagpart y0))
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (color-imagpart function)))

(defmethod draw-line (x0 (y0 real) x1 (y1 complex) function
		     &optional (surface (render function)))
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) (realpart y1))
		   :surface surface
		   :color (color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) (imagpart y1))
		   :surface surface
		   :color (color-imagpart function)))

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
RGB-PLIST should be a property list like (:r x :g x :b x),
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

(defun identify-input-token (input)
  (etypecase input
    (null nil)
    (symbol (cond ((fboundp input) 'function)
		  ((boundp input)
		   (identify-input-token (symbol-value input)))
		  (t (format t "~a is an invalid token!~%" input))))
    (function 'function)
    (list (cond ((keywordp (cadr input)) 'function)
		((and (identify-input-token (car input))
		      (null (cdr input)))
		 'function)
		((and (car input) (cdr input))
		 'master-function)))))
    

(defun plottable-count (func-list)
  (labels ((rec-plot-len (flist sum)
	     (typecase (car flist)
	       (null sum)
	       (list
		(if (keywordp (cadar flist))
		    (rec-plot-len (cdr flist) (1+ sum))
		    (rec-plot-len
		     (cdr flist)
		     (rec-plot-len (cdar flist) sum))))
	       (symbol
		(if (fboundp (car flist))
		    (rec-plot-len (cdr flist) (1+ sum))
		    (rec-plot-len (cdr flist)
				  (rec-plot-len
				   (list (symbol-value (car flist)))
				   sum))))
	       (function (rec-plot-len (cdr flist) (1+ sum))))))
    (rec-plot-len func-list 0)))

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



'(defun OBSOLETE-plotcall (function index &rest arguments
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
      (setf (data-max function)
	    (if (data-max function)
		(complex-max (data-max function)
			     value)
		(complex-max value)))
      (setf (data-min function)
	    (if (data-min function)
		(complex-min (data-min function)
			     value)
		(complex-min value))))
    
    (setf ;;;setfing an applied aref is used as example in the hyperspec!
     (apply #'aref (data function) lindex)
     value)))
  
'(defun OBSOLETE-plotfunc-evaluate (plotfunc index &rest arguments)
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
			 (color-real pfunc))
			(sdl:color
			 pfunc))))
    ;;the components of a complex must be real:
    (complex (draw-value x-coord
			 (realpart value)
			 y-scale slack screen-y0 surface
			 (color-realpart pfunc))
	     (draw-value x-coord
			 (imagpart value)
			 y-scale slack screen-y0 surface
			 (color-imagpart pfunc)))
    (t (draw-vertical x-coord ; bad value, most likely zero div
		      *bad-color*
		      surface)))
  NIL)

(defun render-func-list (func-list surface)
  "Blits all funcdata-renders within FUNC-LIST tree onto SURFACE."
  (dolist (func func-list)
    (typecase func
      (master
       (render-func-list (subs func) surface))
      (drawn
       (sdl:blit-surface (render func) surface))))
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
     do (plotcall function i x)))

(defun compute-2d-tree (state)
  "Computes data for all funcdatas in FUNC-LIST."
  (mapcar #'(lambda (func)
	      (compute-2d-data func state))
	  (pfunc-list state))
  (check-y-extremes state))

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

(defun render-2d-dots (function state
		       &optional (surface (render function)))
  (loop for x from 0 below (sdl:width surface)
     for y across (data function)
     do (draw-value x y (y-scale state) (slack-pixels state) (screen-y0 state)
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

(defun render-2d-lineplot (function state
			   &optional (surface (render function)))
  (let ((x-pixel 0))
    (map
     NIL
     #'(lambda (from to)
	 (draw-line x-pixel
		    (scale-y from (y-scale state)
			     (slack-pixels state) (screen-y0 state))
		    (incf x-pixel)
		    (scale-y to (y-scale state)
			     (slack-pixels state) (screen-y0 state))
		    function
		    surface
		    ))
     (data function)
     (subseq (data function) 1))))
  
(defun render-2d-data (function state)
  "Renders individual funcdata FUNCTION onto SURFACE, stored into
FUNCTION's render slot."
  ;;(declare (funcdata function))
  (format t "Rendering ~a~%" (label function))
  (if (render function)
      ;; Wipe render:
      (sdl:clear-display *transparent* :surface (render function))
      ;; First time rendering -> create surface:
      (setf (render function)
	    (sdl:create-surface (width state) (height state) :pixel-alpha 255)))

  (when (and (draw-labels state)
	     (< (label-position state) (length (data function))))
    (let ((string-render
	   (render-string
	    (label function)
	    (color-real function))))
      ;; Should probably write some smarter position determination...
      ;; update: this is getting ugly
      (sdl:draw-surface-at-* string-render
			     (label-position state)
			     (+
			      ;; move label downwards:
			      (sdl:char-height sdl:*default-font*)
			      (- (sdl:height (render function))
				 (round
				  (realpart
				   (- (+ (slack-pixels state)
					 (handler-case
					     (* (y-scale state)
						(aref (data function)
						      (label-position state)))
					; if trying to label zerodiv:
					   (type-error () 0)))
				      (screen-y0 state))))))
			     :surface (render function))
      (sdl:free string-render))
    
    (incf (label-position state)
	  (* (sdl:char-width sdl:*default-font*)
	     (length (label function)))))

  (funcall *render-function* function state (render function)))

(defun render-2d-tree (state func-list)
  "Will (re)draw all funcdatas in FUNC-LIST ot their sub-funcdatas
that are of class drawn."
  (dolist (func func-list)
    (etypecase func
      (master (render-2d-tree state (subs func)))
      (drawn (render-2d-data func state)))))

;; Placeholder memory manager
(defun free-assets (pfunc-list)
  (let ((func (car pfunc-list)))
    (typecase func
      (null (return-from free-assets))
      (master (free-assets (subs func)))
      (drawn
       (sdl:free (color-real func))
       (sdl:free (color-realpart func))
       (sdl:free (color-imagpart func))
       (sdl:free (render func))
       (setf (slot-value func 'color-real) nil
	     (slot-value func 'color-realpart) nil
	     (slot-value func 'color-imagpart) nil
	     (render func) nil)))
    (free-assets (cdr pfunc-list))))

(defun free-renders (pfunc-list)
  (let ((func (car pfunc-list)))
    (typecase func
      (null (return-from free-renders))
      (plotfunc (free-renders (plotfunc-subs func)))
      (funcdata
       (sdl:free (render func))
       (setf (render func) nil)))
    (free-renders (cdr pfunc-list))))

(defun read-input-list (input-func-list dataset-width)
  "Read a function description, store processed pfunc-list to *draw-functions*
and return it."
  (setf
   *draw-functions*
   (generate-function-containers input-func-list dataset-width)
   ;(to-plotfunc (to-funcdata input-func-list dataset-width))
   ))

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

;; By wrapping a function in parens it should be possible to give arguments
;;to func container init
;; f.ex:
;; (plot (list '((log :data-res 2) identity -)))

;; Used in determining what to update with button bindings:
(defun find-containers (function containers)
  "Returns list of funcdatas from list CONTAINERS,
whose function is eql to FUNCTION."
  (let ((found nil))
    (dolist (container containers)
      (setf found
	    (nconc
	     found
	     (typecase container
	       (master (if (eql (funcdata-function container)
				function)
			   (list container)
			   (find-containers function (subs container))))
	       (drawn (if (eql (funcdata-function container)
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
	    (inc-button dec-button dyn-var delta &optional func-list)
	  binding
	
	(unless (boundp dyn-var)
	  (format t "Symbol ~a has not been dynamically bound!" dyn-var))
	
	(let ((to-update
	       (or ;; failsafe, in case no matches -> update everything
		;; would be smarter to only update anonymous funcs
		;;;; In general the to-update func-list should only be provided
		;;in case of extremely heavy functions and even then only if
		;;there are several of them and not all need to be updated
		;;at the same time.
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
  "Funcalls binding on BUTTON.
Returns NIL when no binding found.
Returns T when binding found and STATE changed."
  (let ((binding (gethash button bindings-table)))
    (unless binding
      (format t "No bindings on ~a~%" button)
      (return-from call-binding nil))
    
    (funcall (binding-action binding))
    (dolist (to-update (binding-functions binding))
      (setf (data-min to-update) NIL ;must be "unset" so extremes set correctly
	    (data-max to-update) NIL)
      (compute-2d-data to-update state))
    ;; check-y-extremes will take care of redrawing if extremes change:
    (unless (check-y-extremes state)
      ;; if extremes did not change only redraw what's on the menu:
      (render-2d-tree state (binding-functions binding)))
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
	    (draw-function
	     processed-func-list
	     from to slack))

      (setf binding-hash-table (make-binding-hash-table bindings state))
      ;; produce renders for all drawn funcs:
      (render-2d-tree state (pfunc-list state))
      ;; render to main surface:
      (render-state state)

      (sdl:update-display)

      (sdl:with-events (:poll)
	(:quit-event
	 ()
	 (free-assets *draw-functions*)
	 t)

	(:key-down-event
	 (:key key)
	 (setf (label-position state) 0)
	 (format t "Pressed: ~a~%" key)

	 (when (call-binding key binding-hash-table state)

	   (check-y-extremes state)

	   (format t "STATE: max-y ~a, min-y ~a~%" (max-y state) (min-y state))

	   (render-state state)
	   (sdl:update-display)
	   ))
	
	(:idle
	 ()
	 )))))
