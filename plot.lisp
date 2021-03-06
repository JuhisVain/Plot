(in-package :plot)

(defun get-arg-count (func)
  "Returns count of number arguments that FUNC accepts,
one of (1 2 NIL)"
  (declare (optimize (safety 3))) ; Undefined if func not safe
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

(defun mark-lines (range)
  "Returns something by gut feeling to be used as multiplier of grid lines."
  (if (zerop range)
      1
      ;; decrement rounded order of magnitude and get its value:
      (expt 10 (1- (round (log range 10))))))


(defun identify-input-token (input)
  "Reads input and partitions it into elements of propertylist
(:function foo :options (opt ...) :subs (sub ...))"
  (etypecase input
    (null nil)
    (symbol (cond ((fboundp input)
		   (list :function input))
		  ((boundp input)
		   (identify-input-token (symbol-value input)))
		  (t
		   (format t "~a is an invalid token!~%" input))))
    (function (list :function input))
    (list (cond ((keywordp (cadr input)) ; (foo :bar ...)
		 (list :function (car input)
		       :options (cdr input)))
		((and (identify-input-token (car input)) ; (foo)
		      (null (cdr input)))
		 (list :function (car input)))
		((and (car input) (cdr input)) ; (foo bar ...)
		 (append (identify-input-token (car input))
			 (list :subs (cdr input))))))))

(defun plottable-count (func-list)
  "Counts drawn functions in user input FUNC-LIST."
  (let ((sum 0))
    (labels ((plot-len (flist)
	       (dolist (func flist)
		 (let* ((prop-list (identify-input-token func))
			(subs (getf prop-list :subs)))
		   (if subs
		       (plot-len subs)
		       (incf sum))))))
      (plot-len func-list)
      sum)))

(defun function-count (pfunc-list &optional test)
  "Processes all funcdatas in PFUNC-LIST, counting when
funcalling TEST with args (function sum-so-far) returns non-nil."
  (when (null test)
    (setf test #'(lambda (x y)
		   (declare (ignore y))
		   (not (null x)))))
  (let ((sum 0))
    (labels ((rec-function-count (pfuncs)
	       (dolist (func pfuncs)
		 (when (funcall test func sum)
		   (incf sum))
		 (when (typep func 'master)
		   (rec-function-count (subs func))))))
      (rec-function-count pfunc-list)
      sum)))

(defgeneric render-funcs (state))

(defmethod render-funcs ((state 2d-state))
  (let ((drawns (drawn-list state)))
    (dolist (func drawns)
      (render-2d-label func state))
    (dolist (func drawns)
      (render-2d-lineplot func state (surface state)))))

(defmethod render-funcs ((state wireframe))
  (render-wireframe state))

(defmethod render-funcs ((state sequential-heatmap))
  (let ((drawns (drawn-list state)))
    (let ((color (sdl:color))
	  (width (the (unsigned-byte 16) (width state)))
	  (height (the (unsigned-byte 16) (height state))))
      (dotimes (x width)
	(dotimes (z height)
	  (destructuring-bind (r &optional (g 0) (b 0))
	      (mapcar #'(lambda (value) ;translate values to range 0 - 255
			  (typecase value
			    (real (* (/ (- value (min-y state))
					(y-range state))
				     255))
			    (t 0)))
		      (loop for function in drawns ; values at (x,z)
			    collecting (3d-dataref function
						   (/ x 1.0 width)
						   (/ z 1.0 height))))
	    (sdl:set-color-* color :r r :g g :b b)
	    (draw-pixel x z (surface state) color))))
      (sdl:free color))))

;; Data-per-pixel has essentially no effect on rendering speed using heatmap
(defmethod render-funcs ((state heatmap))
  (declare (optimize (speed 3)))
  (let ((drawns (drawn-list state)))
    (dolist (function drawns)
      (let ((color (sdl:color))
	    (width (the (unsigned-byte 16) (width state)))
	    (height (the (unsigned-byte 16) (height state))))
	(dotimes (x width)
	  (dotimes (z height)
	    (let ((value
		    (3d-dataref function
				(/ x 1.0 width)
				(/ z 1.0 height))))
	      (typecase value
		(real (destructuring-bind (rk r gk g bk b ak a)
			  (hue-to-rgb (* (/ (- value (min-y state))
					    (y-range state))
					 1.8
					 pi))
			(declare (ignore rk gk bk ak a))
			(sdl:set-color-* color :r r :g g :b b)))
		(t (sdl:set-color color *bad-color*))))
	    (draw-pixel x z (surface state) color)))
	(sdl:free color)))))

(defmethod reset-extremes ((funcdata drawn))
  (setf (data-min funcdata) NIL
	(data-max funcdata) NIL))

(defmethod reset-extremes ((funcdata master))
  (dolist (sub (subs funcdata))
    (reset-extremes sub)))

(defmethod compute-data :before (function state)
  (reset-extremes function))

(defmethod compute-data (function (state 2d-state))
  "Populates funcdata FUNCTION's (and FUNCTION's subs) data slot's array with
results from applying FUNCTION on values of x from MIN-X to MAX-X by X-STEP."
  (loop
     for x from (min-x state) by (/ (x-step state)
				    (data-per-pixel function))
     for i from 0 below (length (data function)) ;data array length set at init
     do (plotcall function i x)))

(defmethod compute-data (function (state 3d-state))
  (loop
     for x from (min-x state) by (/ (x-step state)
				    (data-per-pixel function))
     for i from 0 below (array-dimension (data function) 0)
     do (loop
	   for z from (min-z state) by (/ (z-step state)
					  (data-per-pixel function))
	   for j from 0 below (array-dimension (data function) 1)
	     do (plotcall function (list i j) x z))))

(defun compute-tree (state)
  "Computes data for all funcdatas in FUNC-LIST."
  (mapcar #'(lambda (func)
	      (compute-data func state))
	  (pfunc-list state))
  (check-y-extremes state))

(defun generate-function-containers (input-func-list input-dimensions)
  (let ((resolution-width (car input-dimensions))
	(resolution-height (cadr input-dimensions)))
    (labels
	((funcdata-generator (func-list id-counter &optional master)
	   (mapcar #'(lambda (func)
		       (let* ((plist (identify-input-token func))
			      (main-func (getf plist :function))
			      (options (getf plist :options))
			      (subs (getf plist :subs)))
			 
			 (let ((processed-func
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
				     :master master
				     :subs subs
				     :data-per-pixel data-per-pixel
				     :arg-count arg-count))))
			   
			   (when subs
			     (setf (subs processed-func)
				   (funcdata-generator subs 0 processed-func)))
			   
			   processed-func)))
		   func-list)))
      (funcdata-generator input-func-list 0))))

(defun read-input-list (input-func-list input-dimensions)
  "Read a function description, store processed pfunc-list to *draw-functions*
and return it."
  (setf
   *draw-functions*
   (generate-function-containers input-func-list input-dimensions)
   ;(to-plotfunc (to-funcdata input-func-list dataset-width))
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

(defgeneric state-controls (state)
  (:documentation
   "Lists state dependent controller button bindings to be added to the
bindings hash table."))

(defmethod state-controls ((state 2d-state))
  (macrolet ((key-action (key actions &optional to-update)
	       `(cons (button-to-sdlkey ',key)
		      (make-binding
		       :action #'(lambda () ,actions)
		       :functions ,to-update))))
    (list (key-action left (let ((delta (mark-lines (x-range state))))
			     (incf (max-x state) delta)
			     (incf (min-x state) delta))
		      (pfunc-list state))
	  (key-action right (let ((delta (mark-lines (x-range state))))
			      (decf (max-x state) delta)
			      (decf (min-x state) delta))
		      (pfunc-list state)))))

(defmethod state-controls ((state 3d-state))
  (macrolet ((key-action (key actions &optional to-update)
	       `(cons (button-to-sdlkey ',key)
		      (make-binding
		       :action #'(lambda () ,actions)
		       :functions ,to-update))))
    (list (key-action left (incf (yaw state) (/ +sf-pi+ 36)))
	  (key-action right (decf (yaw state) (/ +sf-pi+ 36)))
	  (key-action up (decf (pitch state) (/ +sf-pi+ 36)))
	  (key-action down (incf (pitch state) (/ +sf-pi+ 36))))))

(defun make-binding-hash-table (bindings state)
  (let* ((controls (state-controls state))
	 (hash-table
	  (make-hash-table :size (+ (* 2 (length bindings))
				    (length controls)))))
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

    (loop for (key . action) in controls
       do (setf (gethash key hash-table) action))
    
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
      (compute-data to-update state))
    t))

(defun plot (func-list
	     &key (from 0) (to 100) wire-density plot-type slack pitch yaw
	       (window-width 500) (window-height 500)
	       bindings)
  "FUNC-LIST = ({fun|(fun[fun]*)}*)
FUN = function|(function[func-options])

Subsequent functions given after an inner function list are post-processing
functions which should always take only the result of their master function,
which will not be drawn.

FUNC-OPTIONS is a &rest property-list. Valid keys at the moment are
:data-per-pixel, a positive non-zero real
values less than 1 reduce rendering time with loss of accuracy,
values greater than 1 might help spot erratic line behaviour.
:arg-count, either 1 or 2
Specifies number of arguments to be given to a master function, to be used for
functions with optional number of arguments or if automatic arg count detection
craps out.

The FROM and TO values may be given either as reals or a list of two reals.

PLOT-TYPE specifies plotting style.
Defaults are '2d-plot-with-grid and 'wireframe-with-grid
Other valid types: '2d-state for a gridless 2d plot
'heatmap 'sequential-heatmap 'wireframe for 3d plots.

WIRE-DENSITY should be a RATIONAL between 0 and 1
best results between ~ 1/50 and 1/10. Denominator+1 lines will be drawn
for both dimensions. Only has an effect on wireframe 3d plots.

SLACK adds (slack/2 * value-range) to a 2d plot grid's maximum and minimum.

PITCH and YAW set initial pitch and yaw for wireframe plots in radians.

BINDINGS = ((inc-button dec-button dynamic-var delta [function*])*)
Binds inc & dec buttons to increment and decrement dynamic-var by delta.
Delta may be either a real or a function of zero args producing a number.
Giving named function arguments (of type function) as &rest will ensure only
these functions will be recomputed.

Some states come with basic controls predefined.
2d-plots will have left and right arrow keys modify min & max x.
3d-plots can rotated with arrow keys."
  (let ((processed-func-list
	 (read-input-list func-list (list window-width window-height)))
	(binding-hash-table nil)
	(state nil))
    (sdl:initialise-default-font)
    (sdl:with-init()
      (sdl:window window-width window-height
		  :title-caption "plot"
		  :hw t
		  :bpp 32)

      (setf state
	    (make-state
	     processed-func-list
	     from to
	     (or wire-density 1/50)
	     plot-type
	     (or slack 1/20)
	     pitch yaw))

      (setf binding-hash-table (make-binding-hash-table bindings state))
      ;; produce renders for all drawn funcs:
      ;(render-tree state (pfunc-list state))
      ;; render to main surface:
      (render-state state)

      (sdl:update-display)

      (when *auto-quit*
	(sdl:push-quit-event))
      
      (sdl:with-events (:poll)
	(:quit-event
	 ()
	 ;(free-assets *draw-functions*)
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
