(ql:quickload :lispbuilder-sdl)

(defvar *auto-quit* nil)
(defvar *draw-labels* t
  "The default state of whether or not to write names for plotted data.")

;; There's also 'render-2d-dots:
(defparameter *render-function* 'render-2d-lineplot
  "Funcallable symbol or function, used to generate funcdata-renders.")

(defvar *bad-color* (sdl:color :r 100 :g 0 :b 0))
(defvar *grid-color* (sdl:color :r 50 :g 50 :b 50))
(defvar *grid-origin-color* (sdl:color :r 150 :g 150 :b 150))

(defconstant +sf-pi+ (float pi 1.0)) ;; PI in single float precision

(defvar *transparent* (sdl:color :r 0 :g 0 :b 0 :a 0)) ; alpha 0 is transparent

(defparameter *draw-functions* nil
  "Storage for funcdatas currently being drawn.")

(load "graphics.lisp")
(load "colors.lisp")
(load "funcdata.lisp")
(load "state.lisp")
(load "2d.lisp")
(load "wireframe.lisp")

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
    (dolist (function drawns)
      (multiple-value-bind
	    (red green blue)
	  (sdl:color-* (color-real function))
	(let ((color (sdl:color :a 0)))
	  (dotimes (x (array-dimension (data function) 0))
	    (dotimes (z (array-dimension (data function) 1))

	      ;; Handle zero div:
	      (if (realp (aref (data function) x z))
		  (let ((value (/ (- (aref (data function) x z) (min-y state))
				  (- (max-y state) (min-y state)))))
		    
		    (sdl:set-color-* color
				     :r red
				     :g green
				     :b blue
				     :a (* 255 value)))
		  ;;if not real:
		  (sdl:set-color color *bad-color*))
	      
	      
	      (draw-pixel x z (surface state) color)))
	  (sdl:free color))))))

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
	    (let* ((value
		     (3d-dataref function
				 (/ x 1.0 width)
				 (/ z 1.0 height)))
		   (spread-value
		     (/ (- value (min-y state)) ;; TODO: ZERODIV ZERODIV ZERODIV
			(y-range state))))
	      (destructuring-bind (rk r gk g bk b ak a)
		  (hue-to-rgb (* spread-value 1.8 pi))
		(declare (ignore rk gk bk ak a))
		(sdl:set-color-* color :r r :g g :b b)
		(draw-pixel x z (surface state) color)))))	
	(sdl:free color)))))

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

(defun read-input-list (input-func-list input-dimensions)
  "Read a function description, store processed pfunc-list to *draw-functions*
and return it."
  (setf
   *draw-functions*
   (generate-function-containers input-func-list input-dimensions)
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

(defun highest-arg-count (pfunc-list)
  (apply #'max
	 (mapcar #'(lambda (pf)
		     (max (arg-count pf)
			  (if (typep pf 'master)
			      (highest-arg-count (subs pf))
			      0)))
		 pfunc-list)))

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

(defgeneric state-controls (state)
  (:documentation
   "Lists state dependent controller button bindings to be added to the
bindings hash table."))

(defmethod state-controls ((state 2d-state))
  NIL)

(defmethod state-controls ((state 3d-state))
  (macrolet ((key-action (key &rest actions)
	       `(cons (button-to-sdlkey ',key)
		      (make-binding
		       :action #'(lambda () ,@actions)))))
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
      (setf (data-min to-update) NIL ;must be "unset" so extremes set correctly
	    (data-max to-update) NIL)
      (compute-data to-update state))
    ;; check-y-extremes will take care of redrawing if extremes change:
;    (unless (check-y-extremes state)
      ;; if extremes did not change only redraw what's on the menu:
;      (render-tree state (binding-functions binding)))
    t))

(defun plot (func-list
	     &key (from 0) (to 100) wire-density plot-type slack
	       (window-width 500) (window-height 500)
	       (draw-labels *draw-labels*)
	       bindings)
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
	     (or slack 1/20)))

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
