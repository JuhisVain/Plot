(defun gaussian (x height center width)
  (expt (* height
	   (exp 1.d0)) ; aka. "e"
	(- (/ (expt (- x center)
		    2)
	      (* 2 (expt width 2))))))

(defun gauss (x)
  (gaussian x 1 0 150))

(defun test-plottable-count ()
  (flet ((test-count (should-return input-list)
	   (let ((test-returns (plottable-count input-list)))
	     (or (= should-return test-returns)
		 (format t "Error on ~a = ~a != ~a~%"
			 input-list test-returns should-return)))))
    (test-count 1 '(+))
    (test-count 1 '((+)))
    (test-count 1 (list '+))
    (test-count 1 (list #'+))
    (test-count 1 (list (list #'+)))

    (test-count 2 '(+ +))
    (test-count 2 (list '(+) #'+))

    (test-count 5 (list (list #'+ :foo 1 :bar 2) '+
			'((+ :test 1 :tset 2) (+ - log (sqrt)))))))

(defun test-data-res ()
  (let ((cen 0.15))
    (declare (special cen))
    (flet ((testfun (x)
	     (gaussian x 1 cen 0.001)))
      ;; plot "1" should show a spike:
      (plot (list (list #'testfun :data-per-pixel 5)
		  #'(lambda (x) (+ 0.01 (testfun x))))
	    :from -5 :to 5
	    :bindings '((q a cen 0.005 nil))))))

(defun test-data-res-ratio ()
  (let ((cen 0.15))
    (declare (special cen))
    (flet ((testfun (x)
	     (gaussian x 1 cen 0.001)))
      ;; I don't know if this works but it sure does something
      (plot (list (list #'testfun :data-per-pixel 1/2)
		  #'(lambda (x) (+ 0.01 (testfun x))))
	    :from -5 :to 5
	    :bindings '((q a cen 0.005 nil))))))

(defun test-data-res-ratio2 ()
  (plot (list
	 (list #'gauss :data-per-pixel 1/300) ; 1
	 (list #'gauss :data-per-pixel 1/100) ; 2
	 (list #'gauss :data-per-pixel 1/75)  ; 3
	 (list #'gauss :data-per-pixel 1/50)  ; 4
	 (list #'gauss :data-per-pixel 1/25)  ; 5
	 (list #'gauss :data-per-pixel 1/10)  ; 6
	 (list #'gauss :data-per-pixel 1/5)   ; 7
	 (list #'gauss :data-per-pixel 1)     ; 8
	 (list #'gauss :data-per-pixel 2)     ; 9
	 (list #'gauss :data-per-pixel 5)     ; 10
	 (list #'gauss :data-per-pixel 10)    ; 11
	 )
	:from 0 :to 600
	:window-width 600
	))

(defun testbind ()
  (defparameter *wave-length* 1)
  (flet ((modsin (x)
	   (sin (/ x *wave-length*))))
    (plot (list #'modsin)
	  :from 0 :to 50
	  :window-width 1500
	  :bindings '((q a *wave-length* 0.1 (#'modsin))))))
;;                                       #'(lambda () (* *wave-length* 2))

(defun testbind2 ()
  (defparameter *ding-dong* 1)
  (defparameter *dd-mod* 0.1)
  (plot (list #'(lambda (x)
		  (* x *ding-dong*)))
	:bindings
	`((q a *ding-dong*
	     ,#'(lambda ()
		  (* *dd-mod* *ding-dong*))
	     nil)
	  (w s *dd-mod* 0.01 nil))))

(defun testbind3 ()
  (defparameter *h* 1)
  (defparameter *c* 0)
  (defparameter *w* 1)
  (defparameter *mod* 0.01)
  (plot (list #'(lambda (x)
		  (gaussian x *h* *c* *w*))
	      #'(lambda (x)
		  (/ (1+ (cos x)) 2)))
	:bindings `((q a *h* ,#'(lambda () *mod*) nil)
		    (w s *c* ,#'(lambda () *mod*) nil)
		    (e d *w* ,#'(lambda () *mod*) nil)
		    (r f *mod*
		       ,#'(lambda ()
			    (format t "mod now ~a~%" *mod*)
			    0.01)
		       nil))
	:to 4))

(defun testbindclosure ()
  (let ((h 1)
	(c 0)
	(w 1)
	(cmod 0.1))
    (declare (special h c w cmod)) ; 'mod' can't be declared special
    (plot (list #'(lambda (x)
		    (gaussian x h c w))
		#'(lambda (x)
		    (/ (1+ (cos x)) 2))
		#'(lambda (x)
		    (declare (ignore x))
		    h))
	  :bindings `((q a h ,#'(lambda () cmod)) ;; are these even closures?
		      (w s c ,#'(lambda () cmod))
		      (e d w ,#'(lambda () cmod))
		      (r f cmod
			 ,#'(lambda ()
			      (format t "mod now ~a~%" cmod)
			      0.01)))
	  :to 4)))

(defun named-fun (x)
  (gaussian x *h* *c* *w*))

(defun testbindnamedfun ()
  (let ((*h* 1)
	(*c* 0)
	(*w* 1))
    (declare (special *h* *c* *w*))
    (plot (list #'named-fun)
	  :bindings `((q a *h* ,#'(lambda () 0.1) ,(list #'named-fun))
		      (w s *c* ,#'(lambda () 0.1) ,(list 'named-fun))
		      (e d *w* ,#'(lambda () 0.1) ,(list #'named-fun)))
	  :to 4)))

(defun testlabel ()
  "Label background should be transparent"
  (plot (list (lambda (x) (+ x 0.01))
	      (lambda (x) (+ (- x) 0.011))
	      (lambda (x) (+ (- x) 0.0095))
	      (lambda (x) (+ x 0.0105)))
	:from 0 :to 0.01))

(defun testbunch ()
  ;;Strange behaviour on sbcl when trying to collect lambdas directly
  ;;; UPDATE: The closure is on i, which is set to be 550 CASE CLOSED
  ;;(loop for i from 50 to 500 by 50
  ;;	collect #'(lambda (x) (gaussian x 1 0 i)))
  (plot (mapcar #'(lambda (ele)
		    #'(lambda (x) (gaussian x 1 0 ele)))
		(loop for i from 50 to 500 by 50
		   collect i)) :from 0 :to 1000))

;; Previous is closures, this one is functions
(defun testbunch2 ()
  (plot #.`(list
	    ,@(loop for i from 50 to 500 by 50
		 collect `#'(lambda (x) (gaussian x 1 0 ,i))))
	:from 0 :to 1000))

(defun test-selective-redraw ()
  (let ((*modifier* 1000)
	(*shifter* 0.0))
    (declare (special *modifier* *shifter*))
    (flet ((modfun (x)
	     (+ (/ x *modifier*)
		*shifter*)))
      (plot #.`(list
		,@(loop for i from 50 to 500 by 5
		     collect `#'(lambda (x) (gaussian x 1 0 ,i)))
		#'modfun)
	    :bindings `((q a *modifier* 10 ,(list #'modfun))
			(w s *shifter* 0.1 ,(list #'modfun)))
	    :from 0 :to 1000))))

;; Uses a lot of memory:
(defun test-memory ()
  (plot (mapcar #'(lambda (ele)
		    #'(lambda (x) (gaussian x 1 0 ele)))
		(loop for i from 50 to 500 by 1
		   collect i)) :from 0 :to 1000
		   :draw-labels t))

(defun testfun (x)
  (if (<= x 400)
      (+ 0.5 (/ (cos (/ x 127.324)) 2))
      0))

(defun testdoublefun (x)
  (if (<= x 400)
      (+ 0.5 (/ (cos (/ x 127.324d0)) 2))
      0))

(defun test-testfuns ()
  (plot (list (list #'testfun
		    #'(lambda (y) ;red
			(+ y 0.00000001d0))) ;shift up
	      (list #'testdoublefun
		    #'identity ;green
		    #'(lambda (x) ;blue
			(coerce x 'single-float))))
	:from 0 :to 0.3))

(defun test-testfuns-datares ()
  (plot (list (list (list #'testfun :data-per-pixel 5)
		    #'(lambda (y) ;red
			(+ y 0.00000001d0))) ;shift up
	      (list (list #'testdoublefun :data-per-pixel 2)
		    #'identity ;green
		    #'(lambda (x) ;blue
			(coerce x 'single-float))))
	:from 0 :to 0.3))
