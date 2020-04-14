(defun gaussian (x height center width)
  (expt (* height
	   (exp 1.d0)) ; aka. "e"
	(- (/ (expt (- x center)
		    2)
	      (* 2 (expt width 2))))))

(defun gauss (x)
  (gaussian x 1 0 150))

(defun testbind ()
  (defparameter *wave-length* 1)
  (flet ((modsin (x)
	   (sin (/ x *wave-length*))))
    (plot (list #'modsin)
	  :from 0 :to 50
	  :window-width 1500
	  :bindings '((q a *wave-length* 0.1 (modsin))))))
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
