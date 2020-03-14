(defun gaussian (x height center width)
  (expt (* height
	   (exp 1.d0)) ; aka. "e"
	(- (/ (expt (- x center)
		    2)
	      (* 2 (expt width 2))))))

(defun gauss (x)
  (gaussian x 1 0 150))

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
