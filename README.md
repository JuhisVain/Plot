A simple program to transform lisp functions into pixels.

    (plot:plot (list #'(lambda (x) (- (expt x 3) (* 9 x)))
                     #'(lambda (x) (- (expt x 4) (expt 4 x)))
                     'log
                     'sqrt)
               :from -2 :to 3
               :window-width 750
               :window-height 500)

![screenshot](doc/2d.png "Screenshot of 2d plot")

    (plot:plot (list #'(lambda (x y)
                         (exp (- (+ (/ (expt x 2) 1)
                                    (/ (expt y 2) 1))))))
               :from '(-3 -3) :to '(3 3)
               :wire-density 1/30
               :window-width 550)

![screenshot](doc/wireframe.png "Screenshot of wireframe plot")

    (plot:plot (list #'(lambda (x y)
                         (* (sin (expt x 2))
                            (cos (expt y 2)))))
               :from -4 :to 4
               :plot-type 'heatmap)

![screenshot](doc/heatmap.png "Screenshot of heatmap plot")