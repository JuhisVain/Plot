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

(load "funcdata.lisp")
(load "graphics.lisp")
(load "colors.lisp")
(load "state.lisp")
(load "2d.lisp")
(load "wireframe.lisp")
(load "plot.lisp")

;;(load "test.lisp")
