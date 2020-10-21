(in-package :common-lisp-user)

(ql:quickload :lispbuilder-sdl)

(with-compilation-unit (:override nil)
  (load "package.lisp")

  (load "setup.lisp")
  (load "funcdata.lisp")
  (load "graphics.lisp")
  (load "colors.lisp")
  (load "state.lisp")
  (load "2d.lisp")
  (load "wireframe.lisp")
  (load "plot.lisp"))

;;(load "test.lisp")
