(defsystem "plot"
    :description "A function plotter"
    :author "Juho Vainio"
    :license "MIT"
    :depends-on ("lispbuilder-sdl")
    :serial t
    :components ((:file "package")
		 (:file "setup")
		 (:file "funcdata")
		 (:file "graphics")
		 (:file "colors")
		 (:file "state")
		 (:file "2d")
		 (:file "wireframe")
		 (:file "plot")))
