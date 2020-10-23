(defsystem plot
    :depends-on (lispbuilder-sdl)
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
