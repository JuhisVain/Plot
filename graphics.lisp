(defun draw-pixel (x y surface color)
  (sdl:draw-pixel-* x (- (sdl:height surface) y)
		    :surface surface
		    :color color))

(defun format-mark (value)
  (declare (real value))
  (format nil "~[~a~:;~G~]"
	  (if (integerp value) 0 1)
	  value))

(defun draw-string (string x y surface
		    &key
		      (rotation 0)
		      (font lispbuilder-sdl:*default-font*)
		      (color *grid-color*))
  "Renders STRING to internal surface which will be rotated ROTATION degrees
and blitted to STATE's surface."
  (declare (string string)
	   (fixnum x y rotation))
  (let ((string-render (sdl:rotate-surface
			rotation
			:free t
			:smooth nil
			:surface
			(sdl:draw-string-solid-*
			 string 0 0 :surface (sdl:create-surface
					      (* (length string)
						 (sdl:char-width font))
					      (sdl:char-height font)
					      :type :hw)
				    :font font :color color))))
    (sdl:draw-surface-at-*
     string-render
     x (- (sdl:height surface) y)
     :surface surface)
    (sdl:free string-render)))

(defun draw-vertical (x color surface &key (mark nil))
  
  (unless (< -1 x (sdl:width surface))
    (return-from draw-vertical))
  
  (sdl:draw-line-* x 0
		   x (sdl:height surface)
		   :surface surface
		   :color color)
  (typecase mark
    (string (draw-string mark (+ x 2) 8 surface :color color))))

(defun draw-free-line (x0 y0 x1 y1 color surface)
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color color))

(defgeneric draw-line (x0 y0 x1 y1 function surface))

(defmethod draw-line (x0 (y0 (eql 'zero-division))
		      x1 y1
		      function surface)
  (draw-vertical x0 *bad-color* surface))

;;; Drawing *bad-color* line for last value when 'zero-division or nil should
;; have been taken care of by increasing array size by one in funcdata init.
;; Could be investigated further but this doesn't seem too important.
(defmethod draw-line (x0 y0
		      x1 (y1 (eql 'zero-division))
		      function
		      surface)
  ;; don't do nuthin
  NIL)

(defmethod draw-line (x0 (y0 (eql nil))
		      x1 y1
		      function surface)
  (draw-vertical x0 *bad-color* surface))

(defmethod draw-line (x0 y0
		      x1 (y1 (eql nil))
		      function surface)
  ;; don't do nuthin
  NIL)

(defmethod draw-line (x0 (y0 real)
		      x1 (y1 real)
		      function surface)
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (color-real function)))

(defmethod draw-line (x0 (y0 complex)
		      x1 (y1 complex)
		      function surface)
  (sdl:draw-line-* x0 (- (sdl:height surface) (realpart y0))
		   x1 (- (sdl:height surface) (realpart y1))
		   :surface surface
		   :color (color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) (imagpart y0))
		   x1 (- (sdl:height surface) (imagpart y1))
		   :surface surface
		   :color (color-imagpart function)))

(defmethod draw-line (x0 (y0 complex)
		      x1 (y1 real)
		      function surface)
  (sdl:draw-line-* x0 (- (sdl:height surface) (realpart y0))
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) (imagpart y0))
		   x1 (- (sdl:height surface) y1)
		   :surface surface
		   :color (color-imagpart function)))

(defmethod draw-line (x0 (y0 real)
		      x1 (y1 complex)
		      function surface)
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) (realpart y1))
		   :surface surface
		   :color (color-realpart function))
  (sdl:draw-line-* x0 (- (sdl:height surface) y0)
		   x1 (- (sdl:height surface) (imagpart y1))
		   :surface surface
		   :color (color-imagpart function)))

(defun draw-circle (x y radius surface color)
  (sdl:draw-circle-* x (- (sdl:height surface) y) radius
		     :surface surface
		     :color color))
