(defun draw-pixel (x y surface color)
  (sdl:draw-pixel-* x (- (sdl:height surface) y)
		    :surface surface
		    :color color))

(defun draw-vertical (x color surface &key (mark nil))
  
  (unless (< -1 x (sdl:width surface))
    (return-from draw-vertical))
  
  (sdl:draw-line-* x 0
		   x (sdl:height surface)
		   :surface surface
		   :color color)
  (typecase mark
    (string (sdl:draw-string-solid-* mark (+ x 2) (- (sdl:height surface) 8)
				     :surface surface
				     :color color))))

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
