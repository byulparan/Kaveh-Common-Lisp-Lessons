
;;; view class that draws a square
(defclass my-opengl-view (ns:opengl-view)
  ())

;;; draw a square outline in OpenGL
(defun draw-square ()
  (gl:color 1.0 1.0 1.0)
  (gl:line-width 3.0)
  (gl:begin :line-loop)
  (gl:vertex  0.5  0.5 0.0)
  (gl:vertex  0.5 -0.5 0.0)
  (gl:vertex -0.5 -0.5 0.0)
  (gl:vertex -0.5  0.5 0.0)
  (gl:end))

;;; display the view
(defmethod ns:draw ((view my-opengl-view))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)
  (draw-square))

(defmethod ns:mouse-down ((view my-opengl-view) event loc-x loc-y)
  (declare (ignore event loc-x loc-y))
  (ns:redisplay view))

;;; create and display a window containing an OpeGL view
(defun show-window ()
  (let* ((w (make-instance 'ns:window :x 0 :y 0 :w 512 :h 512))
	 (v (make-instance 'my-opengl-view)))
    (setf (ns:content-view w) v)
    (ns:window-show w)))


(defun run ()
  (ns:with-event-loop nil
    (show-window)))

(run)

