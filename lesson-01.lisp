


;;; view class that draws a scene
(defclass scene-view (ns:opengl-view)
  ((scene :accessor scene :initarg :scene :initform nil)))

;;; display the view
(defmethod ns:draw ((self scene-view))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)
  (when (scene self)
    (draw (scene self))))


(defmethod ns:mouse-down ((view scene-view) event locx locy)
  (declare (ignore event locx locy))
  (ns:redisplay view))

;;; create and display a window containing an OpenGL view to display a scene
(defun show-window (scene)
  (let* ((w (make-instance 'ns:window
              :title "Kaveh's Common Lisp Lessons"
	      :x 0 :y 0 :w 512 :h 512))
	 (v (make-instance 'scene-view :scene scene)))
    (setf (ns:content-view w) v)
    (ns:window-show w)
    w))

;;;; shape =====================================================================

(defclass shape ()
  ())

(defmethod draw ((self shape))
  ;; subclass responsibility
  )

;;;; scene =====================================================================

;;; scene -- stores a list of shapes and draws them
(defclass scene ()
  ((shapes :accessor shapes :initarg :shapes :initform '())))

(defmethod add-shape ((self scene) (s shape))
  (push s (shapes self))
  s)

(defmethod clear-shapes ((self scene))
  (setf (shapes self) '()))

(defmethod draw ((self scene))
  (dolist (s (shapes self))
    (draw s)))

;;;; run graphics ==============================================================

;;; run function
(defparameter *scene* (make-instance 'scene))
(defparameter *window* nil)

(defun run ()
  (setf *window* (ns:with-event-loop (:waitp t) (show-window *scene*))))

(defun redraw ()
  (ns:with-event-loop nil
    (ns:redisplay (ns:content-view *window*))))

#| >>> create new window
(run)
|#

;;;; utils =====================================================================

;;; random float between a and b
(defun rand2 (a b)
  (if (= a b)				;doesn't like (random 0)
      a
      (let ((lo (min a b))
	    (hi (max a b))
            (*random-state* (make-random-state t)))
	(+ lo (random (coerce (- hi lo) 'float))))))

;;; random float between -a and a
(defun rand1 (a)
  (rand2 (- a) a))

;;; concatenate strings
(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

;;; concatenate symbols
(defun symcat (&rest syms)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

;;;; points ====================================================================

;;; class for representing 2D points
(defclass point ()
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)))

;;; >>> setf with coerce
(defmethod (setf x) (val (self point))
  (setf (slot-value self 'x) (coerce val 'single-float)))

(defmethod (setf y) (val (self point))
  (setf (slot-value self 'y) (coerce val 'single-float)))

(defun p! (x y)
  (make-instance 'point :x (coerce x 'single-float)
			:y (coerce y 'single-float)))

(defmethod p+ ((p1 point) (p2 point))
  (p! (+ (x p1) (x p2))
      (+ (y p1) (y p2))))

#|
>>> printing objects
(defparameter p (p! 2 4))
p
(type-of p)
(class-of p)
(describe p)

;;; print variation 1 -- too simple -- class not indicated
(defmethod print-object ((self point) stream)
  (format stream "[~a, ~a]" (x self) (y self)))

;;; print variation 2 -- too busy
(defmethod print-object ((self point) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "[~a, ~a]" (x self) (y self))))

;;; print variation 3 -- just right
(defmethod print-object ((self point) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "[~a, ~a]" (x self) (y self))))
|#

;;; polygonal shape class ======================================================

;;; this shape is defined by a list of points (vertices)
(defclass polygon-shape (shape)
  ((is-closed-shape? :accessor is-closed-shape? :initarg :is-closed-shape? :initform t)
   (points :accessor points :initarg :points :initform '())))

(defmethod add-point ((self polygon-shape) (p point))
  (push p (points self)))

(defmethod draw ((self polygon-shape))
  (gl:color 1.0 1.0 1.0)
  (gl:line-width 3.0)
  (if (is-closed-shape? self)
      (gl:begin :line-loop)
      (gl:begin :line-strip))
  (dolist (p (points self))
    (gl:vertex (x p) (y p) 0.0))
  (gl:end))

;;; square
(defun make-square-shape (length)
  (let ((v (/ length 2.0)))
    (make-instance 'polygon-shape
		   :points (list (p!    v     v )
				 (p!    v  (- v))
				 (p! (- v) (- v))
				 (p! (- v)    v )))))

#|
>>> create a square
(defparameter *sq* (make-square-shape 1.0))

>>> add square to scene
(progn
  (add-shape *scene* *sq*)
  (redraw))

>>> inspect square -- note point print method being used
(inspect *sq*)

>>> make shape open
(progn
  (setf (is-closed-shape? *sq*) nil)
  (redraw))

>>> add point
(add-point *sq* (p! 0 0))
|#

;;; >>> macro
;;; we notice a pattern in our tests: do some stuff then redraw window

;;; try 1 - naive implementation - a value returned by body would be lost
(defmacro with-redraw (&body body)
  `(progn ,@body
	  (redraw)))

;;; try 2 - if the body returns a value, we want the macro to return it
(defmacro with-redraw (&body body)
  `(let ((result (progn ,@body)))
     (redraw)
     result))

#|
>>> make shape closed
(with-redraw
  (setf (is-closed-shape? *sq*) t))

>>> see generated code
(pprint (macroexpand-1 '(with-redraw
                         (setf (is-closed-shape? *sq*) t))))

>>> clear scene
(with-redraw
  (clear-shapes *scene*))
|#

#|
>>> operating on lists -- mapcar
(print (mapcar (lambda (x) (* x 2))
               '(1 2 3 4 5)))

(print (mapcar #'oddp '(1 2 3 4 5)))
|#

;;; randomize shape points
(defmethod randomize-points ((self polygon-shape) (delta point))
  (setf (points self)
	(mapcar (lambda (p)
		    (let ((offset (p! (rand1 (x delta)) (rand1 (y delta)))))
		      (p+ p offset)))
		(points self))))

#|
>>> add square to scene
(with-redraw
  (add-shape *scene* *sq*))

>>> randomize square points
(with-redraw
  (randomize-points *sq* (p! 0.1 0.1)))

>>> make squares from a list of sizes
(with-redraw
  (clear-shapes *scene*)
  (dolist (size '(.25 .5 .75 1.0))
    (add-shape *scene* (make-square-shape size))))

>>> randomize all scene points
(with-redraw
  (dolist (shape (shapes *scene*))
    (randomize-points shape (p! 0.05 0.05))))
|#
