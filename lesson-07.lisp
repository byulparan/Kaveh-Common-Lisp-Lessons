#|
Lesson 07 -- Colors
|#


;;;; methods belong to generic functions, not classes
#|
(defmethod p* ((p1 point) (p2 point))
  (p! (* (x p1) (x p2)) (* (y p1) (y p2))))

(defmethod p* ((p1 point) (s number))
  (p! (* (x p1) s) (* (y p1) s)))

(defgeneric p* (obj1 obj2)
  (:method ((p1 point) (p2 point))
    (p! (* (x p1) (x p2)) (* (y p1) (y p2))))
  (:method ((p1 point) (s number))
    (p! (* (x p1) s) (* (y p1) s))))
|#

;;;; color =====================================================================

;;; we define colors as simple vectors (arrays)
(defun c! (r g b &optional (a 1.0))
  (vector (coerce r 'single-float)
	  (coerce g 'single-float)
	  (coerce b 'single-float)
	  (coerce a 'single-float)))

(defun c-red   (c) (aref c 0))
(defun c-green (c) (aref c 1))
(defun c-blue  (c) (aref c 2))
(defun c-alpha (c) (aref c 3))

(defun c-set! (c1 c2 &key (alpha t))
  (setf (aref c1 0) (aref c2 0))
  (setf (aref c1 1) (aref c2 1))
  (setf (aref c1 2) (aref c2 2))
  (when alpha
    (setf (aref c1 3) (aref c2 3))))

(defun c-lerp (f c1 c2)
  (map 'vector (lambda (a b) (lerp f a b)) c1 c2))

(defun c-rand ()
  (c! (rand2 0.0 1.0) (rand2 0.0 1.0) (rand2 0.0 1.0)))

(defun c-rand-with-alpha ()
  (c! (rand2 0.0 1.0) (rand2 0.0 1.0) (rand2 0.0 1.0) (rand2 0.0 1.0)))

(defun c-rand1 (c)
  (map 'vector #'rand1 c))

(defun c-rand2 (c1 c2)
  (c-lerp (rand2 0 1) c1 c2))

(defun c+ (c1 c2)
  (map 'vector #'+ c1 c2))

(defun c-jitter (c c-delta)
  (c+ c (c-rand1 c-delta)))

;;;; scene =============================================================

;;; add background color to scene
(defclass scene ()
  ((shapes :accessor shapes :initarg :shapes :initform '())
   (bg-color :accessor bg-color :initarg :bg-color :initform (c! 0 0 0 0))))

;;;; device ============================================================

;;; enhance view draw method to support alpha blending
(defmethod ns:draw ((self scene-view))
  ;; enable alpha blending
  (gl:enable :blend)
  (gl:blend-equation-separate :func-add :func-add)
  (gl:blend-func-separate :src-alpha :one-minus-src-alpha :one :one-minus-src-alpha)
  ;; set background color
  (let ((bg (if (scene self)
                (bg-color (scene self))
                (c! 0 0 0 0))))
    (gl:clear-color (c-red bg) (c-green bg) (c-blue bg) (c-alpha bg)))
  (gl:clear :color-buffer-bit)
  (when (scene self)
    (draw (scene self))))

;;;; appearance ================================================================

(defclass appearance ()
  ((fill-color :accessor fill-color :initarg :fill-color :initform (c! 0 0 0 0))
   (outline-color :accessor outline-color :initarg :outline-color :initform (c! 1 1 1 1))))

;;; polygon-shape ==============================================================

;;; add appearance slot to polygon-shape class
(defclass polygon-shape (shape)
  ((is-closed-shape? :accessor is-closed-shape? :initarg :is-closed-shape? :initform t)
   (points :accessor points :initarg :points :initform '())
   (appearance :accessor appearance :initarg :appearance :initform (make-instance 'appearance))))

(defmethod set-fill-color ((self polygon-shape) color &key (alpha t))
  (c-set! (fill-color (appearance self)) color :alpha alpha))

(defmethod set-outline-color ((self polygon-shape) color &key (alpha t))
  (c-set! (outline-color (appearance self)) color :alpha alpha))

(defmethod draw ((self polygon-shape))
  ;; draw filled shape
  (let ((col (fill-color (appearance self))))
    (when (> (c-alpha col) 0)           ;don't draw if alpha is 0
      (gl:color (aref col 0) (aref col 1) (aref col 2) (aref col 3))
      (gl:begin :polygon)
      (dolist (p (points self))
	(gl:vertex (x p) (y p) 0.0))
      (gl:End)))
  ;; draw outline
  (let ((col (outline-color (appearance self))))
    (when (> (c-alpha col) 0)           ;don't draw if alpha is 0
      (gl:color (aref col 0) (aref col 1) (aref col 2) (aref col 3))
      (gl:line-width 3.0)
      (if (is-closed-shape? self)
	  (gl:begin :line-loop)
	  (gl:begin :line-strip))
      (dolist (p (points self))
	(gl:vertex (x p) (y p) 0.0))
      (gl:End))))

;;;; color tests ===============================================================

(with-redraw
  (setf (bg-color *scene*) (c! 0.5 0.5 0.5)))

(progn
  (defparameter *square* (make-square-shape 1.0))
  (with-clear-and-redraw
    (add-shape *scene* *square*)))

(with-redraw
  (set-outline-color *square* (c! 1 1 0)))

(with-redraw
  (set-fill-color *square* (c! .2 .2 .8)))

(defun make-hexagons (&optional (n 10))
  (apply #'make-group
	 (let ((shapes '()))
	   (dotimes (i n)
	     (let* ((factor (/ i (- n 1.0)))
		    (hex (make-hexagon-shape 1.0))
		    (scale (lerp factor 0.1 1.5)))
	       (scale-by hex scale)
	       (rotate-by hex (lerp factor 0 90))
	       (push hex shapes)))
	   shapes)))

;;; loop version -- note looping from 9 to 0
(defun make-hexagons (&optional (n 10))
  (apply #'make-group
         (loop for i from (1- n) downto 0
               for factor = (/ i (- n 1.0))
               collect (rotate-by (scale-by (make-hexagon-shape 1.0)
                                            (lerp factor 0.1 1.5))
                                  (lerp factor 0 90)))))

(progn
  (defparameter *hex-group* (make-hexagons))
  (with-clear-and-redraw
    (add-shape *scene* *hex-group*)))

;;; random colors
(with-redraw
  (do-hierarchy *hex-group*
    (lambda (s)
      (set-fill-color s (c-rand))
      (set-outline-color s (c! 0 0 0)))
    :test #'is-leaf?))

;;; shades of blue
(with-redraw
  (do-hierarchy *hex-group*
    (lambda (s) (set-fill-color s (c-rand2 (c! .2 .2 .4) (c! .5 .5 1))))
    :test #'is-leaf?))

(defun set-group-color (group c)
  (do-hierarchy group
    (lambda (s) (set-fill-color s c))
    :test #'is-leaf?))

(with-redraw
  (setf (bg-color *scene*) (c! 0 0 0))
  (set-group-color *hex-group* (c! 1 .5 .5 0.1)))

;;; generic functions for setting colors

(defgeneric set-fill-color (self color &rest args)
  (:method ((self t) color &key (alpha t))
    (declare (ignore color alpha))) ;do nothing
  
  (:method ((self appearance) color &key (alpha t))
    (c-set! (fill-color self)
            (if (functionp color)
                (funcall color)
                color)
            :alpha alpha))

  (:method ((self polygon-shape) color &key (alpha t))
    (set-fill-color (appearance self) color :alpha alpha))    

  (:method ((self group) color &key (alpha t))
    (dolist (child (children self))
      (set-fill-color child color :alpha alpha))))

(defgeneric set-outline-color (self color &rest args)

  (:method ((self t) color &key (alpha t))
    (declare (ignore color alpha))) ;do nothing
  
  (:method ((self appearance) color &key (alpha t))
    (c-set! (outline-color self)
            (if (functionp color)
                (funcall color)
                color)
            :alpha alpha))

  (:method ((self polygon-shape) color &key (alpha t))
    (set-outline-color (appearance self) color :alpha alpha))

  (:method ((self group) color &key (alpha t))
    (dolist (child (children self))
      (set-outline-color child color :alpha alpha))))

;;; test

(progn
  (defparameter *hex-group* (make-hexagons))
  (with-clear-and-redraw
    (add-shape *scene* *hex-group*)))

(with-redraw
  (set-fill-color *hex-group* (lambda () (c-rand)))
  (set-outline-color *hex-group* (c! 0 0 0)))

;;; random borders
(with-redraw
  (set-fill-color *hex-group* (lambda () (c-rand)))
  (set-outline-color *hex-group* (lambda () (c-rand))))

;;; shades of blue
(with-redraw
  (set-fill-color *hex-group* (lambda () (c-rand2 (c! .2 .2 .4) (c! .5 .5 1))))
  (set-outline-color *hex-group* (c! 0 0 0)))

(with-redraw
  (set-fill-color *hex-group* (c! 1 .5 .5 0.1)))

(with-redraw
  (set-outline-color *hex-group* (c! 1 1 1 0)))
