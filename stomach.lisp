#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-subject stomach (hud-entity unsavable)
  ((fullness :initarg :fullness :initform 1.0 :accessor fullness)
   (hunger-interval :initarg :hunger-interval :initform 1 :accessor hunger-interval)
   (previous-update :initform 0 :accessor previous-update)))

(defmethod (setf fullness) :around (value (stomach stomach))
  (call-next-method (min 1.0 (max 0.0 value)) stomach))

(defmethod eat ((food food) (stomach stomach))
  (incf (fullness stomach) (food-value food)))

(defmethod paint ((stomach stomach) (hud hud))
  (with-pushed-matrix
    (let* ((w 200)
           (f (* w (fullness stomach)))
           (h (height hud)))
      (gl:translate 5 (- h 80) 0)
      (gl:color 0 0 0)
      (with-primitives :quads
        (gl:vertex 0 0)
        (gl:vertex w 0)
        (gl:vertex w 10)
        (gl:vertex 0 10))
      (gl:color 1 1 0)
      (with-primitives :quads
        (gl:vertex 0 0)
        (gl:vertex f 0)
        (gl:vertex f 10)
        (gl:vertex 0 10))
      (gl:color 1 1 1))))

(define-handler (stomach tick) (ev)
  (when (<= (hunger-interval stomach) (- (clock *loop*) (previous-update stomach)))
    (setf (previous-update stomach) (clock *loop*))
    (decf (fullness stomach) 0.01)))
