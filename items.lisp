#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-subject item (pivoted-entity bound-entity)
  ())

(defmethod initialize-instance :after ((item item) &key pivot bounds)
  (when (and bounds (not pivot))
    (setf (pivot item) (nv- (v/ bounds 2)))))

(define-subject square-item (item)
  ((size :initarg :size :initform 10 :accessor size)))

(defmethod initialize-instance :after ((item square-item) &key pivot bounds)
  (let ((s (size item)))
    (unless bounds (setf (bounds item) (vec s s s)))
    (unless pivot (setf (pivot item) (vec (- (/ s 2)) 0 0)))))

(define-subject rectangle-item (item)
  ((width :initarg :width :initform 10 :accessor width)
   (height :initarg :height :initform 10 :accessor height)))

(defmethod initialize-instance :after ((item rectangle-item) &key pivot bounds width height)
  (unless bounds (setf (bounds item) (vec width height width)))
  (unless pivot (setf (pivot item) (vec 0 0 0))))

(define-asset texture bush (:ld36)
  :file "bush.png")

(define-subject bush (square-item face-entity)
  ()
  (:default-initargs
   :size 40
   :texture '(:ld36 bush)))

(define-asset texture tree (:ld36)
  :file "tree.png")

(define-subject tree (rectangle-item face-entity)
  ()
  (:default-initargs
   :width 40
   :height 150
   :texture '(:ld36 tree)))

(define-asset texture ground (:ld36)
  :file "ground.png"
  :wrapping :repeat)

(define-subject ground (textured-entity)
  ()
  (:default-initargs
   :texture '(:ld36 ground)))

(defmethod paint ((ground ground) target)
  (let* ((size 1000)
         (loc (location (unit :player (scene (window :main)))))
         (zoff (/ (mod (vx loc) size) size))
         (xoff (/ (mod (vz loc) size) size)))
    (gl:translate (vx loc) (vy loc) (vz loc))
    (gl:fog :fog-mode :linear)
    (gl:fog :fog-color (list 1.0 1.0 1.0 1.0))
    (gl:fog :fog-density 0.1)
    (gl:fog :fog-start 10.0)
    (gl:fog :fog-end size)
    (gl:enable :fog)
    (with-primitives :quads
      (gl:tex-coord (+ xoff 0) (+ zoff 0))
      (gl:vertex (- size) 0 (- size))
      (gl:tex-coord (+ xoff 2) (+ zoff 0))
      (gl:vertex (- size) 0 (+ size))
      (gl:tex-coord (+ xoff 2) (+ zoff 2))
      (gl:vertex (+ size) 0 (+ size))
      (gl:tex-coord (+ xoff 0) (+ zoff 2))
      (gl:vertex (+ size) 0 (- size)))))
