#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-subject item (collidable)
  ())

(defmethod initialize-instance :after ((item item) &key pivot bounds)
  (when (and bounds (not pivot))
    (setf (pivot item) (vec (- (/ (vx (bounds item)) 2)) 0
                            (- (/ (vz (bounds item)) 2))))))

(defmethod paint ((item item) target)
  (with-pushed-matrix
    (gl:translate 0 0 (- (vz (pivot item))))
    (call-next-method)))

#+nil
(defmethod paint :after ((item item) target)
  (let ((bounds (bounds item)))
    (gl:disable :texture-2d)
    (gl:color 1.0 1.0 1.0 1.0)
    (with-primitives :quads
      (gl:vertex 0 0 0)
      (gl:vertex 0 0 (vz bounds))
      (gl:vertex (vx bounds) 0 (vz bounds))
      (gl:vertex (vx bounds) 0 0))
    (gl:enable :texture-2d)))

(define-asset texture bush (:ld36)
  :file "bush.png")

(define-subject bush (item face-entity)
  ()
  (:default-initargs
   :bounds (vec 40 40 10)
   :texture '(:ld36 bush)))

(define-asset texture tree (:ld36)
  :file "tree.png")

(define-subject tree (item face-entity)
  ()
  (:default-initargs
   :bounds (vec 40 150 10)
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
      (gl:vertex (- size) -0.5 (- size))
      (gl:tex-coord (+ xoff 2) (+ zoff 0))
      (gl:vertex (- size) -0.5 (+ size))
      (gl:tex-coord (+ xoff 2) (+ zoff 2))
      (gl:vertex (+ size) -0.5 (+ size))
      (gl:tex-coord (+ xoff 0) (+ zoff 2))
      (gl:vertex (+ size) -0.5 (- size)))))
