#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(defclass moving-entity (located-entity)
  ((velocity :initarg :velocity :initform (vec 0 0 0) :accessor velocity)))

;; Not great to make every collidable moving, but simplifies things.
(define-subject collidable (moving-entity bound-entity pivoted-entity)
  ((hitbox :initarg :hitbox :initform (vec 0 0 0) :accessor hitbox)))

(defmethod initialize-instance :after ((collidable collidable) &key hitbox bounds)
  (when (and bounds (not hitbox))
    (setf (hitbox collidable) (vcopy bounds))))

(defun vec-in-box-p (vec loc ext)
  (and (<= (vx loc) (vx vec) (+ (vx loc) (vx ext)))
       (<= (vy loc) (vy vec) (+ (vy loc) (vy ext)))
       (<= (vz loc) (vz vec) (+ (vz loc) (vz ext)))))

(defmethod intersects ((a collidable) (b collidable))
  (let ((apos (v+ (location a) (pivot a)))
        (bpos (v+ (location b) (pivot b)))
        (abound (hitbox a))
        (bbound (hitbox b)))
    (or (vec-in-box-p apos bpos bbound)
        (vec-in-box-p (v+ apos (vec (vx abound)           0           0)) bpos bbound)
        (vec-in-box-p (v+ apos (vec (vx abound) (vy abound)           0)) bpos bbound)
        (vec-in-box-p (v+ apos (vec (vx abound) (vy abound)           0)) bpos bbound)
        (vec-in-box-p (v+ apos (vec           0 (vy abound)           0)) bpos bbound)
        (vec-in-box-p (v+ apos (vec (vx abound)           0 (vz abound))) bpos bbound)
        (vec-in-box-p (v+ apos (vec (vx abound) (vy abound) (vz abound))) bpos bbound)
        (vec-in-box-p (v+ apos (vec (vx abound) (vy abound) (vz abound))) bpos bbound)
        (vec-in-box-p (v+ apos (vec           0 (vy abound) (vz abound))) bpos bbound))))

(defmethod collides ((a collidable) (b collidable))
  (let* ((amin (v+ (location a) (pivot a)))
         (amax (v+ amin (hitbox a)))
         (bmin (v+ (location b) (pivot b)))
         (bmax (v+ bmin (hitbox b)))
         (v (v- (velocity b) (velocity a)))
         (u0 most-negative-single-float)
         (u1 most-positive-single-float))
    (when (intersects a b) T)
    (macrolet ((inner (v?)
                 `(progn (cond ((and (< (,v? amax) (,v? bmin)) (< (,v? v) 0))
                                (let ((u_0 (/ (- (,v? amax) (,v? bmin)) (,v? v))))
                                  (when (< u0 u_0) (setf u0 u_0))))
                               ((and (< (,v? bmax) (,v? amin)) (< 0 (,v? v)))
                                (let ((u_0 (/ (- (,v? amin) (,v? bmax)) (,v? v))))
                                  (when (< u0 u_0) (setf u0 u_0)))))
                         (cond ((and (< (,v? amin) (,v? bmax)) (< 0 (,v? v)))
                                (let ((u_1 (/ (- (,v? amin) (,v? bmax)) (,v? v))))
                                  (when (< u_1 u1) (setf u1 u_1))))
                               ((and (< (,v? bmin) (,v? amax)) (< (,v? v) 0))
                                (let ((u_1 (/ (- (,v? amax) (,v? bmin)) (,v? v))))
                                  (when (< u_1 u1) (setf u1 u_1))))))))
      (inner vx)
      (inner vy)
      (inner vz))
    (when (and (<= u0 u1)
             (<= 0 u0)
             (<= u0 1))
      u0)))

(define-subject item (collidable)
  ())

(defmethod initialize-instance :after ((item item) &key pivot bounds)
  (when (and bounds (not pivot))
    (setf (pivot item) (vec 0 0 0))))

(define-subject square-item (item)
  ())

(defmethod initialize-instance :after ((item square-item) &key pivot bounds)
  (unless pivot (setf (pivot item) (vec 0 0 0))))

(define-subject rectangle-item (item)
  ())

(defmethod initialize-instance :after ((item rectangle-item) &key pivot bounds)
  (unless pivot (setf (pivot item) (vec 0 0 0))))

(define-asset texture bush (:ld36)
  :file "bush.png")

(define-subject bush (square-item face-entity)
  ()
  (:default-initargs
   :bounds (vec 100 100 100)
   :texture '(:ld36 bush)))

(define-asset texture tree (:ld36)
  :file "tree.png")

(define-subject tree (rectangle-item face-entity)
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
