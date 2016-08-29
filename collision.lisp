#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass moving-entity (located-entity)
    ((velocity :initarg :velocity :initform (vec 0 0 0) :accessor velocity)))
  (defclass pass-through () ()))

;; Not great to make every collidable moving, but simplifies things.
(define-subject collidable (moving-entity bound-entity pivoted-entity)
  ((hitbox :initarg :hitbox :initform (vec 0 0 0) :accessor hitbox)))

(defmethod initialize-instance :after ((collidable collidable) &key hitbox bounds)
  (when (and bounds (not hitbox))
    (setf (hitbox collidable) (vcopy bounds))))

;; We only do 2d testing since we don't jump or elevate.
(defun vec-in-box-p (vec loc ext)
  (and (<= (vx loc) (vx vec) (+ (vx loc) (vx ext)))
       (<= (vz loc) (vz vec) (+ (vz loc) (vz ext)))))

(defmethod intersects ((a collidable) (b collidable))
  (let ((apos (v+ (location a) (pivot a)))
        (bpos (v+ (location b) (pivot b)))
        (abound (hitbox a))
        (bbound (hitbox b)))
    (or (vec-in-box-p (v+ apos (vec           0           0           0)) bpos bbound)
        (vec-in-box-p (v+ apos (vec (vx abound)           0           0)) bpos bbound)
        (vec-in-box-p (v+ apos (vec (vx abound)           0 (vz abound))) bpos bbound)
        (vec-in-box-p (v+ apos (vec           0           0 (vz abound))) bpos bbound))))

(defun sfinv (a)
  (if (= 0 a) most-positive-single-float (/ a)))

(defun segment-test (a pos delta &optional (pad (vec 0 0 0)))
  (let* ((loc (location a))
         (bnd (v/ (hitbox a) 2))
         (xscale (sfinv (max 0.0001 (vx delta))))
         (zscale (sfinv (max 0.0001 (vz delta))))
         (xsign  (signum xscale))
         (zsign  (signum zscale))
         (xnear (* xscale (- (vx loc) (vx pos) (* xsign (+ (vx bnd) (vx pad))))))
         (znear (* zscale (- (vz loc) (vz pos) (* zsign (+ (vz bnd) (vz pad))))))
         (xfar  (* xscale (- (vx loc) (vx pos) (* -1 xsign (+ (vx bnd) (vx pad))))))
         (zfar  (* zscale (- (vz loc) (vz pos) (* -1 zsign (+ (vz bnd) (vz pad)))))))
    (unless (or (< zfar xnear) (< xfar znear))
      (let ((near (max xnear znear))
            (far  (min xfar zfar)))
        (unless (or (<= 1 near) (<= far 0))
          (min (max 0 near) 1))))))

(defmethod collides ((a collidable) (b collidable))
  ;; (let ((v (velocity a)))
  ;;   (cond ((v= v 0)
  ;;          (when (intersects a b) 0))
  ;;         (T
  ;;          (segment-test b (location a) (velocity a) (v/ (hitbox a) 2)))))
  (let ((distance (vlength (v- (location a) (location b)))))
    (when (< distance (/ (+ (vz (hitbox a)) (vz (hitbox b))) 2))
      distance)))
