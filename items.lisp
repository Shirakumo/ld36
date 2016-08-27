#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

;; Not necessary, but convenient because it compile-time defines.
(define-subject moving-entity (located-entity)
  ((velocity :initarg :velocity :initform (vec 0 0 0) :accessor velocity)))

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
  (let ((v (velocity a)))
    (cond ((v= v 0)
           (when (intersects a b) 0))
          (T
           (segment-test b (location a) (velocity a) (v/ (hitbox a) 2))))))

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
    ;; (gl:fog :fog-mode :linear)
    ;; (gl:fog :fog-color (list 1.0 1.0 1.0 1.0))
    ;; (gl:fog :fog-density 0.1)
    ;; (gl:fog :fog-start 10.0)
    ;; (gl:fog :fog-end size)
    ;; (gl:disable :fog)
    
    (with-primitives :quads
      (gl:tex-coord (+ xoff 0) (+ zoff 0))
      (gl:vertex (- size) -0.5 (- size))
      (gl:tex-coord (+ xoff 2) (+ zoff 0))
      (gl:vertex (- size) -0.5 (+ size))
      (gl:tex-coord (+ xoff 2) (+ zoff 2))
      (gl:vertex (+ size) -0.5 (+ size))
      (gl:tex-coord (+ xoff 0) (+ zoff 2))
      (gl:vertex (+ size) -0.5 (- size)))))
