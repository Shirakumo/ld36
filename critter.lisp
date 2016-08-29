#|
This file is a part of ld36
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-subject critter (item flipping pivoted-entity)
  ((behavior :initform :idle :accessor behavior)
   (previous-location :initform NIL :accessor previous-location)
   (last-moved :initform 0 :accessor last-moved)
   (target :initform NIL :accessor target)))

(defmethod initialize-instance :after ((critter critter) &key pivot bounds)
  (when (and bounds (not pivot))
    (setf (pivot critter) (vec (- (/ (vx (bounds critter)) 2)) 0
                               (- (/ (vz (bounds critter)) 2)))))
  (nv- (location critter) 1))

(defmethod paint ((critter critter) target)
  (with-pushed-matrix
    (gl:translate 0 0 (- (vz (pivot critter))))
    (call-next-method)))

(defmethod use ((critter critter) (with entity)))

(define-handler (critter tick) (ev)
  (with-slots (behavior last-moved target previous-location location velocity facing) critter
    (case behavior
      (:idle
       (when (< (random 600) (incf last-moved))
         (let* ((avoid-direction (when previous-location (normalize (v- location previous-location))))
                (relative-target (random-target 100 300 :avoid avoid-direction)))
           (setf target (v+ location relative-target)))
         (setf behavior :moving)))
      (:moving
       (let ((relative-target (v- target location)))
         (setf velocity
               (v* (vec 5 0 5) (normalize relative-target)))
         (cond ((< (distance velocity) (distance relative-target))
                (nv+ location velocity)
                (setf facing (if (< (vx velocity) 0) :left :right)))
               (T
                (setf location target)))
         (when (v= location target)
           (setf behavior :idle)
           (setf previous-location location)))))))

(defun random-target (min max &key avoid)
  (let ((x (* (if (< 0 (random 2)) 1 -1) (+ min (random (- max min)))))
        (z (* (if (< 0 (random 2)) 1 -1) (+ min (random (- max min))))))
    (when (and avoid (v< 0 avoid))
      ;; 66% chance to turn away from a direction being avoided if earlier random chance pointed that way
      (when (and (< 0 (* (vx avoid) x)) (< 1 (random 3)))
        (setf x (- x)))
      (when (and (< 0 (* (vz avoid) z)) (< 1 (random 3)))
        (setf z (- z))))
    ;; So that diagonal isn't a longer movement distance
    (cap-distance (vec x 0 z) max)))

(defun cap-distance (vec max)
  (let* ((distance (distance vec)) 
         (capper (/ max distance)))
    (vec (floor (* (vx vec) capper)) 0 (floor (* (vz vec) capper)))))

(defun distance (vec)
  ;; This is a faster way to calculate an approximate distance than (sqrt (* x x) (* y y)).
  ;; Another option would be (* (/ 1 (sqrt 2)) (+ x y)) but that's only better for when x roughly equals y.
  ;; Using this instead of (vlength) because there might be a bunch of critters going around.
  ;; TODO: (sqrt 2) and (sqrt 0.5) should be cached into parameter
  ;; NOTE: This is only the 2D space distance!!!
  (* (/ (1+ (sqrt (- 4 (* 2 (sqrt 2))))) 2)
     (max (abs (vx vec))
          (abs (vz vec))
          (* (sqrt 0.5) (+ (abs (vx vec)) (abs (vz vec)))))))

(defun normalize (vec)
  (v/ vec (distance vec)))

(define-asset texture mouse (:ld36)
  :file "flower.png")

(define-subject mouse (critter)
  ()
  (:default-initargs
   :bounds (vec 40 40 20)
   :texture '(:ld36 mouse)))

(defmethod use ((mouse mouse) (fireplace fireplace))
  (when (built fireplace)
    (let ((ham (make-instance 'ham)))
      (enter ham (scene (window :main)))
      (setf (location ham) (v+ (location fireplace) (vec 0 10 0))
            (velocity ham) (vec (- (random 8.0) 4)
                                (random 10.0)
                                (- (random 4.0) 2.0))))))
