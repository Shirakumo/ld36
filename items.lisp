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

(define-subject square-item (item square)
  ())

(defmethod initialize-instance :after ((item square-item) &key pivot bounds)
  (let ((s (size item)))
    (unless bounds (setf (bounds item) (vec s s s)))
    (unless pivot (setf (pivot item) (vec 0 (/ s 2) 0)))))

(define-subject rectangle-item (item rectangle)
  ())

(defmethod initialize-instance :after ((item rectangle-item) &key pivot bounds width height)
  (unless bounds (setf (bounds item) (vec width height width)))
  (unless pivot (setf (pivot item) (vec 0 (/ height 2) 0))))

(define-asset texture bush (:ld36)
  :file "bush.png")

(define-subject bush (square-item)
  ()
  (:default-initargs
   :size 40))

(define-asset texture tree (:ld36)
  :file "tree.png")

(define-subject tree (rectangle-item)
  ()
  (:default-initargs
   :width 20
   :height 100))
