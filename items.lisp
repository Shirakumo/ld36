#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-subject item (face-entity collidable)
  ()
  (:default-initargs
   :bounds (vec 20 20 20)))

(defmethod paint :around ((item item) (hud hud))
  (with-pushed-matrix
    (gl:scale 5/2 5/2 5/2)
    (call-next-method)))

(defmethod use ((item item) player))

(defmethod interact ((item item) player)
  ;; bad.
  (leave item (scene (window :main)))
  (enter item (inventory player)))

(define-handler (item tick) (ev)
  (cond ((<= (vy (location item)) 0)
         (setf (vy (location item)) 0)
         (vsetf (velocity item) 0 0 0))
        (T
         (decf (vy (velocity item)) 0.5)
         (nv+ (location item) (velocity item)))))

(define-asset texture stick (:ld36)
  :file "stick.png")

(define-subject stick (item)
  ()
  (:default-initargs
   :texture '(:ld36 stick)))

(defmethod use ((stick stick) player)
  )

(define-asset texture pebble (:ld36)
  :file "pebble.png")

(define-subject pebble (item)
  ()
  (:default-initargs
   :texture '(:ld36 pebble)))
