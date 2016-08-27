#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(defclass item (textured-entity)
  ())

(defmethod use ((item item)))

(defmethod paint ((item item) target)
  (gl:color 1.0 1.0 1.0)
  (with-primitives :quads
    (gl:tex-coord 0 0)
    (gl:vertex 0 50)
    (gl:tex-coord 0 1)
    (gl:vertex 50 50)
    (gl:tex-coord 1 1)
    (gl:vertex 50 0)
    (gl:tex-coord 1 0)
    (gl:vertex 0 0)))

(define-asset texture stick (:ld36)
  :file "stick.png")

(defclass stick (item)
  ()
  (:default-initargs
   :texture '(:ld36 stick)))

(defmethod use ((stick stick))
  )

(define-asset texture pebble (:ld36)
  :file "pebble.png")

(defclass pebble (item)
  ()
  (:default-initargs
   :texture '(:ld36 pebble)))
