#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(defclass inventory-item (textured-entity)
  ())

(defmethod use ((item inventory-item)))

(defmethod paint ((item inventory-item) (hud hud))
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

(define-action use (player-action)
  (mouse-press (one-of button :left))
  (key-press (one-of key :e))
  (gamepad-press (eql button :b)))

(define-action inventory-next (player-action)
  (mouse-scroll (< delta 0))
  (gamepad-press (eql button :r1)))

(define-action inventory-prev (player-action)
  (mouse-scroll (< 0 delta))
  (gamepad-press (eql button :l1)))

(define-asset texture inventory-item (:ld36)
  :file "inventory.png"
  :wrapping :repeat)

(define-subject inventory (hud-entity unsavable)
  ((items :initform () :accessor items)
   (index :initarg :index :initform 0 :accessor index)
   (invbg :initform (get-resource 'texture :ld36 'inventory-item) :reader invbg)))

(defmethod initialize-instance :after ((inventory inventory) &key items)
  (dolist (item items) (enter item inventory)))

(defmethod enter ((name symbol) (inventory inventory))
  (enter (make-instance name) inventory))

(defmethod enter ((item inventory-item) (inventory inventory))
  (pushnew item (items inventory)))

(defmethod leave ((item inventory-item) (inventory inventory))
  (setf (items inventory) (remove item (items inventory))))

(defmethod paint ((inventory inventory) (hud hud))
  (let ((h (height hud))
        (s 50)
        (p 5)
        (num (length (items inventory))))
    (gl:translate p (- h s p) 0)
    (loop for item in (items inventory)
          for i from 0
          do (when (= i (index inventory))
               (gl:bind-texture :texture-2d (data (invbg inventory)))
               (with-primitives :quads
                 (gl:tex-coord 0 0)
                 (gl:vertex (- p) (- p))
                 (gl:tex-coord 1 0)
                 (gl:vertex (+ s p) (- p))
                 (gl:tex-coord 1 1)
                 (gl:vertex (+ s p) (+ s p))
                 (gl:tex-coord 0 1)
                 (gl:vertex (- p) (+ s p))))
             (paint item hud)
             (gl:translate s 0 0))))

(define-handler (inventory use) (ev)
  (let ((item (elt (items inventory) (index inventory))))
    (when item
      (use item))))

(define-handler (inventory inventory-next) (ev)
  (v:info :test "~a ~a" (mod (1+ (index inventory)) (length (items inventory))) (length (items inventory)))
  (setf (index inventory) (mod (1+ (index inventory)) (length (items inventory)))))

(define-handler (inventory inventory-prev) (ev)
  (setf (index inventory) (mod (1- (index inventory)) (length (items inventory)))))

(define-asset texture stick (:ld36)
  :file "stick.png")

(defclass stick (inventory-item)
  ()
  (:default-initargs
   :texture '(:ld36 stick)))

(defmethod use ((stick stick))
  )

(define-asset texture pebble (:ld36)
  :file "pebble.png")

(defclass pebble (inventory-item)
  ()
  (:default-initargs
   :texture '(:ld36 pebble)))
