#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-asset texture selected-item (:ld36)
  :file "inventory.png")

(define-subject inventory (hud-entity unsavable)
  ((items :initform () :accessor items)
   (index :initarg :index :initform 0 :accessor index)
   (invbg :initform (get-resource 'texture :ld36 'selected-item) :reader invbg)))

(defmethod initialize-instance :after ((inventory inventory) &key items)
  (dolist (item items) (enter item inventory)))

(defmethod enter ((name symbol) (inventory inventory))
  (enter (make-instance name) inventory))

(defmethod enter ((item item) (inventory inventory))
  (vsetf (location item) 0 0 0)
  (pushnew item (items inventory)))

(defmethod leave ((item item) (inventory inventory))
  (setf (items inventory) (remove item (items inventory))))

(defmethod paint ((inventory inventory) (hud hud))
  (let ((h (height hud))
        (s 50)
        (p 5))
    (with-pushed-matrix
      (gl:translate p (- h s p) 0)
      (loop for item in (items inventory)
            for i from 0
            do (when (= i (index inventory))
                 (gl:bind-texture :texture-2d (data (invbg inventory)))
                 (with-primitives :quads
                   (gl:tex-coord 0 0)
                   (gl:vertex 0 0)
                   (gl:tex-coord 1 0)
                   (gl:vertex s 0)
                   (gl:tex-coord 1 1)
                   (gl:vertex s s)
                   (gl:tex-coord 0 1)
                   (gl:vertex 0 s)))
               (paint item hud)
               (gl:translate s 0 0)))))

(defmethod select-next ((inventory inventory))
  (setf (index inventory) (mod (1+ (index inventory)) (length (items inventory)))))

(defmethod select-prev ((inventory inventory))
  (setf (index inventory) (mod (1- (index inventory)) (length (items inventory)))))

(defmethod item ((inventory inventory) &optional (index (index inventory)))
  (when (< -1 index (length (items inventory)))
    (nth index (items inventory))))

(defmethod remove-item ((inventory inventory) &optional (index (index inventory)))
  (when (< -1 index (length (items inventory)))
    (let ((item (elt (items inventory) index)))
      (leave item inventory)
      (when (and (<= 1 (length (items inventory)) (index inventory)))
        (decf (index inventory)))
      item)))
