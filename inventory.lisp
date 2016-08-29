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
  ((items :initform (make-array 0 :adjustable T :fill-pointer T) :accessor items)
   (index :initarg :index :initform 0 :accessor index)
   (invbg :initform (get-resource 'texture :ld36 'selected-item) :reader invbg)))

(defmethod initialize-instance :after ((inventory inventory) &key items)
  (dolist (item items) (enter item inventory)))

(defmethod enter ((name symbol) (inventory inventory))
  (enter (make-instance name) inventory))

(defun inv-find-container (item inventory)
  (find (type-of item) (items inventory) :test (lambda (a b) (typep (elt b 0) a))))

(defmethod enter ((item item) (inventory inventory))
  (vsetf (location item) 0 0 0)
  (let ((vec (inv-find-container item inventory)))
    (if vec
        (vector-push-extend item vec)
        (vector-push-extend (make-array 1 :initial-element item :adjustable T :fill-pointer T)
                            (items inventory))))
  item)

(defmethod leave ((item item) (inventory inventory))
  (let ((vec (inv-find-container item inventory)))
    (cond ((= 1 (length vec))
           (array-utils:vector-pop-position
            (items inventory)
            (position vec (items inventory)))
           (when (and (<= 1 (length (items inventory)) (index inventory)))
             (decf (index inventory)))
           item)
          (T
           (array-utils:vector-pop-front vec)))))

(defmethod leave ((resource resource) (inventory inventory))
  NIL)

(defmethod leave ((index integer) (inventory inventory))
  (when (< -1 index (length (items inventory)))
    (let ((item (item inventory index)))
      (leave item inventory))))

(defun mkcolor (vec)
  (flet ((fmt (a) (cond ((<= 0 a 1) (round (* 255 a)))
                        ((< a 0) 0)
                        ((< 255 a) 255)
                        (T a))))
    (etypecase vec
      (vec (q+:make-qcolor (fmt (vx vec)) (fmt (vy vec)) (fmt (vz vec))))
      (list (destructuring-bind (r g b &optional (a 1.0)) vec
              (q+:make-qcolor (fmt r) (fmt g) (fmt b) (fmt a)))))))

(defun draw-text (x y text &key (color (vec 1 1 1))
                                (font (get-resource 'font :trial :debug-hud)))
  (with-pushed-attribs T
    (with-painter (painter *context*)
      (with-finalizing ((color (mkcolor color)))
        (setf (q+:render-hint painter) (q+:qpainter.text-antialiasing))
        (setf (q+:render-hint painter) (q+:qpainter.high-quality-antialiasing))
        (setf (q+:font painter) (data font))
        (setf (q+:color (q+:pen painter)) color)
        (q+:draw-text painter x y text)))))

(defmethod paint ((inventory inventory) (hud hud))
  (let ((h (height hud))
        (s 50)
        (p 5))
    (with-pushed-matrix
      (gl:translate p (- h p) 0)
      (gl:scale 1 -1 1)
      (draw-text p (- h p s 5) "Inventory: ")
      (loop for item across (items inventory)
            for i from 0
            do (paint (elt item 0) hud)
               (when (= i (index inventory))
                 (gl:bind-texture :texture-2d (data (invbg inventory)))
                 (with-primitives :quads
                   (gl:tex-coord 0 0)
                   (gl:vertex 0 0)
                   (gl:tex-coord 1 0)
                   (gl:vertex s 0)
                   (gl:tex-coord 1 1)
                   (gl:vertex s s)
                   (gl:tex-coord 0 1)
                   (gl:vertex 0 s))
                 (gl:bind-texture :texture-2d 0))
               (unless (typep (elt item 0) 'resource)
                 (draw-text (* s (1+ i)) (- h s) (princ-to-string (length item))))
               (gl:translate s 0 0)))))

(defmethod select-next ((inventory inventory))
  (when (< 0 (length (items inventory)))
    (setf (index inventory) (mod (1+ (index inventory)) (length (items inventory))))))

(defmethod select-prev ((inventory inventory))
  (when (< 0 (length (items inventory)))
    (setf (index inventory) (mod (1- (index inventory)) (length (items inventory))))))

(defmethod item ((inventory inventory) &optional (index (index inventory)))
  (when (< -1 index (length (items inventory)))
    (let ((vec (elt (items inventory) index)))
      (when vec (elt vec 0)))))

(defmethod remove-item ((inventory inventory) &optional (index (index inventory)))
  (leave index inventory))
