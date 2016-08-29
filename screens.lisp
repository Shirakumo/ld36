#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-asset font title (:ld36)
  :family "Impact, Arial"
  :size 72)

(define-asset font screen (:ld36)
  :family "Arial"
  :size 12)

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
                                (font (get-resource 'font :ld36 'screen)))
  (with-pushed-attribs T
    (with-painter (painter *context*)
      (with-finalizing ((color (mkcolor color)))
        (setf (q+:render-hint painter) (q+:qpainter.text-antialiasing))
        (setf (q+:render-hint painter) (q+:qpainter.high-quality-antialiasing))
        (setf (q+:font painter) (data font))
        (setf (q+:color (q+:pen painter)) color)
        (q+:draw-text painter x y text)))))

(define-subject gameover (hud-entity unsavable clocked-subject)
  ((fade-duration :initarg :fade-duration :initform 5.0 :accessor fade-duration)
   (end-time :initform 0 :accessor end-time)))

(defmethod start :after ((gameover gameover))
  (setf (end-time gameover) (clock (scene (window :main)))))

(defmethod paint ((gameover gameover) (hud hud))
  (when (running gameover)
    (let* ((a (/ (min (clock gameover) (fade-duration gameover))
                 (fade-duration gameover)))
           (c (list 1 1 1 a)))
      (gl:color 0 0 0 a)
      (with-primitives :quads
        (gl:vertex 0 0)
        (gl:vertex (width hud) 0)
        (gl:vertex (width hud) (height hud))
        (gl:vertex 0 (height hud)))
      (gl:color 1 1 1 1)
      (draw-text 10 90 "Game Over" :color c :font (get-resource 'font :ld36 'title))
      (draw-text 10 120 (format NIL "Your stomach was filled for ~a." (format-time (end-time gameover))) :color c :font (get-resource 'font :ld36 'screen))
      (draw-text 10 200 "Press F6 to restart." :color c))))

(defun format-time (time)
  (format NIL "~a minute~:p and ~a second~:p"
          (floor time 60) (mod (floor time) 60)))
