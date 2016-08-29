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

(define-subject gameover (hud-entity unsavable clocked-subject)
  ((fade-duration :initarg :fade-duration :initform 5.0 :accessor fade-duration)
   (end-time :initform 0 :accessor end-time)))

(defmethod start :after ((gameover gameover))
  (setf (end-time gameover) (clock (scene (window :main)))))

(defmethod paint ((gameover gameover) (hud hud))
  (when (running gameover)
    (let ((a (/ (min (clock gameover) (fade-duration gameover))
                (fade-duration gameover))))
      (gl:color 0 0 0 a)
      (with-primitives :quads
        (gl:vertex 0 0)
        (gl:vertex (width hud) 0)
        (gl:vertex (width hud) (height hud))
        (gl:vertex 0 (height hud)))
      (gl:color 1 1 1 1)
      (draw-text 10 90 "Game Over"
                 :font (get-resource 'font :ld36 'title)
                 :color (list 1 1 1 a))
      (draw-text 10 120 (format NIL "Your stomach was filled for ~a." (format-time (end-time gameover)))
                 :color (list 1 1 1 a)))))

(defun format-time (time)
  (format NIL "~a minute~:p and ~a second~:p"
          (floor time 60) (mod (floor time) 60)))
