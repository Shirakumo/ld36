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
                                (font (asset 'font :ld36 'screen)))
  (let ((resource (restore font)))
    (with-pushed-attribs T
      (with-painter (painter *context*)
        (with-finalizing ((color (mkcolor color)))
          (setf (q+:render-hint painter) (q+:qpainter.text-antialiasing))
          (setf (q+:render-hint painter) (q+:qpainter.high-quality-antialiasing))
          (setf (q+:font painter) (data resource))
          (setf (q+:color (q+:pen painter)) color)
          (loop for line in (cl-ppcre:split "\\n" text)
                for yy from y by (+ (size font) 6)
                do (q+:draw-text painter x yy line)))))))

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
      (draw-text 10 90 "Game Over" :color c :font (asset 'font :ld36 'title))
      (draw-text 10 120 (format NIL "Your stomach was filled for ~a." (format-time (end-time gameover))) :color c)
      (draw-text 10 300 "Press F6 or Start to restart." :color c))))

(defun format-time (time)
  (format NIL "~a minute~:p and ~a second~:p"
          (floor time 60) (mod (floor time) 60)))

(define-subject introduction (hud-entity unsavable)
  ())

(defmethod paint ((introduction introduction) (hud hud))
  (let* ((c (list 0 0 0 1)))
    (gl:color 1 1 1 0.7)
    (with-primitives :quads
      (gl:vertex 0 0)
      (gl:vertex (width hud) 0)
      (gl:vertex (width hud) (height hud))
      (gl:vertex 0 (height hud)))
    (gl:color 1 1 1 1)
    (draw-text 10 90 "SUPER GAYME" :color c :font (asset 'font :ld36 'title))
    (draw-text 10 120 "Try not to starve yourself to death." :color c)
    (draw-text 10 180 "

Movement:
Use/drop Item:
Interact:
Drop Item:
Cycle Inventory:
Restart:" :color c)
    (draw-text 150 180 "Keyboard Controls:

WASD
Left Click / E
Right Click / Space
Q
Scroll / 1, 2
F6" :color c)
    (draw-text 320 180 "Gamepad Controls:

Left Analog
B
A
Y
L1, L2
Start" :color c)
    (draw-text 10 400 "Click or press A to start the game." :color c)))

(define-handler (introduction tick) (ev)
  (stop *loop*))

(define-handler (introduction mouse-release) (ev button)
  (when (eql button :left)
    (leave introduction *loop*)
    (start *loop*)
    (abort)))

(define-handler (introduction gamepad-release) (ev button)
  (when (eql button :a)
    (leave introduction *loop*)
    (start *loop*)
    (abort)))

(define-action trial::reload-scene (trial::system-action)
  (gamepad-press (eql button :start))
  (key-press (eql key :f6)))
