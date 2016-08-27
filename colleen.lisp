#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-action player-action ())

(define-action movement (player-action))

(define-action start-left (movement)
  (key-press (one-of key :a :left))
  (gamepad-press (eql button :dpad-left))
  (gamepad-move (one-of axis :left-h :dpad-h) (< pos -0.2 old-pos)))

(define-action start-right (movement)
  (key-press (one-of key :d :right))
  (gamepad-press (eql button :dpad-right))
  (gamepad-move (one-of axis :left-h :dpad-h) (< old-pos 0.2 pos)))

(define-action start-up (movement)
  (key-press (one-of key :w :up))
  (gamepad-press (eql button :dpad-up))
  (gamepad-move (one-of axis :left-v :dpad-v) (< pos -0.2 old-pos)))

(define-action start-down (movement)
  (key-press (one-of key :s :down))
  (gamepad-press (eql button :dpad-down))
  (gamepad-move (one-of axis :left-v :dpad-v) (< old-pos 0.2 pos)))

(define-action stop-left (movement)
  (key-release (one-of key :a :left))
  (gamepad-release (eql button :dpad-left))
  (gamepad-move (one-of axis :left-h :dpad-h) (< old-pos -0.2 pos)))

(define-action stop-right (movement)
  (key-release (one-of key :d :right))
  (gamepad-release (eql button :dpad-right))
  (gamepad-move (one-of axis :left-h :dpad-h) (< pos 0.2 old-pos)))

(define-action stop-up (movement)
  (key-release (one-of key :w :up))
  (gamepad-release (eql button :dpad-up))
  (gamepad-move (one-of axis :left-v :dpad-v) (< old-pos -0.2 pos)))

(define-action stop-down (movement)
  (key-release (one-of key :s :down))
  (gamepad-release (eql button :dpad-down))
  (gamepad-move (one-of axis :left-v :dpad-v) (< pos 0.2 old-pos)))

(define-action perform (player-action)
  (key-press (one-of key :space))
  (gamepad-press (eql button :a)))

(define-retention movement (ev)
  (typecase ev
    (start-left (setf (retained 'movement :left) T))
    (start-right (setf (retained 'movement :right) T))
    (start-up (setf (retained 'movement :up) T))
    (start-down (setf (retained 'movement :down) T))
    (stop-left (setf (retained 'movement :left) NIL))
    (stop-right (setf (retained 'movement :right) NIL))
    (stop-up (setf (retained 'movement :up) NIL))
    (stop-down (setf (retained 'movement :down) NIL))))

(define-asset texture colleen-idle (:ld36)
  :file "colleen-idle.png")

(define-asset texture colleen-walking (:ld36)
  :file "colleen-walking.png")

(define-subject colleen (sprite-subject rotated-entity pivoted-entity)
  ((velocity :initarg :velocity :accessor velocity)
   (facing :initarg :facing :accessor facing))
  (:default-initargs
   :velocity (vec 0 0 0)
   :location (vec 0 0 0)
   :bounds (vec 50 80 1)
   :pivot (vec -25 0 0.5)
   :facing :left
   :name :player
   :animations '((idle 2.0 20 :texture (:ld36 colleen-idle))
                 (walk 0.7 20 :texture (:ld36 colleen-walking)))))

(define-handler (colleen tick) (ev)
  (with-slots (facing velocity location angle) colleen
    (cond ((retained 'movement :left) (setf facing :left))
          ((retained 'movement :right) (setf facing :right)))
    
    (setf (vx velocity)
          (cond ((retained 'movement :left) -5)
                ((retained 'movement :right) 5)
                (T 0))
          (vz velocity)
          (cond ((retained 'movement :up) -5)
                ((retained 'movement :down) 5)
                (T 0)))

    (if (< 0 (vlength velocity))
        (setf (animation colleen) 'walk)
        (setf (animation colleen) 'idle))
    
    (when (< 0 (vy location))
      (decf (vy velocity) 0.5))
    
    (nv+ location velocity)

    (let* ((ang (* (/ angle 180) PI))
           (vec (nvrot (vec -1 0 0) (vec 0 1 0) ang)))
      (when (< 0.01 (abs (- (vx vec) (ecase facing (:left -1) (:right 1)))))
        (incf angle 20)))

    (when (< (vy location) 0)
      (setf (vy location) 0)
      (setf (vy velocity) 0))))

(define-handler (colleen perform) (ev)
  (when (= 0 (vy (location colleen)))
    (setf (vy (velocity colleen)) 5)))

(defmethod paint ((colleen colleen) target)
  (call-next-method))
