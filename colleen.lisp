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
  (mouse-press (one-of button :right))
  (key-press (one-of key :space))
  (gamepad-press (eql button :a)))

(define-action use (player-action)
  (mouse-press (one-of button :left))
  (key-press (one-of key :e))
  (gamepad-press (eql button :b)))

(define-action drop (player-action)
  (key-press (one-of key :q))
  (gamepad-press (eql button :y)))

(define-action inventory-next (player-action)
  (key-press (one-of key :2))
  (mouse-scroll (<= 1 delta))
  (gamepad-press (eql button :r1)))

(define-action inventory-prev (player-action)
  (key-press (one-of key :1))
  (mouse-scroll (<= delta -1))
  (gamepad-press (eql button :l1)))

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

(define-asset texture colleen-using (:ld36)
  :file "colleen-using.png")

(define-asset texture colleen-throw (:ld36)
  :file "colleen-throw.png")

(define-asset texture colleen-die (:ld36)
  :file "colleen-die.png")

(define-subject colleen (sprite-subject collidable flipping pivoted-entity)
  ((inventory :initform NIL :accessor inventory)
   (placing :initform NIL :accessor placing)
   (stomach :initform NIL :accessor stomach))
  (:default-initargs
   :location (vec 0 0 0)
   :hitbox (vec 50 80 20)
   :bounds (vec 50 80 1)
   :pivot (vec -25 0 0.5)
   :name :player
   :animations '((idle  2.0 20 :texture (:ld36 colleen-idle) :next idle)
                 (walk  0.7 20 :texture (:ld36 colleen-walking) :next walk)
                 (use   0.7 12 :texture (:ld36 colleen-using) :next idle)
                 (throw 0.7 12 :texture (:ld36 colleen-throw) :next idle)
                 (die   2.0 37 :texture (:ld36 colleen-die)))))

(defmethod initialize-instance :after ((colleen colleen) &key inventory fullness)
  (setf (inventory colleen) (make-instance 'inventory :items inventory))
  (setf (stomach colleen) (make-instance 'stomach :fullness (or fullness 1.0))))

(defmethod enter :after ((colleen colleen) (scene scene))
  (enter (inventory colleen) scene)
  (enter (stomach colleen) scene))

(defmethod leave :after ((colleen colleen) (scene scene))
  (leave (inventory colleen) scene)
  (leave (stomach colleen) scene))

(defun align (vector grid)
  (vec (* (round (vx vector) (vx grid)) (vx grid))
       (* (round (vy vector) (vy grid)) (vy grid))
       (* (round (vz vector) (vz grid)) (vz grid))))

(define-handler (colleen tick) (ev)
  (with-slots (facing velocity location placing stomach) colleen
    (cond ((<= (fullness stomach) 0)
           (setf (animation colleen) 'die))
          (T
           (cond ((retained 'movement :left) (setf facing :left))
                 ((retained 'movement :right) (setf facing :right)))
           
           (let ((prevlen (vlength velocity)))
             (setf (vx velocity)
                   (cond ((retained 'movement :left) -5)
                         ((retained 'movement :right) 5)
                         (T 0))
                   (vz velocity)
                   (cond ((retained 'movement :up) -5)
                         ((retained 'movement :down) 5)
                         (T 0)))

             (when (< 0 (vlength velocity))
               (setf (animation colleen) 'walk))
             (when (and (= 0 (vlength velocity)) (/= 0 prevlen))
               (setf (animation colleen) 'idle)))
           
           (when (< 0 (vy location))
             (decf (vy velocity) 0.5))

           (do-container-tree (item *loop*)
             (when (and (not (eql item colleen))
                        (not (eql item (placing colleen)))
                        (typep item 'collidable))
               (let ((time (collides colleen item)))
                 (when time
                   (let* ((colpos (v+ location (v* velocity time)))
                          (ll (v+ (location item) (pivot item)))
                          (rr (v+ ll (bounds item))))
                     (cond ((or (and (<= (abs (- (vx colpos) (vx ll))) 1)
                                     (< 0 (vx velocity)))
                                (and (<= (abs (- (vx colpos) (vx rr))) 1)
                                     (< (vx velocity) 0)))
                            (setf (vx velocity) 0))
                           ((or (and (<= (abs (- (vz colpos) (vz ll))) 1)
                                     (< 0 (vz velocity)))
                                (and (<= (abs (- (vz colpos) (vz rr))) 1)
                                     (< (vz velocity) 0)))
                            (setf (vz velocity) 0))))))
               (when (and (typep item 'plaster)
                          (close-by colleen item))
                 (cond ((< 0 (vx velocity))
                        (setf (vx velocity) 8))
                       ((< (vx velocity) 0)
                        (setf (vx velocity) -8))))))

           (nv+ location velocity)

           (when (< (vy location) 0)
             (setf (vy location) 0)
             (setf (vy velocity) 0))

           (when placing
             (let ((maybe-loc (v+ location (vec (* (vx (bounds placing)) (ecase facing (:left -1) (:right 1)))
                                                0 0))))
               (setf (location placing) (v+ (align maybe-loc (bounds placing))
                                            (vec 0 5 0)))))))))

(defmethod interactables ((colleen colleen))
  (let ((found ()))
    (do-container-tree (item *loop*)
      (when (and (not (eql item colleen))
                 (not (eql item (placing colleen)))
                 (typep item 'collidable))
        (when (close-by colleen item)
          (push item found))))
    (sort found #'< :key (lambda (a) (vlength (v- (location colleen)
                                                  (location a)))))))

(define-handler (colleen perform) (ev)
  (when (cond
          ((placing colleen)
           (setf (placing colleen) NIL))
          (T
           (loop for interactable in (interactables colleen)
                 thereis (interact interactable colleen))))
    (setf (animation colleen) 'use)))

(define-handler (colleen use) (ev)
  (let* ((item (item (inventory colleen))))
    (when item
      (when (or (loop for interactable in (interactables colleen)
                      thereis (use item interactable))
                (use item colleen))
        (setf (animation colleen) 'use)
        (leave item (inventory colleen))))))

(define-handler (colleen drop) (ev)
  (cond ((placing colleen)
         (leave (placing colleen) *loop*)
         (setf (placing colleen) NIL))
        (T
         (let ((item (remove-item (inventory colleen))))
           (when item
             (setf (animation colleen) 'throw)
             (setf (location item) (nv+ (vec (ecase (facing colleen)
                                               (:left -30)
                                               (:right 10))
                                             40
                                             0)
                                        (location colleen)))
             (setf (velocity item) (nv+ (vec (ecase (facing colleen)
                                               (:left (- (random 4.0)))
                                               (:right (random 4.0)))
                                             (random 10.0)
                                             (- (random 4.0) 2.0))
                                        (velocity colleen)))
             (enter item *loop*))))))

(define-handler (colleen inventory-next) (ev)
  (select-next (inventory colleen)))

(define-handler (colleen inventory-prev) (ev)
  (select-prev (inventory colleen)))

(defmethod use ((resource resource) (colleen colleen))
  (unless (placing colleen)
    (let ((resource (make-instance (type-of resource))))
      (enter resource (scene (window :main)))
      (setf (placing colleen) resource))
    T))

(defmethod use ((food food) (colleen colleen))
  (eat food (stomach colleen))
  T)
