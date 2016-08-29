#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(defmethod interact (a b))
(defmethod use (a b))

(define-subject resource (item)
  ())

(defmethod initialize-instance :after ((resource resource) &key pivot bounds)
  (when (and bounds (not pivot))
    (setf (pivot resource) (vec (- (/ (vx (bounds resource)) 2)) 0
                                (- (/ (vz (bounds resource)) 2)))))
  (nv- (location resource) 1))

(defmethod paint ((resource resource) target)
  (with-pushed-matrix
    (gl:translate 0 0 (- (vz (pivot resource))))
    (call-next-method)))

(defmethod paint ((resource resource) (hud hud))
  (with-pushed-matrix
    (gl:translate (- (vx (pivot resource))) 0 0)
    (call-next-method)))

(defmethod interact ((resource resource) player))

#+nil
(defmethod paint :after ((resource resource) target)
  (let ((bounds (bounds resource)))
    (gl:disable :texture-2d)
    (gl:color 1.0 1.0 1.0 1.0)
    (with-primitives :quads
      (gl:vertex 0 0 0)
      (gl:vertex 0 0 (vz bounds))
      (gl:vertex (vx bounds) 0 (vz bounds))
      (gl:vertex (vx bounds) 0 0))
    (gl:enable :texture-2d)))

(define-asset texture grass (:ld36)
  :file "grass.png")

(define-subject grass (resource pass-through)
  ()
  (:default-initargs
   :bounds (vec 40 40 20)
   :texture '(:ld36 grass)))

(define-asset texture bush (:ld36)
  :file "bush.png")

(define-asset texture bush2 (:ld36)
  :file "bush2.png")

(define-subject bush (resource pass-through)
  ()
  (:default-initargs
   :bounds (vec 50 50 20)
   :texture (alexandria:random-elt '((:ld36 bush)
                                     (:ld36 bush2)))))

(define-asset texture tree (:ld36)
  :file "tree.png")

(define-asset texture tree2 (:ld36)
  :file "tree2.png")

(define-subject tree (resource)
  ()
  (:default-initargs
   :bounds (vec 40 150 60)
   :texture (alexandria:random-elt '((:ld36 tree)
                                     (:ld36 tree2)))))

(defmethod interact ((tree tree) player)
  (enter 'stick (inventory player)))

(define-asset texture rock (:ld36)
  :file "rock.png")

(define-asset texture rock2 (:ld36)
  :file "rock2.png")

(define-subject rock (resource)
  ()
  (:default-initargs
   :bounds (vec 60 60 60)
   :texture (alexandria:random-elt '((:ld36 rock)
                                     (:ld36 rock2)))))

(defmethod interact ((rock rock) player)
  (enter 'pebble (inventory player)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass buildable (entity)
    ((requirements :accessor requirements))
    (:default-initargs :requirements NIL)))

(defmethod initialize-instance :after ((buildable buildable) &key requirements)
  (setf (requirements buildable) (copy-tree requirements)))

(defmethod built ((buildable buildable))
  (null (requirements buildable)))

(defmethod use (thing (buildable buildable))
  (with-slots (requirements) buildable
    (loop for cons in requirements
          for (type count) = cons
          do (when (typep thing type)
               (case count
                 (1 (setf requirements (remove cons requirements)))
                 (T (setf (second cons) (1- count))))
               (return T)))))

(defmethod paint :around ((buildable buildable) (main main))
  (cond ((built buildable) (call-next-method))
        (T (gl:color 1.0 0.0 0.0)
           (call-next-method)
           (gl:color 1.0 1.0 1.0))))

(define-asset texture fireplace (:ld36)
  :file "fireplace.png")

(define-subject fireplace (resource buildable)
  ((burning :initform NIL :accessor burning)
   (part-sys :initform (make-instance 'particle-system :jitter (vec 0.1 0.1 0.1)
                                                       :force (vec 0 0.1 0)) :accessor part-sys))
  (:default-initargs
   :bounds (vec 40 40 40)
   :texture '(:ld36 fireplace)
   :requirements '((stick 5)
                   (pebble 5))))

(defmethod interact ((fireplace fireplace) player)
  (when (built fireplace)
    (setf (burning fireplace) (not (burning fireplace)))
    T))

(defmethod use ((dead-mouse dead-mouse) (fireplace fireplace))
  (when (built fireplace)
    (let ((ham (make-instance 'ham)))
      (enter ham (scene (window :main)))
      (setf (location ham) (v+ (location fireplace) (vec 0 10 0))
            (velocity ham) (vec (- (random 8.0) 4)
                                (random 10.0)
                                (- (random 4.0) 2.0))))))

(defun random-range (range)
  (- (random (* 2.0 range)) range))

(defmethod paint :after ((fireplace fireplace) target)
  (when (burning fireplace)
    (dotimes (i 5)
      (add-particle (part-sys fireplace)
                    :loc (v- (vec (random-range 8) (+ (random-range 6) 13) (random-range 1))
                             (pivot fireplace))
                    :vel (vec (random 0.5) (random 0.5) (random 0.5))
                    :col (let ((r (random 0.3)))
                           (vec (+ (random 0.2) 0.8)
                                (+ r (random 0.3))
                                (+ r (random 0.1))))
                    :size (+ 1 (random 2.0))
                    :life (+ (random 20) 10))))
  (gl:bind-texture :texture-2d 0)
  (paint (part-sys fireplace) target)
  (gl:color 1 1 1))

(defclass particle-system ()
  ((particles :initform () :accessor particles)
   (force :initarg :force :initform (vec 0 0 0) :accessor force)
   (jitter :initarg :jitter :initform (vec 0 0 0) :accessor jitter)))

(defun add-particle (sys &key loc vel col size life)
  (push (make-instance 'particle :location loc :velocity vel :color col :size size :lifetime life)
        (particles sys)))

(defmethod paint ((sys particle-system) target)
  (setf (particles sys)
        (delete-if (lambda (part)
                     (decf (lifetime part))
                     (nv+ (velocity part) (force sys))
                     (let ((j (jitter sys)))
                       (nv+ (velocity part) (vec (random-range (vx j))
                                                 (random-range (vy j))
                                                 (random-range (vz j)))))
                     (nv+ (location part) (velocity part))
                     (paint part target)
                     (<= (lifetime part) 0))
                   (particles sys))))

(defclass particle (colored-entity moving-entity disc)
  ((lifetime :initarg :lifetime :accessor lifetime)
   (initial-lifetime :initarg :lifetime :accessor initial-lifetime))
  (:default-initargs :lifetime (+ (random 30) 10)))

(defmethod paint ((particle particle) target)
  (gl:blend-func :src-alpha :one)
  (gl:color (vx (color particle))
            (vy (color particle))
            (vz (color particle))
            (/ (lifetime particle) (initial-lifetime particle)))
  (call-next-method)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(define-asset texture plaster (:ld36)
  :file "plaster.png")

(define-asset texture plaster2 (:ld36)
  :file "plaster2.png")

(define-asset texture plaster3 (:ld36)
  :file "plaster3.png")

(define-subject plaster (resource pass-through)
  ()
  (:default-initargs
   :bounds (vec 40 40 40)
   :texture (alexandria:random-elt '((:ld36 plaster)
                                     (:ld36 plaster2)
                                     (:ld36 plaster3)))))

(defmethod paint :before ((plaster plaster) (main main))
  (gl:translate 0 (- (/ (vy (bounds plaster)) 2)) (vz (bounds plaster)))
  (gl:rotate -90 1 0 0))

(define-asset texture mouse-hole (:ld36)
  :file "mouse-hole.png")

(define-subject mouse-hole (resource pass-through)
  ((spawn-chance :initarg :spawn-chance :initform 0.005 :accessor spawn-chance)
   (gone :initform 0 :accessor gone)
   (live :initform 0 :accessor live)
   (spawn-max :initform (+ 1 (random 10)) :accessor spawn-max)
   (tunnels :initarg :tunnels :accessor tunnels))
  (:default-initargs
   :bounds (vec 40 40 40)
   :texture '(:ld36 mouse-hole)))

(define-handler (mouse-hole tick) (ev)
  (with-slots (spawn-chance live gone spawn-max) mouse-hole
    (when (and (< live spawn-max)
               (<= (random 1.0) spawn-chance))
      (incf (live mouse-hole))
      (enter (make-instance 'mouse :location (vcopy (location mouse-hole))
                                   :home mouse-hole) *loop*))
    (when (<= spawn-max gone)
      (leave mouse-hole (scene (window :main))))))

(defmethod leave :after ((mouse-hole mouse-hole) (scene scene))
  (when (running scene)
    (add-hole (tunnels mouse-hole))))

(define-subject mouse-tunnels ()
  ((noise-map :initform NIL :accessor noise-map)
   (filter-locations :initarg :filter-locations :accessor filter-locations)
   (location-queue :initform NIL :accessor location-queue)
   (new-locations :initform NIL :accessor new-locations)
   (old-locations :initform NIL :accessor old-locations)
   (hole-count :initarg :hole-count :accessor hole-count))
  (:default-initargs
   :hole-count 100))

(defmethod initialize-instance :after ((tunnels mouse-tunnels) &key (width 2000)
                                                                    (height 2000))
  (setf (noise-map tunnels) (make-instance 'noise-map :width width :height height)))

(defmethod enter :after ((tunnels mouse-tunnels) (scene scene))
  (let ((locations (locations (noise-map tunnels) '(210 50)
                              :cluster-size 3
                              :filter-locations (filter-locations tunnels))))
    (for:for ((loc in locations)
              (i repeat (hole-count tunnels)))
      (until (<= (hole-count tunnels) i))
      (enter (make-instance 'mouse-hole :location (vec (car loc) 0 (cdr loc))
                                        :tunnels tunnels) *loop*))
    (setf (old-locations tunnels) locations)))

(defmethod add-hole ((tunnels mouse-tunnels))
  (with-slots (location-queue new-locations old-locations hole-count noise-map filter-locations) tunnels
    (unless location-queue
      (setf (location-queue tunnels) (locations noise-map '(210 50)
                                                :cluster-size 3
                                                :filter-locations (append old-locations filter-locations new-locations))))
    (let* ((loc (pop (location-queue tunnels)))
           (new-hole (make-instance 'mouse-hole :location (vec (car loc) 0 (cdr loc))
                                                :tunnels tunnels)))
      (push loc (new-locations tunnels))
      (enter new-hole *loop*))
    (unless location-queue
      (setf (old-locations tunnels) new-locations
            (new-locations tunnels) NIL))))

(define-asset texture ground (:ld36)
  :file "ground.png"
  :wrapping :repeat)

(define-subject ground (textured-entity)
  ()
  (:default-initargs
   :texture '(:ld36 ground)))

(defmethod paint ((ground ground) target)
  (let* ((size 1000)
         (loc (location (unit :player (scene (window :main)))))
         (zoff (/ (mod (vx loc) size) size))
         (xoff (/ (mod (vz loc) size) size)))
    (with-pushed-matrix
      (gl:translate (vx loc) (vy loc) (vz loc))
      (gl:fog :fog-mode :linear)
      (gl:fog :fog-color (list 1.0 1.0 1.0 1.0))
      (gl:fog :fog-density 0.1)
      (gl:fog :fog-start 10.0)
      (gl:fog :fog-end size)
      (gl:enable :fog)
      
      (with-primitives :quads
        (gl:tex-coord (+ xoff 0) (+ zoff 0))
        (gl:vertex (- size) -0.5 (- size))
        (gl:tex-coord (+ xoff 2) (+ zoff 0))
        (gl:vertex (- size) -0.5 (+ size))
        (gl:tex-coord (+ xoff 2) (+ zoff 2))
        (gl:vertex (+ size) -0.5 (+ size))
        (gl:tex-coord (+ xoff 0) (+ zoff 2))
        (gl:vertex (+ size) -0.5 (- size))))))
