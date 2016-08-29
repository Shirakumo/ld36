#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-pool :ld36)

(define-widget main (QGLWidget trial:main)
  ()
  (:default-initargs :clear-color (vec 1 1 1)))

(define-initializer (main first-time-setup -1000)
  (enter (make-instance 'introduction) (scene main)))

(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    (setf (clock scene) 0)
    (enter (make-instance 'ground) scene)
    #+:ld36-debug (enter (make-instance 'space-axes :size 100) scene)
    (enter (make-instance 'colleen :inventory '(fireplace plaster stick stick)) scene)
    (enter (make-instance 'following-camera :name :camera
                                            :target (unit :player scene)
                                            :location (vec 0 100 150)) scene)
    (let ((populated-locations (populate-scene (make-instance 'noise-map
                                                              :width 2000
                                                              :height 2000)
                                               scene
                                               '(tree bush rock flower grass))))
      (enter (make-instance 'mouse-tunnels :filter-locations populated-locations) scene))
    (enter (make-instance 'gameover :name :gameover) scene)))

(define-override (main focus-in-event) (ev)
  (start (scene main))
  (stop-overriding))

(define-override (main focus-out-event) (ev)
  (stop (scene main))
  (stop-overriding))

(defun launch ()
  (trial:launch 'main))
