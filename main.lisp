#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(define-pool :ld36)

(define-widget main (QGLWidget trial:main)
  ((started :initform NIL :accessor started))
  (:default-initargs :clear-color (vec 1 1 1)
                     :title "Cool Gayme"))

(define-initializer (main set-resolution)
  (q+:resize main 1024 786))

(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    (reset scene)
    (enter (make-instance 'ground) scene)
    #+:ld36-debug (enter (make-instance 'space-axes :size 100) scene)
    (enter (make-instance 'colleen :inventory '(fireplace plaster stick stick)) scene)
    (enter (make-instance 'following-camera :name :camera
                                            :target (unit :player scene)
                                            :location (vec 0 100 150)) scene)
    (let ((populated-locations (populate-scene (make-instance 'noise-map
                                                              :width 3000
                                                              :height 3000
                                                              :tile-size 20)
                                               scene '(tree flower grass)
                                               :min-distance 2
                                               :cluster-size 3
                                               :zones '(50 210))))
      (nconc populated-locations (populate-scene (make-instance 'noise-map
                                                                :width 3000
                                                                :height 3000
                                                                :tile-size 40)
                                                 scene '(bush rock)
                                                 :filter-locations populated-locations
                                                 :cluster-size 4
                                                 :zones '(80 200)))
      (enter (make-instance 'mouse-tunnels :filter-locations populated-locations) scene))
    (enter (make-instance 'gameover :name :gameover) scene)))

(define-override (main focus-in-event) (ev)
  (start (scene main))
  (stop-overriding))

(define-override (main focus-out-event) (ev)
  (stop (scene main))
  (stop-overriding))

(defun launch ()
  #+sbcl (when *standalone* (sb-ext:disable-debugger))
  (trial:launch 'main :application-name "Cool Gayme"))
