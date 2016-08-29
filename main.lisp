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

(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    (enter (make-instance 'ground) scene)
    (enter (make-instance 'space-axes :size 100) scene)
    (enter (make-instance 'colleen :inventory '(fireplace plaster stick stick)) scene)
    (enter (make-instance 'following-camera :name :camera
                                            :target (unit :player scene)
                                            :location (vec 0 100 150)) scene)
    (enter (make-instance 'bush :location (vec -50 0 -50)) scene)
    (enter (make-instance 'tree :location (vec 100 0 100)) scene)
    (enter (make-instance 'rock :location (vec -100 0 100)) scene)
    (let ((populated-locations (populate-scene (make-instance 'noise-map
                                                              :width 2000
                                                              :height 2000)
                                               scene
                                               '(tree bush rock flower grass))))
      (populate-scene (make-instance 'noise-map :width 2000 :height 2000)
                      scene '(mouse-hole) :filter-locations populated-locations
                      :zones '(210 50) :cluster-size 3))))

(defun launch ()
  (trial:launch 'main))
