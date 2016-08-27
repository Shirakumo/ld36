#|
This file is a part of ld36
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.ld36)
(in-readtable :qtools)

(defun launch (&optional standalone)
  (setf *root* (if standalone
                   (uiop:argv0)
                   (asdf:system-source-directory :ld36)))
  (trial:launch))
