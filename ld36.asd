#|
This file is a part of ld36
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem ld36
  :version "1.0.0"
  :license "Artistic"
  :description "Entry for the Ludum Dare 36"
  :homepage "https://github.com/Shirakumo/trial"
  :serial T
  :defsystem-depends-on (:qtools)
  :components ((:file "package")
               (:file "main"))
  :depends-on (:trial)
  :build-operation "qt-program-op"
  :build-pathname "ld36"
  :entry-point "ld36:launch")
