#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#


(asdf:defsystem ld36
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>"
  :description "Entry for the Ludum Dare 36"
  :homepage "https://github.com/Shirakumo/ld36"
  :serial T
  :defsystem-depends-on (:qtools)
  :components ((:file "package")
               (:file "main")
               (:file "screens")
               (:file "map-generator")
               (:file "collision")
               (:file "items")
               (:file "resources")
               (:file "inventory")
               (:file "stomach")
               (:file "flipping")
               (:file "colleen")
               (:file "critter"))
  :depends-on (:trial
               :array-utils)
  :build-operation "qt-program-op"
  :build-pathname "ld36"
  :entry-point "ld36:launch")
