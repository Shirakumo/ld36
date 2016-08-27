#|
 This file is a part of ld36
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>, Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:cl-user)
(defpackage #:ld36
  (:nicknames #:org.shirakumo.fraf.ld36)
  (:use #:cl+qt #:trial)
  (:shadowing-import-from #:flare #:slot)
  (:shadow #:launch #:main)
  (:export #:launch))

