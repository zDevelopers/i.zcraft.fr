#|
  This file is a part of raccourciceur project.
|#

(in-package :cl-user)
(defpackage raccourciceur.app
  (:use :cl )
  (:import-from :caveman.app
                :<app>))
(in-package :raccourciceur.app)

(cl-syntax:use-syntax :annot)

@export
(defclass <raccourciceur-app> (<app>) ())

@export
(defvar *app* (make-instance '<raccourciceur-app>))
