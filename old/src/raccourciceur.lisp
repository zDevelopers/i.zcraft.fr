#|
  This file is a part of raccourciceur project.
|#

(in-package :cl-user)
(defpackage raccourciceur
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.static
        :clack.middleware.session)
  (:shadow :stop)
  (:import-from :caveman
                :config)
  (:import-from :caveman.project
                :<project>
                :build
                :project-mode
                :debug-mode-p)
  (:import-from :clack.builder
                :*builder-lazy-p*)
  (:import-from :raccourciceur.app
                :*app*)
  (:import-from :cl-ppcre
                :scan
                :regex-replace))
(in-package :raccourciceur)

(cl-syntax:use-syntax :annot)

@export
(defclass <raccourciceur> (<project>) ())

@export
(defvar *project* nil)

(defmethod build ((this <raccourciceur>))
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/static/|/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$|jpg$)" path)
              (ppcre:regex-replace "^/static" path "")))
    :root (merge-pathnames (config :static-path)
                           (config :application-root)))
   <clack-middleware-session>
   raccourciceur.app:*app*))

@export
(defun start (&key (mode :dev) (debug t) lazy port)
  (setf *project* (make-instance '<raccourciceur>))
  (caveman.project:start *project* :mode mode :debug debug :lazy lazy :port port))

@export
(defun stop ()
  (when *project*
    (caveman.project:stop *project*)
    (setf *project* nil)))
