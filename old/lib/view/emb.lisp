#|
  This file is a part of raccourciceur project.
|#

(in-package :cl-user)
(defpackage raccourciceur.view.emb
  (:use :cl)
  (:import-from :caveman
                :config))
(in-package :raccourciceur.view.emb)

(cl-syntax:use-syntax :annot)

@export
(defun render (file &optional params)
  (caveman.view.emb:render
   (merge-pathnames file
    (merge-pathnames
     (config :template-path)
     (config :application-root)))
   params))
