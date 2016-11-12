#|
  This file is a part of raccourciceur project.
|#

(in-package :cl-user)
(defpackage raccourciceur-asd
  (:use :cl :asdf))
(in-package :raccourciceur-asd)

(defsystem raccourciceur
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:clack
               :caveman
               :cl-syntax
               :cl-syntax-annot
	       :drakma
	       :cl-redis
               :cl-ppcre)
  :components ((:module "lib"
                :components
                ((:module "view"
                  :components
                  ((:file "emb")))))
               (:module "src"
                :depends-on ("lib")
                :components
                ((:file "app")
                 (:file "raccourciceur" :depends-on ("app"))
                 (:file "controller" :depends-on ("app")))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op raccourciceur-test))))
