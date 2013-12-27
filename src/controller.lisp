(in-package :cl-user)
(defpackage raccourciceur.controller
  (:use :cl
        :caveman
	:drakma
	:cl-ppcre
        :raccourciceur.app)
  (:import-from :raccourciceur.view.emb
                :render))
(in-package :raccourciceur.controller)

(ql:quickload :cl-redis)

(cl-syntax:use-syntax :annot)

@url GET "/"
(defun index (params)
  @ignore params
  (render "index.html"))

@url POST "/dl"
(defun dl (params)
  (print "DL !")
  (print params)
  (let ((url (getf params :url)))

    ;; l'url est en http
    (if (cl-ppcre:scan-to-strings "^http" url)
	(progn
	  (let ((extension (cl-ppcre:scan-to-strings "\.(jpg|jpeg|png|PNG|JPG|JPEG|bmp|BMP|gif|GIF|svg|SVG)$" url)))
	    
	    ;; l'extension est valide
	    (if extension
		(progn
		  (redis:with-connection 
		   (:host "127.0.0.1")
		   (multiple-value-bind (result status-code)
					(drakma:http-request url)
					
					;; l'appel a fonctionne
					(if (= 200 status-code)
					    (if (> (length result) 0)
						(progn
						  (let ((count (red:incr "images")))
						    (let ((stream (open (concatenate 'string "static/" (write-to-string count) extension) :direction :output :element-type '(unsigned-byte 8))))
						      (progn
							(loop for value across result
							      do
							      (write-byte value stream)))
						      (render "index.html" 
							      (nconc params
								     (list ':lien
									   (concatenate 'string (write-to-string count) extension)))))))
					      "Url invalide")
					  ))))
	      "Probleme d'extension"
	      )))
      "L'url doit commencer par http")))


@url POST "/up"
(defun up (params)
  (print "Up en cours !")
  (print params)
  (print "")
  (time
   (let ((url (getf params :url)))
    (let ((path (first url)))
      (let ((nom (second url)))
	
	(let ((extension (cl-ppcre:scan-to-strings "\.(jpg|jpeg|png|PNG|JPG|JPEG|bmp|BMP|gif|GIF|svg|SVG)$" nom)))
	  
	  ;; l'extension est valide
	  (if extension	    
	      (progn
		(redis:with-connection 
		 (:host "127.0.0.1")
		 (let ((count (red:incr "images")))
		   (let ((dest (merge-pathnames 
				(concatenate 'string "static/" (write-to-string count) extension) 
				(truename ".")  )))
		     (rename-file path dest)
		     (if (probe-file dest)
			 (sb-ext:run-program "/usr/bin/convert" (list
								 (namestring dest)
								 "-resize" "300x300" 
								 (concatenate 'string
									      "static/mini_"
									      (write-to-string count) extension))))
		     (progn
		       (render "index.html" 
			       (nconc params
				      (list 
				       ':tn
				       (concatenate 'string "mini_" (write-to-string count) extension)
				       ':lien
				       (concatenate 'string (write-to-string count) extension)))))))))
	    
	    (progn
	      (delete-file path)))
	  ))))))


