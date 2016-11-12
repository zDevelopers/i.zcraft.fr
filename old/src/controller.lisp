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
  (render "index.html" 
	  (nconc params
		 (redis:with-connection (:host "127.0.0.1")
					(list :vues (red:incr "vues")))


			(list :images
			  (redis:with-connection (:host "127.0.0.1")
						 (loop for num in (red:lrange "last" 0 10)
						       collect
						       (list :num num :nom (concatenate 'string num (red:get num))))))
		  )))

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
					
					;; l'appel a fonctionné
					(if (= 200 status-code)
					    (if (> (length result) 0)
						(progn
						  (let ((count (red:incr "images")))
						    (let ((stream (open (concatenate 'string "static/" (write-to-string count) extension) :direction :output :element-type '(unsigned-byte 8))))
						      (progn
							(loop for value across result
							      do
							      (write-byte value stream))
							(last-list count)
							(red:set count extension))
						      (render "index.html" 
							      (nconc params
								     (list ':lien
									   (concatenate 'string (write-to-string count) extension)))))))
					      "Url invalide")
					  ))))
	      "Probleme d'extension"
	      )))
      "L'url doit commencer par http")))


@url GET "/img/:id"
(defun img (params)
  (print "Affichage d'une image")
  (print params)
  (print "")
  (redis:with-connection (:host "127.0.0.1")
			 (progn
			   (render "index.html" 
				   (nconc params
					  (list
					   ':id
					   (getf params :id)
					   ':tn
					   (concatenate 'string "mini_" (getf params :id) ".jpg")
					   ':lien
					   (concatenate 'string (getf params :id) (red:get (getf params :id)))))))))


; Liste circulaire avec les 10 dernières images
(defun last-list (num)
  (redis:with-connection (:host "127.0.0.1")
			 (red:lpush "last" num)
			 (red:ltrim "last" 0 10)))

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
		     (last-list count)
		     (red:set count extension)
		     (rename-file path dest)
		     (if (probe-file dest)
			 (progn
			   (sb-ext:run-program "/usr/bin/convert" (list
								   (namestring dest)
								   "-resize" "300x300>" 
								   (concatenate 'string
										"static/mini_"
										(write-to-string count) ".jpg")))
			   (if (not (string-equal extension ".jpg"))
			       (sb-ext:run-program "/usr/bin/convert" (list
								       (namestring dest)
								       "-resize" "300x300>" 
								       (concatenate 'string
										    "static/mini_"
										    (write-to-string count) extension))))))
;		     (progn
;		       (render "index.html" 
;			       (nconc params
;				      (list 
;				       ':tn
;				       (concatenate 'string "mini_" (write-to-string count) extension)
;				       ':lien
;				       (concatenate 'string (write-to-string count) extension)))))))))
		     (caveman:redirect-to (concatenate 'string "/img/" (write-to-string count)))))))
	    
	    (progn
	      (delete-file path)))
	  ))))))


