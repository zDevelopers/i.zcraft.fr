(ql:quickload :cl-redis)
(ql:quickload :raccourciceur)

(raccourciceur:start)



;(ql:quickload :drakma)
;(ql:quickload :cl-redis)
;(ql:quickload :cl-ppcre)


;(redis:with-connection 
; (:host "127.0.0.1")
; (let ((count (red:incr "images")))
;   (let ((url "http://pix.toile-libre.org/upload/img/1379966670.jpg"))
;     (let ((extension (cl-ppcre:scan-to-strings "\.([a-zA-Z]*)$" url)))
;       (let ((stream (open (concatenate 'string (write-to-string count) extension) :direction :output :element-type '(unsigned-byte 8))));
;	 (let ((result (drakma:http-request url)))
;	   (loop for value across result
;		 do
;		 (write-byte value stream))))
 ;      ))))
 
