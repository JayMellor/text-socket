(defpackage text-socket
  (:use :cl)
  (:use :usocket)
  (:export #:create-server
		   #:create-client))

(in-package :text-socket)

(defconstant +localhost+ "127.0.0.1")

(defmacro with-accepted-connection ((connection-var &rest socket-accept-args)
									&body body)
  `(usocket:with-server-socket (,connection-var (usocket:socket-accept ,@socket-accept-args))
	 ,@body))

(defun create-server (port)
  (usocket:with-socket-listener (socket +localhost+ port)
	(let ((name nil))
	  (loop
		(with-accepted-connection (connection socket :element-type 'character)
		  (usocket:wait-for-input connection)
		  (multiple-value-bind (request eof-p)
			  (read-line (usocket:socket-stream connection)	nil)
			(if eof-p
				(format t "Client terminated~%")
				(progn 
				  (format t "Received ~a~%" request)
				  (setf name request)
				  (send-over-connection connection (format nil "Hi ~a~%" name))))))))))

(defun get-name (connection)
  (send-over-connection connection "Welcome! Please Enter your name:~%")
  (usocket:wait-for-input connection)
  (let (( name (read-line (usocket:socket-stream connection))))
	(send-over-connection connection (format nil "Welcome ~a!~%" name))
	name))

(defun send-over-connection (connection text)
  (progn
	(format (usocket:socket-stream connection) text)                
	(force-output (usocket:socket-stream connection))))

(defun create-client (port)  
  (loop
	(usocket:with-client-socket (socket stream +localhost+ port :element-type 'character)
	  (format t "Welcome to Text Socket!~% Please enter your name:~%")
	  (let ((command (format nil "~a~~%" (read-line))))        
		(send-over-connection socket command))
	  (usocket:wait-for-input socket)
	  (format t "~a~%" (read-line stream))
	  )))

;; ~% very important!
;; if server is waiting and client is cut, server breaks
