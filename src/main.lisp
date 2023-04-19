(defpackage text-socket
  (:use :cl)
  (:use :usocket)
  (:export #:create-server
		   #:create-client))

(in-package :text-socket)

(defun create-server (port)
  (usocket:with-socket-listener (socket "127.0.0.1" port)
  	(loop 
	  (let ((connection (usocket:socket-accept socket :element-type 'character)))
		(format (usocket:socket-stream connection) "Hallo~%")                
		(force-output (usocket:socket-stream connection))
		(usocket:wait-for-input connection)
		(format t "Received: ~a~%"
				(read-line (usocket:socket-stream connection)))
		(usocket:socket-close connection)))))

(defun create-client (port)
  (usocket:with-client-socket (socket stream "127.0.0.1" port :element-type 'character)	
	(usocket:wait-for-input socket)
	(format t "Input is ~a~%" (read-line stream))
	(format (usocket:socket-stream socket) "Fredd~%")))
