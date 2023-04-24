(defpackage text-socket
  (:use :cl)
  (:use :usocket)
  (:export #:create-server
		   #:create-client))

(in-package :text-socket)

(defconstant +localhost+ "127.0.0.1")
(defconstant +init-state+ '(:name nil
							:location nil)
  "Initial game state")

(defmacro with-accepted-connection ((connection-var &rest socket-accept-args)
									&body body)
  `(usocket:with-server-socket (,connection-var (usocket:socket-accept ,@socket-accept-args))
	 ,@body))

(defun create-server (port)
  (usocket:with-socket-listener (socket +localhost+ port)
	(let ((state +init-state+))
	  (loop
		(with-accepted-connection (connection socket :element-type 'character)
		  (usocket:wait-for-input connection)
		  (multiple-value-bind (request eof-p)
			  (read-line (usocket:socket-stream connection)	nil)
			(if eof-p
				(format t "Client terminated~%")
				(progn 
				  (format t "Received ~a~%" request)
                  (let* ((parsed (parse-request request))
						 (response (generate-response parsed state))) 
					(send-over-connection connection (format nil "~a~%" response)))))))))))

(defun parse-request (request)
  "Expect string of form ':command <cmd> :content <content>'"
  (let ((content-pos (search ":content" request))
		(command-pos (search ":command" request)))
	(when (and (not (null command-pos))
			   (not (null content-pos)))        
		(let ((content-start (+ content-pos
								(length ":content")))
			  (content-end (when (< content-pos command-pos)
							 command-pos))
			  (command-start (+ command-pos
								(length ":command")))
			  (command-end (when (> content-pos command-pos)
							 content-pos)))
		  (list :command (string-trim " " (subseq request
												  command-start
												  command-end))
				:content (string-trim " " (subseq request
												  content-start
												  content-end)))))))

(defun generate-response (request state)
  (cond ((null request)
		 "Badly formed request")
		((equalp (getf request :command) "name")
		 (setf (getf state :name) (getf request :content))         
		 (format nil ":name ~a" (getf state :name)))
		(t
		 (format nil ":error command not found ~a" (getf request :command)))))

(defun update-state (request state)
  (cond ((null (getf request :name))
		 (setf (getf request :name) request))))

(defun send-over-connection (connection text)
  (progn
	(format (usocket:socket-stream connection) text)                
	(force-output (usocket:socket-stream connection))))

(defun create-client (port)
  (let ((state nil)
		(continue? t ))
	(loop
	  while (not (null continue?))
	  do (usocket:with-client-socket (socket stream +localhost+ port :element-type 'character)
           (format t "~a~%" (message-from-state state))
		   (format t "~a:> " (if (and (listp state)
									   (getf state :name))
								(getf state :name)
								""))
		   (let* ((command (read-line)))
			 (if (equalp command "exit")
				 (setf continue? nil)
				 (progn 
				   (let ((request (create-request state command)))
					 (send-over-connection socket (format nil "~a~%" request)))
				   (usocket:wait-for-input socket)
				   (let* ((response (read-line stream))
						  (parsed (parse-response response)))                     
					 (when (not (null parsed))
					   (setf (getf state
								   (first parsed))
							 (second parsed)))))))))))

(defun message-from-state (state)
  (cond ((not (listp state))
		 "Whoops there's something off here. Please close and restart this session!")
		((or (null state)
			 (null (getf state :name)))         
		 (format nil "Welcome to Text Socket!~%Please enter your name:"))        
		(t (format nil "Well ~a, what now?" (getf state :name)))))

(defun create-request (state command)  
  (if (null state)
	  (format nil ":command name :content ~a" command)
	  ;; todo
	  nil))

(defun parse-response (response)  
  (cond
	((or (null response)
		 (not (stringp response)))
	 nil)
	((let ((search-name (search ":name" response)))
	   (and (not (null search-name))
			(zerop search-name)))     
	 (list :name (string-trim " " (subseq response (length ":name")))))))

;; ~% very important for read-line
;; are defconstants appropriate here?
