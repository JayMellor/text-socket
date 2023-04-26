(defpackage text-socket
  (:use :cl)
  (:use :usocket)
  (:export #:create-server
		   #:create-client))

(in-package :text-socket)

(defconstant +localhost+ "127.0.0.1")
(defconstant +init-state+ '(:name nil
							:location nil
							:inventory nil)
  "Initial game state")

(defconstant +objects-by-location+ '(:bedroom (:description "You wake up in a hotel room to your alarm clock going off. There's a door to the corridor leading to the foyer opposite your bed. It's a nice, sunny day."
											   :objects ((:name "Alarm clock"
														  :code :alarm-clock
														  :description "Woke you up. Has 3 hands relating to hours, minutes and alarm.")
														 (:name "Suitcase"
														  :code :suitcase
														  :description "Contains everything you own except the clothes on your back."))))
  "All objects grouped by their initial location")

(defconstant +command-tags+ '(:name
							  :get
							  :inventory))

(defconstant +response-tags+ '(:name
							   :get
							   :error))

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
					(format t "Responding with: ~a~%" response)
					(send-over-connection connection (format nil "~a~%" response)))))))))))

;; (defun parse-request (request)
;;   "Expect string of form ':command <cmd> :content <content>'"
;;   (let ((content-pos (search ":content" request))
;; 		(command-pos (search ":command" request)))
;; 	(when (and (not (null command-pos))
;; 			   (not (null content-pos)))        
;; 		(let* ((content-start (+ content-pos
;; 								(length ":content")))
;; 			  (content-end (when (< content-pos command-pos)
;; 							 command-pos))
;; 			  (command-start (+ command-pos
;; 								(length ":command")))
;; 			  (command-end (when (> content-pos command-pos)
;; 							 content-pos))
;; 			   (command (string-trim " " (subseq request command-start command-end)))
;; 			   (content (string-trim " " (subseq request content-start content-end))))
;; 		  (list :command command
;; 				:content content)))))

(defun parse-request (request)
  (multiple-value-bind (tag-str args) (split-by-first-space request)    
	(let ((valid-tag (find tag-str +command-tags+
						   :test (lambda (tag-str cmd-tag)
								   (equalp tag-str (symbol-name cmd-tag))))))
	  (if (not (null valid-tag))
		  (list valid-tag args)
		  `(:error ,(format nil "bad request: ~a" request))))))

(defun generate-response (request state)  
  (let ((command (first request))
		(content (second request)))
	(case command 
	  (:name		   
	   (setf (getf state :name) content)
	   (format nil "name ~a" content))
	  
	  (:get
	   (let* ((location (getf state :location))
			  (in-inventory (in-inventory content state))
			  (in-location (object-in-location content location)))		   
		 (cond
		   (in-inventory
			(format nil "error in-inventory"))
		   ((not in-location)
			(format nil "error not-found"))
		   (t
			(push content (getf state :inventory))
			(format nil "~a ~a" command content)))))
	  
	  (:error
	   content))))

(defun in-inventory (object-code state)
  (let* ((inventory (getf state :inventory))
		(match (find object-code inventory
					 :test #'(lambda (object-code object)
							   (equalp object-code
									   (getf object :code))))))
	(not (null match))))

(defun object-in-location (object-code location-name)
  (let* ((location (getf +objects-by-location+ location-name))
		 (objects (getf location :objects))
		 (match (find object-code objects
					  :test #'(lambda (object-code object)
								(equalp object-code
										(getf object :code))))))
	(not (null match))))

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
					   (format t "parsed: ~a~%" parsed)
                       (let ((subject (first parsed))
							 (payload (second parsed)))
						 (format t "subject: ~a~% payload: ~a~%" subject payload)
						 (if (equalp subject :name)
							 (setf state (list subject payload))
							 (setf state (list :name (getf state :name)
											   subject payload)))))))))))))
(defun message-from-state (state)
  (format t "~a~%" state)
  (cond ((not (listp state))
		 "Whoops there's something off here. Please close and restart this session!")
		((or (null state)
			 (null (getf state :name)))         
		 (format nil "Welcome to Text Socket!~%Please enter your name:"))
		((not (null (getf state :get)))
		 (format nil "Picked up the ~a" (getf state "get")))
		((not (null (getf state :error)))
		 (getf state :error))
		(t
		 (format nil "Well ~a, what now?" (getf state :name)))))

(defun create-request (state command)  
  (if (null state)
	  (format nil "name ~a" command)
	  (multiple-value-bind (tag args) (split-by-first-space command)
		(if (find tag +command-tags+ :test #'(lambda (tag-str cmd-tag)
											   (equalp tag-str (symbol-name cmd-tag))))
			(format nil "~a ~a" tag args)
			nil))))

(defun parse-response (response)
  (multiple-value-bind (tag-str args) (split-by-first-space response)
	(let ((valid-tag (find tag-str +response-tags+
						   :test (lambda (tag-str cmd-tag)
								   (equalp tag-str (symbol-name cmd-tag))))))
	  (if (not (null  valid-tag))
		  (list valid-tag args)
		  `(:error ,(format nil "bad response: ~a" response))))))

(defun in-response (tag response)
  (let ((match (search tag response)))
	(and (not (null match))
		 (zerop match))))

(defun split-by-first-space (string)
  (let ((first-space-pos (search " " string)))
	(if (null first-space-pos)
		(values string nil)
		(values (subseq string 0 first-space-pos)
				(subseq string (+ first-space-pos 1))))))

;; ~% very important for read-line
;; are defconstants appropriate here?
