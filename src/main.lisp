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

(defconstant +command-tags+ '(:get
							  :inventory
							  :look
							  :help
							  :exit))

(defconstant +request-tags+ '(:name
							  :get
							  :inventory
							  :look))

(defconstant +response-tags+ '(:name
							   :get
							   :inventory
                               :location
							   :error))

(defmacro with-accepted-connection ((connection-var &rest socket-accept-args)
									&body body)
  `(usocket:with-server-socket (,connection-var (usocket:socket-accept ,@socket-accept-args))
	 ,@body))

(defun create-server (port)
  (usocket:with-socket-listener (socket +localhost+ port)
	(let ((state nil))
	  (setf state +init-state+) ;; to fix weird issues with STATE initialising properly
	  ;; Change :location to object?
	  (setf (getf state :location) :bedroom) ;;; todo change to use start check
	  (format t "state: ~a" state)
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

(defun parse-request (request)  
  (multiple-value-bind (tag-str args-str) (split-by-first-space request)    
	(let ((valid-tag (find tag-str +request-tags+
						   :test #'eq-symbol)))
      (case valid-tag
		(:name
		 `(:name ,args-str))
		(:get
		 (if (zerop (length args-str))
             '(:error "no item specified")
			 `(:get ,args-str)))
		(:inventory
		 (if (zerop (length args-str))
			 '(:inventory nil)
			 `(:error ,(format nil "unexpected arguments: ~{~a, ~}"
							   args-str))))
		(:look
		 (if (zerop (length args-str))
			 '(:look nil)
			 `(:error ,(format nil "unexpected arguments: ~{~a, ~}"
							   args-str))))
		(otherwise
		 `(:error ,(format nil "bad request: ~a" request)))))))

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
			  (item (item-by-code-and-location content location)))		   
		 (cond
		   (in-inventory
			(format nil "error in-inventory"))
		   ((null item)
			(format nil "error not-found"))
		   (t
			(push item (getf state :inventory))
			(format nil "~a ~a" command
					(getf item :name))))))

	  (:inventory
	   (let* ((inventory (getf state :inventory))
			  (response (if (null inventory)
							"empty"
							(format nil "~{~{ ~a ~} ~}" inventory))))
		 (format nil "~a ~a" command response)))

	  (:look
	   (let ((location (getf state :location)))
		 (format nil "~a ~{~{~a ~} ~}" :location location)))
	  
	  (:error
	   content))))

(defun in-inventory (object-code state)
  (let* ((inventory (getf state :inventory))
		(match (find object-code inventory
					 :test #'eq-symbol)))
	(not (null match))))

(defun item-by-code-and-location (item-code location-name)
  (let* ((location (getf +objects-by-location+ location-name))
		 (items (getf location :objects))
		 (match (find item-code items
					  :test #'(lambda (item-code item)
								(eq-symbol item-code (getf item :code))))))
	match))

(defun object-in-location (object-code location-name)
  (not (null (item-by-code object-code location-name))))

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
		   (format t "~a:> " (or (getf state :name)
								""))
		   (let* ((command (read-line))
				  (parsed-command (parse-command state command))
				  (tag (first parsed-command))
				  (args (second parsed-command)))             
			 (case tag
			   (:error
				(format t "ERROR parsing command: ~a~%" args))
			   (:exit 
				(setf continue? nil))
			   (:help
				(format t "Available commands:~%~{~a~%~}" +command-tags+))
			   (otherwise				 
				(let ((request (create-request tag args)))
				  (if (eql request :error)
					  (format t "ERROR creating request: ~a~%" args)
					  (progn
						(format t "sending ~a~%" request)
						(send-over-connection socket (format nil "~a~%" request))
						(usocket:wait-for-input socket)
						(let* ((response (read-line stream))
							   (parsed (parse-response response)))                     
						  (setf state (next-client-state state parsed)))))))))))))

(defun message-from-state (state)  
  (format t "~a~%" state)
  (cond ((not (listp state))
		 "Whoops there's something off here. Please close and restart this session!")
		((or (null state)
			 (null (getf state :name)))         
		 (format nil "Welcome to Text Socket!~%Please enter your name:"))
		((not (null (getf state :get)))
		 (format nil "Picked up the ~a" (getf state :get)))
		((not (null (getf state :error)))
		 (getf state :error))
		(t
		 (format nil "~{~a ~%~}~%" state))))

(defun parse-command (state command)
  (if (null state)
	  ;; Always assume setting name
	  `(:name ,command)
	  (multiple-value-bind (tag-str args-str) (split-by-first-space command)
		(let ((tag (command-tag-from-string tag-str)))
		  (case tag
			(:exit
			 (if (zerop (length args-str))
				 '(:exit)
				 (no-args-expected-error args-str)))
			(:help
			 (if (zerop (length args-str))
				 '(:help)
				 (no-args-expected-error args-str)))
			(:inventory
			 (if (zerop (length args-str))
				 '(:inventory)
				 (no-args-expected-error args-str)))
			(:look
			 (if (zerop (length args-str))
				 '(:look)
				 (no-args-expected-error args-str)))
			(:get
			 (if (zerop (length args-str))
                 (args-expected-error)
				 `(:get ,args-str)))
			(otherwise
			 `(:error ,(format nil "Invalid command ~a" tag))))))))

(defun args-expected-error ()
  `(:error ,(format nil "Expected an argument")))

(defun no-args-expected-error (args-str)
  `(:error ,(format nil "No arguments expected. Received ~a" args-str)))

(defun command-tag-from-string (string)
  (find string +command-tags+ :test #'eq-symbol))

(defun create-request (command-tag args)  
  (case command-tag
	((:inventory :look)
	 (symbol-name command-tag))
	((:name :get)
	 (format nil "~a ~a"
			 (symbol-name command-tag)
			 args))
	(otherwise
	 :error)))

(defun parse-response (response)
  (multiple-value-bind (tag-str args) (split-by-first-space response)
	(let ((valid-tag (find tag-str +response-tags+
						   :test #'eq-symbol)))
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

(defun next-client-state (state parsed-response)
  (when (not (null parsed-response))
	(format t "parsed: ~a~%" parsed-response)
    (let ((subject (first parsed-response))
		  (payload (second parsed-response)))
	  (format t "subject: ~a~% payload: ~a~%" subject payload)
	  (if (equalp subject :name)
		  (list subject payload)
		  (list :name (getf state :name)
				subject payload)))))

(defun eq-symbol (string symbol)
  "Checks if STRING equals SYMBOL"
  (equalp string (symbol-name symbol)))

;; ~% very important for read-line
;; are defconstants appropriate here?
