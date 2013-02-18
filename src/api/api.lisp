;;;; api.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen;;
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-biel;;efeld.de>

(cl:in-package #:jenkins.api)

(defmacro define-operation ((name &key path) (&rest args)
			    &body body)
  "TODO(jmoringe): document"
  `(defun ,name ,args
     (flet ((request (&rest args-and-parameters
		      &key
		      method
		      content-type
		      content
		      &allow-other-keys)
	      (let+ ((parameters (alexandria:remove-from-plist
				 args-and-parameters
				 :method :content-type :content))
		     ((&whole result body code &rest &ign)
		      (multiple-value-list
		       (apply #'drakma:http-request
			      (puri:merge-uris
			       (make-instance 'puri:uri
					      :path    ,path
					      :escaped t)
			       *base-url*)
			      :parameters          (loop for (key value) on parameters :by #'cddr
							 collect (cons (let ((*readtable* (copy-readtable)))
									 (setf (readtable-case *readtable*) :invert)
									 (format nil "~A" key))
								       (princ-to-string value)))

			      (append
			       (when (and *username* *password*)
				 (list :basic-authorization (list *username* *password*)))
			       (when method
				 (list :method method))
			       (when content-type
				 (list :content-type content-type))
			       (when content
				 (list :content content)))))))
		(unless (<= 200 code 399)
		  (error "~@<Request failed (code ~D):~_~A~@:>"
			 code body))
		(values-list result))))
       ,@body)))

(defmacro define-operation/json ((name &key path) (&rest args)
				 &body body)
  `(define-operation (,name :path ,path) ,args
     (labels ((request/json (&rest args
			     &key
			     (depth 1)
			     &allow-other-keys)
		(json:decode-json-from-string
		 (sb-ext:octets-to-string
		  (apply #'request :depth depth
			 (remove-from-plist args :depth)))))
	      (field (structure spec)
		(cdr (or (assoc spec structure)
			 (error "~@<No such field: ~S~@:>" spec)))))
       (declare (ignorable #'field))
       ,@body)))

(defmacro define-operation/xml ((name &key path) (&rest args)
				&body body)
  `(define-operation (,name :path ,path) ,args
     (flet ((request/xml (&rest args &key &allow-other-keys)
	      (cxml:parse (apply #'request args) (stp:make-builder))))
       ,@body)))

(defmacro define-operation/name-or-object ((name
					    &key
					    (operation-definer 'define-operation)
					    path)
					   (&rest args)
					   &body body)
  (let+ ((((object-name object-class) &rest other-args) args)
	 ((&values required optional rest keyword)
	  (parse-ordinary-lambda-list other-args))
	 (args-for-apply (append required
				 (mapcar #'first optional)
				 (mappend #'first keyword)
				 (list rest)))
	 (name/private (format-symbol *package* "%~A/~A" name object-class)))
    `(progn
       (,operation-definer (,name/private :path ,path) (,object-name ,@other-args)
	  ,@body)

       (defmethod ,name ((,object-name string) ,@other-args)
	 (apply #',name/private ,object-name ,@args-for-apply))

       (defmethod ,name ((,object-name ,object-class) ,@other-args)
	 (apply #',name/private (id ,object-name) ,@args-for-apply)))))

(defmacro define-items (name &body options)
  (let+ (((&plist-r/o
	   (prefix      :prefix)
	   (id          :id          #+no (missing-required-argument :id))

	   (all-name    :all-name    (format-symbol *package* "ALL-~AS" name))
	   (all-path    :all-path    (format nil "~@[~A/~]api/json" prefix))
	   (all-filter  :all-filter  (format nil "~(~A~)[~A]" prefix id))
	   (all-field   :all-field   (make-keyword
				      (if prefix
					  (json:camel-case-to-lisp prefix)
					  (format nil "~AS" name))))

	   (get-name    :get-name    (format-symbol *package* "~A/JSON" name))
	   (get-path    :get-path    `(format nil "~@[~A/~]~A/api/json" ,prefix name))

	   (config?     :config?     t)

	   (config-name :config-name (format-symbol *package* "~A-CONFIG" name))
	   (config-path :config-path `(format nil "~@[~A/~]~A/config.xml" ,prefix name))

	   (setf-config-name :setf-config-name `(setf ,config-name))

	   (make-name   :make-name   (format-symbol *package* "MAKE-~A" name))

	   (copy-name   :copy-name   (format-symbol *package* "COPY-~A" name))

	   (delete-name :delete-name (format-symbol *package* "DELETE-~A" name))
	   (delete-path :delete-path `(format nil "~@[~A/~]~A/doDelete" ,prefix name))

	   (rename-name :rename-name (format-symbol *package* "RENAME-~A" name))
	   (rename-path :rename-path `(format nil "~@[~A/~]~A/doRename" ,prefix source-name)))
	  (apply #'append options)))
    `(progn
       (define-operation/json (,all-name :path ,all-path) (&optional regex)
	 (let ((names (mapcar (rcurry #'field ,(make-keyword (json:camel-case-to-lisp id)))
			      ,(if all-field
				   `(field (request/json :tree ,all-filter) ,all-field)
				   `(request/json :tree ,all-filter)))))
	   (mapcar
	    #',name
	    (if regex
		(remove-if (complement (curry #'ppcre:scan regex)) names)
		names))))

       (define-operation/json (,get-name :path ,get-path) (name &key (depth 1))
	 (request/json :depth depth))

       ,@(when config?
	   `((define-operation/xml (,config-name :path ,config-path) (name)
	       (request/xml))

	     (define-operation (,setf-config-name :path ,config-path) (config name)
	       (request :name         name
			:content-type "text/xml"
			:content      (stp:serialize config (cxml:make-octet-vector-sink))
			:method       :post))

	     (define-operation (,make-name :path "createItem") (name config)
	       (request :name         name
			:content-type "text/xml"
			:content      (stp:serialize config (cxml:make-octet-vector-sink))
			:method       :post))))

       (define-operation (,copy-name :path "createItem") (source-name new-name)
	 (request :name   new-name
		  :mode   "copy"
		  :from   source-name
		  :method :post)
	 ,(if config?
	      `(,name new-name)
	      t))

       (define-operation (,rename-name :path ,rename-path) (source-name new-name)
	 ;; Some objects cannot be renamed while they are "busy" in
	 ;; some sense. Retry until renaming becomes possible.
	 (iter:iter
	   (let+ (((&values &ign &ign props) (request :|newName| new-name
						      :method    :post)))
	     (iter:while (ppcre:scan "rename\\?newName" (cdr (assoc :location props))))
	     (sleep 1)))
	 ,(if config?
	      `(,name new-name)
	      t))

       (define-operation/name-or-object (,delete-name :path ,delete-path)
	   ((name ,name))
	 (request :method :post)
	 (values)))))


;;; Node-related operations
;;

(define-items node
  (:prefix "computer")
  (:id     "displayName"))

(defmethod online? ((node string))
  (not (cdr (assoc :offline (node/json node)))))

(defmethod online? ((node node))
  (online? (id node)))

(define-operation/name-or-object
    (mark-online! :path (format nil "computer/~A/toggleOffline" node))
    ((node node) &key (if-online #'error))
  (if (online? node)
      (etypecase if-online
	(null     nil)
	(function (funcall if-online
			   (make-condition 'simple-error
					   :format-control   "~@<Node ~A is already online.~@:>"
					   :format-arguments (list node)))))
      (request :method :post))
  (values))

(define-operation/name-or-object
    (mark-offline! :path (format nil "computer/~A/toggleOffline" node))
    ((node node) &key (if-offline #'error))
  (if (online? node)
      (request :method :post)
      (etypecase if-offline
	(null     nil)
	(function (funcall if-offline
			   (make-condition 'simple-error
					   :format-control   "~@<Node ~A is already offline.~@:>"
					   :format-arguments (list node))))))
  (values))


;;; Job-related operations
;;

(define-items job
  (:prefix     "job")
  (:id         "name")

  (:all-path   "api/json")
  (:all-filter "jobs[name]")
  (:all-field  :jobs))

(defun copy-job/fixup (old new)
  ;; Create the new job and disabled it immediately. This is racy. We
  ;; could disable OLD before copying, but I would rather not touch
  ;; the old job in case something goes wrong.
  (let ((new/job (copy-job old new)))
    ;; Disable the new job to prevent it from building while we are
    ;; still setting it up.
    (disable! new/job)

    ;; Install OLD's upstream jobs as NEW's upstream jobs.
    (dolist (upstream (mapcar #'job (upstream (job old))))
      (format *trace-output* "Adjusting relation to upstream job ~A" upstream)
      (push new (children upstream))
      (commit! upstream))

    ;; Remove any downstream jobs NEW might have copied from OLD.
    (setf (children new/job) nil)
    (commit! new/job)))

(define-operation/name-or-object
    (build! :path (format nil "job/~A/build" job))
    ((job job))
  (request :method :post)
  (values))

(define-operation/name-or-object
    (enable! :path (format nil "job/~A/enable" job))
    ((job job))
  (request :method :post)
  (values))

(define-operation/name-or-object
    (disable! :path (format nil "job/~A/disable" job))
    ((job job))
  (request :method :post)
  (values))

(defmethod relate ((parent string) (child string))
  (relate (job parent) (job child)))

(defmethod relate ((parent string) (child job))
  (relate (job parent) child))

(defmethod relate ((parent job) (child string))
  (relate parent (job child)))

(defmethod relate ((parent job) (child job))
  (when (member (id child) (children parent) :test #'string=)
    (error "~@<~A already is a downstream project of ~A~@:>"
	   child parent))
  (push (id child) (children parent))
  (commit! parent))

(defmethod unrelate ((parent string) (child string))
  (unrelate (job parent) (job child)))

(defmethod unrelate ((parent string) (child job))
  (unrelate (job parent) child))

(defmethod unrelate ((parent job) (child string))
  (unrelate parent (job child)))

(defmethod unrelate ((parent job) (child job))
  (unless (member (id child) (children parent) :test #'string=)
    (error "~@<~A is not a downstream project of ~A~@:>"
	   child parent))
  (removef (children parent) (id child) :test #'string=)
  (commit! parent))

(defmethod last-build ((job job))
  "TODO(jmoringe): document"
  ()
  )

(defvar *job-cache* nil
  "TODO(jmoringe): document")

(defun all-jobs/cache (&optional regex)
  (let ((jobs (or *job-cache*
		  (setf *job-cache* (all-jobs)))))
    (if regex
	(remove-if (complement (curry #'ppcre:scan regex)) jobs
		   :key #'id)
	jobs)))


;;; build
;;

(define-operation/json (all-builds :path "/api/json")
    (&optional regex)
  (let* ((jobs (field (request/json :tree "jobs[name,builds[number]]") :jobs))
	 (names
	   (mapcan (lambda (job)
		     (map-product (lambda (name number)
				    (format nil "~a/~a" name number))
				  (list (field job :name))
				  (mapcar (rcurry #'field :number) (field job :builds))))
		   jobs)))
    (mapcar #'build
	    (if regex
		(remove-if (complement (curry #'cl-ppcre:scan regex)) names)
		names))))

(define-operation/json (last-builds :path "/api/json")
    (&optional regex)
  (let* ((jobs (field (request/json :tree "jobs[name,lastBuild[number]]") :jobs))
	 (names
	   (mapcan (lambda (job)
		     (when-let ((last-build (field job :last-build)))
		       (list (format nil "~a/~a"
				     (field job :name)
				     (field last-build :number)))))
		   jobs)))
    (mapcar #'build
	    (if regex
		(remove-if (complement (curry #'cl-ppcre:scan regex)) names)
		names))))

(define-operation/json (build/json :path (format nil "job/~A/api/json" name))
    (name &key (depth 1))
  (request/json :depth depth))

(define-operation/xml (build-config :path (format nil "job/~A/api/xml" name))
    (name)
  (request/xml))

(define-operation/name-or-object
    (stop :path (format nil "job/~a/stop" build))
    ((build build))
  (request :method :post)
  (values))


;;; item (Queue item)
;;

(define-items item
  (:prefix      "queue")
  (:id          "id")

  (:all-filter  "items[id]")
  (:all-field   :items)

  (:config-path (format nil "queue/~A/api/xml" name)))

(define-operation/name-or-object
    (cancel :path (format nil "queue/item/~A/cancelQueue" item))
    ((item item))
  (request :method :post)
  (values))


;;; View-related operations
;;

(define-items view
  (:prefix      "view")
  (:id          "name")

  (:all-path    "api/json")
  (:all-filter  "views[name]")
  (:all-field   :views)

  (:config-path (format nil "view/~A/api/xml" name)))
