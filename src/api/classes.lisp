;;;; classes.lisp --- Classes used by the api module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(defclass standard-model-object ()
  ((id       :initarg  :id
	     :accessor id
	     :documentation
	     "")
   (data     :initarg  :data
	     :accessor %data
	     :documentation
	     "")
   (get-func :initarg  :get-func
	     :type     function
	     :accessor get-func
	     :documentation
	     "")
   (put-func :initarg  :put-func
	     :type     (or null function)
	     :accessor put-func
	     :initform nil
	     :documentation
	     ""))
  (:default-initargs
   :id       (missing-required-initarg 'standard-model-object :id)
   :get-func (missing-required-initarg 'standard-model-object :get-func)
   #+no :put-func #+no (missing-required-initarg 'standard-model-object :put-func))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod slot-unbound :around ((class     t)
				 (instance  standard-model-object)
				 (slot-name t))
  (setf (%data instance) nil)
  (update! instance)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      (call-next-method)))

(defmethod (setf closer-mop:slot-value-using-class) :around ((new-value t)
							     (class     t)
							     (instance  standard-model-object)
							     (slot      t))
  (when (and (not (member (closer-mop:slot-definition-name slot) '(id data get-func put-func)))
	     (not (slot-boundp instance 'data)))
    (update! instance))
  (call-next-method))

(defmacro define-model-class (name () (&rest slots) &body options)
  "TODO(jmoringe): document"
  (let+ (((&flet+ make-slot-spec ((name
				   &rest options
				   &key
				   (initarg   (make-keyword name))
				   (type      t)
				   xpath
				   (optional? t)
				   (accessor  name)
				   &allow-other-keys))
	    `(,name
	      :initarg  ,initarg
	      :type     ,(cond
			   ((eq (getf (rest (ensure-list xpath)) :if-multiple-matches) :all)
			    'list)
			   ((typep type '(cons (eql list)))
			    'list)
			   (optional?
			    `(or null ,type))
			   (t
			    type))
	      :accessor ,accessor
	      ,@(remove-from-plist options :type :xpath :optional?))))

	 ((&flet+ make-xml->slot ((name
				   &key
				   (type      t)
				   (xpath     (format (cdr '(1)) "./~(~A~)/text()" name))
				   (optional? t)
				   &allow-other-keys))
	    `(let ((loc (xloc:loc value ,@(ensure-list xpath)
				  ,@(when optional?
				      '(:if-no-match :do-nothing)))))
	       (setf (slot-value type ',name)
		     (xloc:val loc :type ',type)))))

	 ((&flet+ make-slot->xml ((name
				   &key
				   (type      t)
				   (xpath     (format (cdr '(1)) "./~(~A~)/text()" name))
				   (optional? t)
				   &allow-other-keys))
	    `(let ((loc (xloc:loc dest ,@(ensure-list xpath)
				  :if-no-match (if (and ,optional? (not (slot-value value ',name)))
						   :do-nothing
						   :create))))
	       (when (xloc:location-result loc)
		 (setf (xloc:val loc :type ',type)
		       (slot-value value ',name))))))

	 (name-slot (second (or (find :name-slot options :key #'first)
				'(:name-slot id))))
	 (get-func  (second (find :get-func options :key #'first)))
	 (put-func  (second (find :put-func options :key #'first)))
	 (root?     (or get-func put-func)))
    `(progn
       (defclass ,name (,@(when root? '(standard-model-object)))
	 (,@(mapcar #'make-slot-spec slots))
	 (:default-initargs
	  ,@(append
	     (when get-func
	       (list :get-func get-func))
	     (when put-func
	       (list :put-func put-func))
	     (second (find :default-initargs options :key #'first))))
	 ,@(remove-if (lambda (key)
			(member key '(:name-slot :get-func :put-func
				      :default-initargs)))
		      options :key #'first))

       (defmethod ,name ((id t) &rest initargs &key &allow-other-keys)
	 (apply #'make-instance ',name :id id initargs))

       (defmethod xloc:xml-> ((value stp:element)
			      (type  ,name)
			      &key &allow-other-keys)
	 ,@(mapcar #'make-xml->slot slots)
	 type)

       (defmethod xloc:->xml ((value ,name)
			      (dest  stp:element)
			      (type  (eql ',name))
			      &key &allow-other-keys)
	 ,@(mapcar #'make-slot->xml slots)
	 dest)

       ,@(when root?
	   `((defmethod update! ((object ,name))
	       "TODO(jmoringe): document"
	       (let+ (((&accessors id (data %data) get-func) object))
		 (setf data (funcall get-func id))
		 (xloc:xml-> (stp:root data) object)))

	     (defmethod commit! ((object ,name))
	       "TODO(jmoringe): document"
	       (let+ (((&accessors-r/o id (data %data) put-func) object))
		 (unless put-func
		   (error "~@<Read-only object ~A.~@:>" object))
		 (xloc:->xml object (stp:root data) ',name)
		 (funcall put-func id data))
	       object)))

       ,@(when name-slot
	   `((defmethod print-object ((object ,name) stream)
	       (print-unreadable-object (object stream :type t :identity t)
		 (let* ((value (,name-slot object))
			(end   (when (stringp value)
				 (or (position #\Newline value) (length value)))))
		   (princ (if end (subseq value 0 end) value) stream)))))))))

(defmacro define-interface-implementations ((name
					     &key
					     (class-location '(xloc:name ".")))
					    &body implementations)
  "TODO(jmoringe): document"
  (let+ (((class-accessor class-path) class-location)
	 (name->class-table (format-symbol *package* "*NAME->~A-CLASS*" name))
	 (class->name-table (format-symbol *package* "*CLASS->~A-NAME*" name))
	 ((&flet+ make-implementation (((key class &key plugin) (&rest slots) &body options))
	    (let ((class-name (format-symbol *package* "~A/~A" name key)))
	     `((setf (gethash ,class       ,name->class-table) ',class-name
		     (gethash ',class-name ,class->name-table) ,class)

	       (define-model-class ,class-name ()
		   (,@slots
		    ,@(when plugin
			`((%plugin :type     string
				   :xpath    "@plugin"
				   :initform ,plugin))))
		 ,@options))))))
   `(progn
      (defvar ,name->class-table (make-hash-table :test #'equal))
      (defvar ,class->name-table (make-hash-table :test #'eq))

      (defmethod xloc:xml-> ((value stp:element)
			     (type  (eql ',name))
			     &key &allow-other-keys)
	,(format nil "Lookup the name of the ~S implementation and ~
                      convert VALUE to an instance of that type."
		 name)
	;; Try to look up the implementation class for the
	;; implementation name stored in VALUE. If the class cannot be
	;; found, signal an `unmapped-class' condition and return a
	;; marker object.
	(let ((name (,class-accessor (xloc:loc value ,class-path))))
	  (if-let ((class-name (gethash name ,name->class-table)))
	    (xloc:xml-> value class-name)
	    (progn
	      (signal 'unmapped-class
		      :interface ',name
		      :name      name)
	      (list :unimplemented ',name name value)))))

      (defmethod xloc:->xml ((value t)
			     (dest  stp:element)
			     (type  (eql ',name))
			     &key &allow-other-keys)
	,(format nil "Store the ~S instance VALUE in DEST." name)
	(let* ((class-name (class-name (class-of value)))
	       (name       (gethash class-name ,class->name-table)))
	  (unless name
	    (error ,(format nil "~~@<~~A is not a valid ~S
class. Valid ~:*~S classes are ~~{~~S~~^, ~~}.~~@:>"
			    name)
		   value (hash-table-keys ,class->name-table)))
	  (setf (,class-accessor (xloc:loc dest ,class-path :if-no-match :create)) name)
	  (xloc:->xml value dest class-name)))

      (defmethod xloc:->xml ((value list)
			     (dest  stp:element)
			     (type  (eql ',name))
			     &key &allow-other-keys)
	"This helper method ensures that XML substree DEST is still in
sync with the XML substree stored in the unmapped implementation
marker VALUE."
	(check-type value unmapped-marker)
	(assert (eq dest (fourth value))))

      ,@(mappend #'make-implementation implementations))))


;;; `node' class
;;

(define-model-class node ()
  ((name        :type      string)
   (description :type      string)
   (host        :type      string
		:xpath     "launcher/host/text()")
   (mode        :type      keyword)
   (label       :type      (list/space string)
		:xpath     "label/text()")
   (environment :type      tree-map/plist
		:xpath     "/slave/nodeProperties/hudson.slaves.EnvironmentVariablesNodeProperty/envVars/tree-map"))
  (:get-func (lambda (id)      (node-config id)))
  (:put-func (lambda (id data) (setf (node-config id) data))))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~:[off~;on~]line"
	    (name object) (online? object))))


;;; `job' class
;;
;; Aggregated classes:
;;
;; * `scm'
;; * `trigger'
;; * `builder'
;; * `publisher'

(define-interface-implementations (scm
				   :class-location (xloc:val "@class"))
  ((svn "hudson.scm.SubversionSCM"
	:plugin "subversion@1.43")
   ((url :type  string
	 :xpath "locations/hudson.scm.SubversionSCM_-ModuleLocation/remote/text()"))
   (:name-slot url))

  ((git "hudson.plugins.git.GitSCM"
        :plugin "git@1.1.26")
   ((url                  :type  string
			  :xpath "userRemoteConfigs/hudson.plugins.git.UserRemoteConfig/url/text()")
    (branches             :type  string
			  :xpath ("branches/hudson.plugins.git.BranchSpec/name/text()"
				  :if-multiple-matches :all))
    (wipe-out-workspace?  :type  boolean
			  :xpath "wipeOutWorkspace/text()")
    (checkout-submodules? :type  boolean
			  :xpath "recursiveSubmodules/text()")
    (skip-internal-tag?   :type  boolean
			  :xpath "skipTag/text()"))
   (:name-slot url))

  ((bzr "hudson.plugins.bazaar.BazaarSCM")
   ((url :type  string
	 :xpath "source/text()"))
   (:name-slot url))

  ((null "hudson.scm.NullSCM")
   ()
   (:name-slot nil)))

(define-interface-implementations (trigger)
  ((scm "hudson.triggers.SCMTrigger")
   ((spec :type string))
   (:name-slot spec))

  ((timer "hudson.triggers.TimerTrigger")
   ((spec :type string))
   (:name-slot spec))

  ((github "com.cloudbees.jenkins.GitHubPushTrigger"
	   :plugin "github@1.4")
   ((spec :type string))
   (:name-slot spec)))

(define-interface-implementations (builder)
  ((shell "hudson.tasks.Shell")
   ((command :type  string))
   (:name-slot command))

  ((batch "hudson.tasks.BatchFile")
   ((command :type  string))
   (:name-slot command))

  ((cmake "hudson.plugins.cmake.CmakeBuilder")
   ((command :type  string
	     :xpath "makeCommand/text()"))
   (:name-slot command))

  ((ant "hudson.tasks.Ant"
	:plugin "ant@1.1")
   ((targets    :type  string)
    (properties :type  string))
   (:name-slot targets))

  ((maven "hudson.tasks.Maven")
   ((targets             :type  (list/space string))
    (properties          :type  (equals+newline/plist keyword string))
    (private-repository? :type  boolean
			 :xpath "usePrivateRepository/text()"))
   (:name-slot targets))

  ((copy-artifact "hudson.plugins.copyartifact.CopyArtifact"
		  :plugin "copyartifact@1.25")
   ((project-name :type  string
		  :xpath "projectName/text()") ;;; TODO(jmoringe, 2012-07-10): camelcase
    (filter       :type  string)
    (target       :type  string)
    (flatten?     :type  boolean
		  :xpath "flatten/text()")
    ;;; TODO(jmoringe, 2012-12-13): temp
    (clazz        :type  string
		  :xpath "selector/@class"))
   (:name-slot project-name)))

(define-interface-implementations (publisher)
  ((ssh      "jenkins.plugins.publish__over__ssh.BapSshPublisherPlugin")
   ()
   (:name-slot nil))

  ((warnings "hudson.plugins.warnings.WarningsPublisher")
   ()
   (:name-slot nil))

  ((tasks "hudson.plugins.tasks.TasksPublisher"
	  :plugin "tasks@4.35")
   ((threshold-limit :type   keyword/downcase
		     :xpath  "thresholdLimit/text()")
    (keywords/low    :type   (list/comma string)
		     :xpath  "low/text()")
    (keywords/normal :type   (list/comma string)
		     :xpath  "normal/text()")
    (keywords/high   :type   (list/comma string)
		     :xpath  "high/text()"))
   (:name-slot nil))

  ((archive-artifacts "hudson.tasks.ArtifactArchiver")
   ((files         :type     (list/comma string)
		   :xpath    "artifacts/text()")
    (only-latests? :type boolean
		   :xpath    "onlyLatest/text()"))
   (:name-slot nil))

  ((fingerprint "hudson.tasks.Fingerprinter")
   ((targets          :type      (list/comma string))
    (build-artifacts? :type      boolean
		      :xpath     "recordBuildArtifacts/text()"))
   (:name-slot targets))

  ((sloccount "hudson.plugins.sloccount.SloccountPublisher"
	      :plugin "sloccount@1.8")
   ((pattern :type string))
   (:name-slot pattern))

  ((junit "hudson.tasks.junit.JUnitResultArchiver")
   ((result-files     :type      string
		      :xpath     "testResults/text()")
    (keep-long-stdio? :type      boolean
		      :xpath     "keepLongStdio/text()"))
   (:name-slot result-files))

  ((cobertura "hudson.plugins.cobertura.CoberturaPublisher"
	      :plugin "cobertura@1.7.1")
   ((report-file :type  string
		 :xpath "coberturaReportFile/text()"))
   (:name-slot report-file))

  ((html "htmlpublisher.HtmlPublisher"
	 :plugin "htmlpublisher@1.2")
   ()
   (:name-slot nil)))

(define-model-class job ()
    ((description     :type     string)
     (keep/days       :type     (or (eql -1) non-negative-integer)
		      :xpath    "logRotator/daysToKeep/text()")
     (keep/count      :type     (or (eql -1) non-negative-integer)
		      :xpath    "logRotator/numToKeep/text()")
     (permissions     :type     string/node
		      :xpath    ("properties/hudson.security.AuthorizationMatrixProperty/permission"
				 :if-multiple-matches :all))
     (redmine-url     :type     string
		      :xpath    "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineWebsite/text()")
     (redmine-name    :type     string
		      :xpath    "properties/hudson.plugins.redmine.RedmineProjectProperty/projectName/text()")
     (children        :type     (list/comma string)
		      :xpath    "publishers/hudson.tasks.BuildTrigger/childProjects/text()")
     (repositories    :type     scm
		      :xpath    ("scm"
				 :if-multiple-matches :all)
		      :optional? t)
     (triggers        :type     trigger
		      :xpath    ("triggers/*"
				 :if-multiple-matches :all))
     (environment     :type     (equals+newline/plist keyword string)
		      :xpath    "buildWrappers/hudson.plugins.setenv.SetEnvBuildWrapper/localVarText/text()")
     (builders        :type     builder
		      :xpath    ("builders/*"
				 :if-multiple-matches :all))
     (publishers      :type     publisher
		      :xpath    ("publishers/*"
				 :if-multiple-matches :all))
     (warning-parsers :type     string/node
		      :xpath    ("publishers/hudson.plugins.warnings.WarningsPublisher/consoleParsers/hudson.plugins.warnings.ConsoleParser/parserName"
				 :if-multiple-matches :all)) ;;; TODO(jmoringe, 2012-07-10): not correct
     (slaves          :type     string/node
		      :xpath    ("axes/hudson.matrix.LabelAxis[name/text()='label']/values/string"
				 :if-multiple-matches :all)
		      :optional? t)
     (archive-files   :type     (list/comma string)
		      :xpath    "publishers/hudson.tasks.ArtifactArchiver/artifacts/text()")
     (archive-only-latests? :type boolean
			    :xpath    "publishers/hudson.tasks.ArtifactArchiver/onlyLatest/text()")
     (upload-target   :type     string
		      :xpath    "publishers/jenkins.plugins.publish__over__ssh.BapSshPublisherPlugin/delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/configName/text()"
		       :optional? t)
     (upload-directory :type    string
		       :xpath   "publishers/jenkins.plugins.publish__over__ssh.BapSshPublisherPlugin/delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/remoteDirectory/text()"
		       :optional? t))
  (:get-func (lambda (id)      (job-config id)))
  (:put-func (lambda (id data) (setf (job-config id) data))))

(defmethod kind ((object job))
  "TODO(jmoringe): document"
  (stp:local-name (stp:document-element (%data object))))

(defmethod (setf kind) ((new-value string)
			(object    job))
  "TODO(jmoringe): document"
  (unless (member new-value '("project" "matrix-project")
		  :test #'string=)
    (cerror "Continue" "~@<Unknown job kind ~S.~@:>"
	    new-value))
  (setf (stp:local-name (stp:document-element (%data object)))
	new-value))

(defmethod upstream ((object job))
  "TODO(jmoringe): document"
  (iter:iter (iter:for job iter:in (all-jobs/cache))
	     (when (find (id object) (children job) :test #'string=)
	       (iter:collect (id job)))))

(defmethod grant ((job job) (subject string) (action string))
  (pushnew (format nil "~A:~A" action subject)
	   (permissions job)
	   :test #'string=)
  (permissions job))

(defmethod revoke ((job job) (subject string) (action string))
  (removef (permissions job)
	   (format nil "~A:~A" action subject)
	   :test #'string=)
  (permissions job))

(macrolet
    ((define-permission-methods (name)
       `(progn
	  (defmethod ,name ((job string) (subject t) (action t))
	    (,name (job job) subject action))

	  (defmethod ,name ((job job) (subject list) (action t))
	    (mapc #'(lambda (subject) (,name job subject action)) subject)
	    (permissions job))

	  (defmethod ,name ((job job) (subject t) (action list))
	    (,name job subject (format nil "hudson.model.~{~@(~A~)~^.~}" action))))))

  (define-permission-methods grant)
  (define-permission-methods revoke))


;;; `build' class
;;

(define-model-class build ()
    ((building?  :type  boolean
		 :xpath "building/text()")
     (slave-name :type  string
		 :xpath "builtOn/text()")
     (result     :type  keyword
		 :xpath "result/text()"))
  (:get-func (lambda (id) (build-config id))))

(defmethod job ((build build) &key &allow-other-keys)
  (job (first (split-sequence #\/ (id build)))))

(defmethod slave ((build build) &key &allow-other-keys)
  (node (slave-name build)))

(defmethod failed? ((build build))
  (eq (result build) :failure))

(defmethod print-object ((object build) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
	    (id object) (if (building? object)
			    :in-progress
			    (result object)))))


;;; `item' class (Queue items)
;;

(define-model-class item ()
    ((job-name :type  string
	       :xpath "task/name/text()"))
  (:get-func (lambda (id) (item-config id))))

(defmethod job ((item item) &key &allow-other-keys)
  (job (job-name item)))


;;; `view' class
;;

(define-model-class view ()
    ((jobs :type  string
	   :xpath ("job/name/text()"
		   :if-multiple-matches :all)))
  (:get-func (lambda (id)      (view-config id)))
  (:put-func (lambda (id data) (setf (view-config id) data))))
