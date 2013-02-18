;;;; macros.lisp --- Macro-based DSL for Jenkins jobs.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.dsl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-template-name (name)
    "TODO(jmoringe): document"
    (format-symbol #.*package* "+~A-TEMPLATE+" name)))

(defmacro define-template (name xml)
  "TODO(jmoringe): document"
  `(defvar ,(make-template-name name)
     (load-time-value (cxml:parse ,xml (stp:make-builder)) t)))

(define-template scm/git
  "<scm class=\"hudson.plugins.git.GitSCM\" plugin=\"git@1.1.26\">
    <configVersion>2</configVersion>
    <userRemoteConfigs>
      <hudson.plugins.git.UserRemoteConfig>
        <name/>
        <refspec/>
        <url>https://code.cor-lab.org/git/rsb.git.cpp</url>
      </hudson.plugins.git.UserRemoteConfig>
    </userRemoteConfigs>
    <branches>
      <hudson.plugins.git.BranchSpec>
        <name>remotes/origin/master</name>
      </hudson.plugins.git.BranchSpec>
    </branches>
    <disableSubmodules>false</disableSubmodules>
    <recursiveSubmodules>false</recursiveSubmodules>
    <doGenerateSubmoduleConfigurations>false</doGenerateSubmoduleConfigurations>
    <authorOrCommitter>false</authorOrCommitter>
    <clean>false</clean>
    <wipeOutWorkspace>true</wipeOutWorkspace>
    <pruneBranches>false</pruneBranches>
    <remotePoll>false</remotePoll>
    <ignoreNotifyCommit>false</ignoreNotifyCommit>
    <useShallowClone>false</useShallowClone>
    <buildChooser class=\"hudson.plugins.git.util.DefaultBuildChooser\"/>
    <gitTool>Default</gitTool>
    <submoduleCfg class=\"list\"/>
    <relativeTargetDir/>
    <reference/>
    <excludedRegions/>
    <excludedUsers/>
    <gitConfigName/>
    <gitConfigEmail/>
    <skipTag>true</skipTag>
    <includedRegions/>
    <scmName/>
  </scm>
")

(define-template job
  "<matrix-project>
  <description/>
  <executionStrategy class=\"hudson.matrix.DefaultMatrixExecutionStrategyImpl\">
    <runSequentially>false</runSequentially>
  </executionStrategy>
  <axes>
    <hudson.matrix.LabelAxis>
      <name>label</name>
      <values><string>default</string></values>
    </hudson.matrix.LabelAxis>
  </axes>
  <keepDependencies>false</keepDependencies>
  <canRoam>true</canRoam>
  <disabled>true</disabled>
  <blockBuildWhenDownstreamBuilding>true</blockBuildWhenDownstreamBuilding>
  <blockBuildWhenUpstreamBuilding>true</blockBuildWhenUpstreamBuilding>
  <actions/>
  <properties>
    <hudson.plugins.redmine.RedmineProjectProperty plugin=\"redmine@0.11\">
      <redmineWebsite>https://code.cor-lab.org/</redmineWebsite>
      <projectName>rsb</projectName>
    </hudson.plugins.redmine.RedmineProjectProperty>
    <hudson.security.AuthorizationMatrixProperty>
      <permission>hudson.model.Item.Read:anonymous</permission>
    </hudson.security.AuthorizationMatrixProperty>
  </properties>
  <logRotator>
    <daysToKeep>30</daysToKeep>
    <numToKeep>10</numToKeep>
    <artifactDaysToKeep>-1</artifactDaysToKeep>
    <artifactNumToKeep>5</artifactNumToKeep>
  </logRotator>
  <triggers class=\"vector\">
    <hudson.triggers.SCMTrigger>
      <spec>*/5 * * * *</spec>
      <ignorePostCommitHooks>false</ignorePostCommitHooks>
    </hudson.triggers.SCMTrigger>
  </triggers>
  <concurrentBuild>false</concurrentBuild>
  <builders/>
  <publishers>
    <hudson.plugins.warnings.WarningsPublisher plugin=\"warnings@4.19-CORLAB\">
      <healthy/>
      <unHealthy/>
      <!--thresholdLimit>
        low</thresholdLimit-->
      <pluginName>
        [WARNINGS]</pluginName>
      <defaultEncoding/>
      <canRunOnFailed>
        false</canRunOnFailed>
      <useStableBuildAsReference>
        false</useStableBuildAsReference>
      <useDeltaValues>
        false</useDeltaValues>
      <thresholds plugin=\"analysis-core@1.48\">
        <unstableTotalAll/>
        <unstableTotalHigh/>
        <unstableTotalNormal/>
        <unstableTotalLow/>
        <unstableNewAll/>
        <unstableNewHigh/>
        <unstableNewNormal/>
        <unstableNewLow/>
        <failedTotalAll/>
        <failedTotalHigh/>
        <failedTotalNormal/>
        <failedTotalLow/>
        <failedNewAll/>
        <failedNewHigh/>
        <failedNewNormal/>
        <failedNewLow/>
      </thresholds>
      <shouldDetectModules>
        false</shouldDetectModules>
      <dontComputeNew>
        false</dontComputeNew>
      <doNotResolveRelativePaths>
        false</doNotResolveRelativePaths>
      <parserConfigurations/>
      <consoleParsers/>
    </hudson.plugins.warnings.WarningsPublisher>
  </publishers>
  <buildWrappers>
    <hudson.plugins.build__timeout.BuildTimeoutWrapper plugin=\"build-timeout@1.11\">
      <timeoutMinutes>20</timeoutMinutes>
      <failBuild>false</failBuild>
      <writingDescription>false</writingDescription>
      <timeoutPercentage>0</timeoutPercentage>
      <timeoutType>absolute</timeoutType>
      <timeoutMinutesElasticDefault>3</timeoutMinutesElasticDefault>
    </hudson.plugins.build__timeout.BuildTimeoutWrapper>
    <hudson.plugins.setenv.SetEnvBuildWrapper plugin=\"setenv@1.1\"/>
  </buildWrappers>
</matrix-project>")

(define-template copy-artifact
  "<hudson.plugins.copyartifact.CopyArtifact plugin=\"copyartifact@1.25\">
      <selector class=\"hudson.plugins.copyartifact.StatusBuildSelector\"/>
    </hudson.plugins.copyartifact.CopyArtifact>")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-accessor (class name)
    "TODO(jmoringe): document"
    (or (when-let ((name (find-symbol (string name) :jenkins.api)))
	  (when (closer-mop:compute-applicable-methods-using-classes
		 (fdefinition name) (list (find-class class)))
	    name))
	(error "~@<Unknown ~S attribute: ~S.~@:>"
	       class name))))

(defmacro job ((kind name &rest attributes &key &allow-other-keys)
	       &body body)
  "TODO(jmoringe): document"
  ;;; TODO(jmoringe, 2012-12-12): kind
  (let+ ((class-name    'jenkins.api:job)
	 (template-name (make-template-name '#:job))
	 ((&with-gensyms node))
	 ((&flet process-attribute (name value)
	    (check-type name symbol)

	    (let ((accessor-name (find-accessor class-name name)))
	      `(setf (,accessor-name ,node) ,value))))
	 ((&flet+ process-child ((name &rest specs))
	    (check-type name symbol)

	    (let ((accessor-name (find-accessor class-name name)))
	      `(setf (,accessor-name ,node) (list ,@specs))))))
   `(let ((,node (make-instance ',class-name
			       :id   ,name
			       :data (stp:copy ,template-name))))
      (xloc:xml-> (stp:root ,template-name) ,node)
      ,@(iter (for (name value) on attributes :by #'cddr)
	      (collect (process-attribute name value)))
      ,@(mapcar #'process-child body)
      ;; TODO temp: synchronize
      (xloc:->xml ,node (stp:root (jenkins.api::%data ,node)) ',class-name)
      ,node)))


(defmacro define-model-class-macro ((name
				     &key
				     (class-name name)))
  "Define a macro named NAME which accepts attributes as a plist and
returns an instance of the model class CLASS-NAME."
  `(progn
     (defmacro ,name ((&rest attributes &key &allow-other-keys))
       "TODO(jmoringe): document"
       ;; TODO(jmoringe, 2012-12-12): kind
       (let+ (((&with-gensyms node))
	      ((&flet process-attribute (name value)
		 (check-type name symbol)

		 (let ((accessor-name (find-accessor ',class-name name)))
		   `(setf (,accessor-name ,node) ,value)))))
	 `(let ((,node (make-instance ',',class-name)))
	    ,@(iter (for (name value) on attributes :by #'cddr)
		    (collect (process-attribute name value)))
	    ,node)))
     (export ',name)))

(defmacro define-interface-implementation-macros (interface)
  (let+ ((map-name        (format-symbol
			   (symbol-package interface)
			   "*CLASS->~A-NAME*" interface))
	 (implementations (hash-table-keys
			   (symbol-value map-name)))
	 ((&flet process-implementation (class-name)
	    (let+ (((&ign implementation)
		    (split-sequence #\/ (string class-name)))
		   (name (intern implementation)))
	      (if (find-symbol (string implementation) :cl)
		  (warn 'simple-style-warning
			:format-control "~@<Implementation ~S of ~
interface ~S would clash with ~S is in ~S package; skipping.~@:>"
			:format-arguments (list class-name interface implementation :cl))
		  `(define-model-class-macro
		       (,name
			:class-name ,class-name)))))))
    `(progn
       ,@(mapcar #'process-implementation implementations))))

(define-interface-implementation-macros jenkins.api::scm)
(define-interface-implementation-macros jenkins.api::builder)
