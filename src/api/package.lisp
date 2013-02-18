;;;; package.lisp --- Package definition for api module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.api
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions)

  ;; Variables
  (:export
   #:*base-url*
   #:*username*
   #:*password*)

  ;; API
  (:export
   #:id
   #:commit!)

  ;; Node
  (:export
   #:node

   #:node
   #:all-nodes
   #:node-config
   #:make-node
   #:delete-node

   #:name
   #:description
   #:host
   #:mode
   #:label
   #:environment

   #:online?
   #:mark-online!
   #:mark-offline!)

  ;; Job
  (:export
   #:job

   #:job ;; TODO find-job?
   #:all-jobs ;; TODO jobs?
   #:job-config
   #:make-job
   #:delete-job

   #:kind ; matrix or regular
   #:description
   #:upstream
   #:children
   #:repositories
   #:environment
   #:builders

   #:keep/days
   #:keep/count
   #:slaves
   #:warning-parsers ; not sure about this one

   #:copy-job/fixup
   #:build!
   #:enable!
   #:disable!

   #:relate
   #:unrelate)

  ;; SCM
  (:export
   #:scm/git
   #:url
   #:branches
   #:wipe-out-workspace?
   #:checkout-submodules?
   #:skip-internal-tag?

   #:scm/svn
   #:url

   #:scm/bzr
   #:url)

  ;; Trigger
  (:export
   #:trigger/scm
   #:spec

   #:trigger/timer
   #:spec)

  ;; Builders
  (:export
   #:builder/shell
   #:command

   #:builder/batch
   #:command

   #:builder/cmake
   #:command

   #:builder/ant
   #:targets
   #:properties

   #:builder/maven
   #:targets
   #:properties
   #:private-repository?

   #:builder/copy-artifact
   #:project-name
   #:filter
   #:target
   #:flatten?)

  ;; Build
  (:export
   #:build

   #:build
   #:all-builds
   #:last-builds
   #:build-config
   #:make-build
   #:delete-build

   #:job
   #:slave
   #:slave-name
   #:result
   #:building?
   #:failed?)

  ;; View
  (:export
   #:view

   #:view
   #:all-views
   #:view-config
   #:make-view
   #:delete-view

   #:jobs)

  (:documentation
   "TODO"))
