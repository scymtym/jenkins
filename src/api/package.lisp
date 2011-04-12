;;; package.lisp --- Package definition for api module.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

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
   #:*jenkins-base-url*
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
