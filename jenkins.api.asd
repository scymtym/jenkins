;;; jenkins.api.asd --- System definition for the jenkins.api system.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:defpackage #:jenkins.api-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:jenkins.api-system)


;;; Version stuff
;;

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))


;;; System definition
;;

(defsystem :jenkins.api
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Bindings for Jenkins' REST API."
  :depends-on  (:alexandria #+maybe-later (:version :alexandria      "0.0.0")
		(:version :split-sequence  "1.1")
		(:version :closer-mop      "0.61")
		:iterate #+maybe-later (:version :iterate         "1.4.4")
		(:version :let-plus        "0.1")
		(:version :more-conditions "0.1.0")

		(:version :cl-ppcre        "2.0.3")
		:puri
		(:version :drakma          "1.2.8")
		(:version :xml.location    "0.2.0")
		(:version :cl-json         "0.4.1"))
  :components  ((:module     "api"
		 :pathname   "src/api"
		 :serial     t
		 :components ((:file     "package")
			      (:file     "types")
			      (:file     "conditions")
			      (:file     "variables")
			      (:file     "protocol")
			      (:file     "conversion")
			      (:file     "classes")
			      (:file     "api")))

		(:module     "dsl"
		 :pathname   "src/dsl"
		 :depends-on ("api")
		 :serial     t
		 :components ((:file     "package")
			      (:file     "macros")))))
