;;; protocol.lisp --- Protocol provided by the api module.
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

(cl:in-package #:jenkins.api)


;;; Generic modle object protocol
;;

(defgeneric id (object)
  (:documentation
   "TODO"))

(defgeneric commit! (object)
  (:documentation
   "Write transient changes to OBJECT back to the Jenkins server to
have them take effect and make them permanent."))

(defgeneric rename (object new-name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric delete* (object)
  (:documentation
   "TODO(jmoringe): document"))


;;; Node protocol
;;

#+no (defgeneric make-slave (name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric online? (node)
  (:documentation
   "TODO"))

(defgeneric mark-online! (node
			  &key
			  if-online)
  (:documentation
   "TODO"))

(defgeneric mark-offline! (node
			   &key
			   if-offline)
  (:documentation
   "TODO"))


;;; Job protocol
;;

#+no (defgeneric make-job (name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric enable! (job)
  (:documentation
   "TODO"))

(defgeneric disable! (job)
  (:documentation
   "TODO"))

(defgeneric relate (parent child)
  (:documentation
   "TODO"))

(defgeneric unrelate (parent child)
  (:documentation
   "TODO"))
