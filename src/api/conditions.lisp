;;; conditions.lisp --- Conditions used by the api module.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
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

(define-condition unmapped-class (condition)
  ((interface :initarg :interface
	      :type    symbol
	      :reader  unmapped-class-interface
	      :documentation
	      "Name of the interface for which the named
implementation could not be found.")
   (name      :initarg :name
	      :type    string
	      :reader  unmapped-class-name
	      :documentation
	      "Name of the implementation which could not be found."))
  (:default-initargs
   :interface (missing-required-initarg 'unmapped-class :interface)
   :name      (missing-required-initarg 'unmapped-class :name))
  (:report (lambda (condition stream)
	     (format stream "~@<No mapping for implementation ~S of ~
                                interface ~S.~@:>"
		     (unmapped-class-name condition)
		     (unmapped-class-interface condition))))
  (:documentation
   "This condition is signaled when a named implementation of an
interface cannot be found during deserializing of a model object from
XML."))
