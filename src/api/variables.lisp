;;;; variables.lisp --- Dynamic variables used by the api module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(declaim (special *base-url* *username* *password*))

(defvar *base-url* nil
  "")

(defvar *username* nil
  "")

(defvar *password* nil
  "")
