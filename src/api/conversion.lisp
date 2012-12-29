;;; conversion.lisp --- Conversions used by the api module.
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


;;; Boolean
;;

(defmethod xloc:xml-> ((value string)
		       (type  (eql 'boolean))
		       &key &allow-other-keys)
  (cond
    ((string= value "true")
     t)
    ((string= value "false")
     nil)
    (t
     (error "~@<Value ~S is invalid for type ~S.~@:>"
	    value type))))

(defmethod xloc:->xml ((value t)
		       (dest  (eql 'string))
		       (type  (eql 'boolean))
		       &key &allow-other-keys)
  (if value "true" "false"))


;;; Keyword
;;

(defmethod xloc:xml-> ((value string)
		       (type  (eql 'keyword))
		       &key &allow-other-keys)
  (make-keyword value))

(defmethod xloc:->xml ((value symbol)
		       (dest  (eql 'string))
		       (type  (eql 'keyword))
		       &key &allow-other-keys)
  (symbol-name value))


;;; `equals-string/cons'
;;

(deftype equals-string/cons (&optional (key 'keyword) (value 'string))
  "A `cl:cons' which should be stored into certain locations of XML
documents as a string of the form KEY=VALUE."
  `(cons ,key ,value))

(defmethod xloc:xml-> ((value string)
		       (type  (eql 'equals-string/cons))
		       &key
		       inner-types
		       &allow-other-keys)
  (let+ (((&optional (keyword-type 'keyword) (value-type 'string))
	  inner-types)
	 ((key value) (split-sequence #\= value)))
    (cons (xloc:xml-> key keyword-type) (xloc:xml-> value value-type))))

(defmethod xloc:->xml ((value cons)
		       (dest  (eql 'string))
		       (type  (eql 'equals-string/cons))
		       &key
		       inner-types
		       &allow-other-keys)
  (let+ (((&optional (keyword-type 'keyword) (value-type 'string))
	  inner-types))
    (format nil "~A=~A"
	    (xloc:->xml (car value) 'string keyword-type)
	    (xloc:->xml (cdr value) 'string value-type))))


;;; `string/node'
;;

(deftype string/node ()
  "A `cl:string' which should be stored into certain locations of XML
documents in a particular way."
  'string)

(defmethod xloc:xml-> ((value stp:element)
		       (type  (eql 'string/node))
		       &key &allow-other-keys)
  (stp:data (stp:first-child value)))

(defmethod xloc:->xml ((value string)
		       (dest  stp:element)
		       (type  (eql 'string/node))
		       &key &allow-other-keys)
  (stp:delete-children dest)
  (stp:append-child dest (stp:text :data value))
  dest)


;;; list/{comma,newline,space}
;;
;; Lists which are represented as strings with certain separator
;; characters.

(macrolet
    ((define-separator-string-list-type (separator)
       (let ((name                    (format-symbol
				       *package* "LIST/~@:(~A~)" (char-name separator)))
	     (format-string/aesthetic (format nil "~~{~~A~~^~C~~}" separator))
	     (format-string/readable  (format nil "~~{~~S~~^~C~~}" separator)))
	 `(progn
	    (deftype ,name (&optional element-type)
	      (if element-type
		  `(or null (cons ,element-type))
		  'list))

	    (defmethod xloc:xml-> ((value string)
				   (type  (eql ',name))
				   &key
				   inner-types
				   &allow-other-keys)
	      (mapcar (compose (curry #'string-trim '(#\Space #\Tab #\Newline))
			       (rcurry #'xloc:xml-> inner-types))
		      (split-sequence ,separator value :remove-empty-subseqs t)))

	    (defmethod xloc:->xml ((value list)
				   (dest  (eql 'string))
				   (type  (eql ',name))
				   &key
				   inner-types
				   &allow-other-keys)
	      (with-standard-io-syntax
		(if (equal inner-types '(string))
		    (format nil ,format-string/aesthetic value)
		    (format nil ,format-string/readable  value))))))))

  (define-separator-string-list-type #\,)
  (define-separator-string-list-type #\Newline)
  (define-separator-string-list-type #\Space))


;;; equals+newline/plist
;;

(deftype equals+newline/plist (&optional key-type value-type)
  `(or null (cons ,key-type (cons ,(or value-type t)))))

(defmethod xloc:xml-> ((value string)
		       (type  (eql 'equals+newline/plist))
		       &key
		       inner-types
		       &allow-other-keys)
  (let ((element-type (list* 'equals-string/cons inner-types)))
    (iter (for line in (xloc:xml-> value '(list/newline string)))
	  (let+ (((key . value) (xloc:xml-> line element-type)))
	    (collect key)
	    (collect value)))))

(defmethod xloc:->xml ((value list)
		       (dest  (eql 'string))
		       (type  (eql 'equals+newline/plist))
		       &key
		       inner-types
		       &allow-other-keys)
  (let ((element-type (list* 'equals-string/cons inner-types)))
    (xloc:->xml (iter (for (key value1) on value :by #'cddr)
		      (collect
			  (xloc:->xml (cons key value1) dest element-type)))
		dest '(list/newline string))))


;;; `tree-map/plist'
;;
;; Converter Lisp plists to Jenkins' "tree-map". A "tree-map" looks
;; like this:
;;
;;   <SOME-CONTEXT>
;;     <int>SIZE-AS-INTEGER</int>
;;     <string>KEY</string>
;;     <string>VALUE</string>
;;     ...
;;   </SOME-CONTEXT>

(deftype tree-map/plist ()
  "A Lisp plist which should into XML documents as a \"tree-map\"."
  '(or null (cons keyword (cons string))))

(defmethod xloc:xml-> ((value stp:element)
		       (type  (eql 'tree-map/plist))
		       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:val items :type 'string/node) "./string"
	:if-multiple-matches :all))
      value
    (iter:iter (iter:for (key value) on items :by #'cddr)
	       (iter:collect (make-keyword key))
	       (iter:collect value))))

(defmethod xloc:->xml ((value list)
		       (dest  stp:element)
		       (type  (eql 'tree-map/plist))
		       &key &allow-other-keys)
  (let+ (((&values count remainder) (floor (length value) 2)))
    (unless (zerop remainder)
      (error 'type-error
	     :datum         value
	     :expected-type 'tree-map/plist))

    (xloc:with-locations
	(((:val size  :type 'integer)     "./int/text()" )
	 ((:val items :type 'string/node) "./string"
	  :if-multiple-matches :all))
	dest
      (setf size  count
	    items (mapcar #'string value))))
  dest)
