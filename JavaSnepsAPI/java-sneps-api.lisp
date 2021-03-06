;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: java-sneps-api.lisp,v

;; This file is part of SNePS.

;; $BEGIN LICENSE$

;;; The contents of this file are subject to the University at
;;; Buffalo Public License Version 1.0 (the "License"); you may
;;; not use this file except in compliance with the License. You
;;; may obtain a copy of the License at 
;;; http://www.cse.buffalo. edu/sneps/Downloads/ubpl.pdf.
;;; 
;;; Software distributed under the License is distributed on an
;;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;;; or implied. See the License for the specific language gov
;;; erning rights and limitations under the License.
;;; 
;;; The Original Code is SNePS 2.8.
;;; 
;;; The Initial Developer of the Original Code is Research Foun
;;; dation of State University of New York, on behalf of Univer
;;; sity at Buffalo.
;;; 
;;; Portions created by the Initial Developer are Copyright (C)
;;; 2011 Research Foundation of State University of New York, on
;;; behalf of University at Buffalo. All Rights Reserved.


;; $END LICENSE$

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial jlinker and GUI setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :snepslog)
(use-package :javatools.jlinker)


;;; Define the JavaSNePSAPI class
(def-java-class (java-sneps-api "edu.buffalo.sneps.JavaSnepsAPI")
    () () () ())

;;; Define the Substitution class and functions needed
(def-java-class (substitutions "edu.buffalo.sneps.Substitution")
    () () () ())
(def-java-constructor new-substitution-set 
  (substitutions "int"))
(def-java-method (add-to-subs "addSubstitutionPair") 
  (substitutions "java.lang.String" "java.lang.String"))

;;; Define array list and functions needed
(def-java-class (hash-set "java.util.HashSet") 
  () () () ())
(def-java-constructor new-hash-set
    (hash-set "int"))
(def-java-method (add-to-hs "add") (hash-set "java.lang.Object")) 


(export '(java-sneps-ask 
	  java-sneps-askwh 
	  java-sneps-askifnot
	  java-sneps-askwhnot
	  init-java-sneps-connection
	  jlinker-end))

(defun java-sneps-ask (command)
  "Special implementation of the snepslog ask function needed to interact
   with Java. Returns Java objects useless without the Jlinker
   interface and a Java process to interpret their data."
  (let* ((result (ask command))
	 (result-list (mapcar #'(lambda (x) 
				  (string-trim '(#\Space #\Newline) 
					       (snepslog-print x nil)))  
			      result))
	 (result-hash (new-hash-set (length result-list))))
    (loop for x in result-list
	do (add-to-hs result-hash x))
    result-hash))

(defun java-sneps-askifnot (command)
  "Special implementation of the snepslog askifnot function needed to interact
   with Java. Returns Java objects useless without the Jlinker
   interface and a Java process to interpret their data."
  (let* ((result (askifnot command))
	 (result-list (mapcar #'(lambda (x) 
				  (string-trim '(#\Space #\Newline) 
					       (snepslog-print x nil)))  
			      result))
	  (result-hash (new-hash-set (length result-list))))
    (loop for x in result-list
	do (add-to-hs result-hash x))
    result-hash))
	 
	 
(defun java-sneps-askwh (command)
  "Special implementation of the snepslog askwh function needed to interact
   with Java. Returns Java objects useless without the Jlinker
   interface and a Java process to interpret their data."
  (let* ((result (askwh command))
	 (return-hash (new-hash-set (length result))))
    (loop for subs in result
      do (let ((substitute-set (new-substitution-set (length subs))))
	   (loop for pairs in subs
	     do (add-to-subs 
		 substitute-set
		 (string (car pairs))
		 (string (sneps:node-na (cdr pairs)))))
	   (add-to-hs return-hash substitute-set)))
    return-hash))

(defun java-sneps-askwhnot (command)
  "Special implementation of the snepslog askwhnot function needed to interact
   with Java. Returns Java objects useless without the Jlinker
   interface and a Java process to interpret their data."
  (let* ((result (askwhnot command))
	 (return-hash (new-hash-set (length result))))
    (loop for subs in result
      do (let ((substitute-set (new-substitution-set (length subs))))
	   (loop for pairs in subs
	     do (add-to-subs 
		 substitute-set
		 (string (car pairs))
		 (string (sneps:node-na (cdr pairs)))))
	   (add-to-hs return-hash substitute-set)))
    return-hash))
	   

(defparameter *sleep-time* 5
  "Time to check if jlinker has terminated.")

(defun init-java-sneps-connection (port classpath)
  "Initialize jlinker connection between sneps and java on the
   specified port number using the given java classpath string. This function 
   only needs to be called if you intend to use the Java Sneps Interface by
   manually initiating the SNePS system. Call this function after creating
   the JavaSnepsAPI object on the in the Java process."
  (setf (sys:getenv "CLASSPATH") classpath)
  ;;; Open a Lisp => Java connection
  (jlinker-init :java-advertises
		:verbose t
		:java-file nil
		:java-port port
		:error-p nil)

  (loop 
    until (not (jlinker-query))
      do (mp:process-sleep *sleep-time*))
  (cl-user::exit))

