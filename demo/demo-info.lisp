;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: demo-info.lisp,v

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


(in-package :sneps)


(defvar *available-demos-menu*
  (mapcar
   #'(lambda (demo-description)
       (make-menu-item :label (cond ((second demo-description)
				     (format nil (second demo-description)))
				    (t (first demo-description)))
		       :value (first demo-description)))

   ;; A list of <filename description> pairs, if the description
   ;; is nil it defaults to filename.
   '(("sneps:demo;dbms;dbmsdemo.sneps"
      "DBMS - Using SNePS as a relational database")
     ;;("sneps:demo;sneps;jobspuzzle.sneps"
     ;; "The Jobs Puzzle - A solution to a puzzle that demonstrates~
     ;;~%                  non-standard connectives and quantifiers")
     ("sneps:demo;snepslog;socrates.snlog"
      "Socrates - Is he mortal?")
     ("sneps:demo;sneps;uvbr.sneps"
      "UVBR - Demonstrating the Unique Variable Binding Rule")
     ("sneps:demo;snepslog;uvbr.snlog"
      "UVBR - Demonstrating the Unique Variable Binding Rule")
     ("sneps:demo;sneps;numquant;jobspuzzle.sneps"
      "The Jobs Puzzle - A solution with the Numerical Quantifier")
     ("sneps:demo;snepslog;jobspuzzle.snlog"
      "The Jobs Puzzle - A solution with the Numerical Quantifier")
     ("sneps:demo;sneps;numquant;meeting.sneps"
      "Meeting - A numerical quantifier example about people in meetings")
     ("sneps:demo;sneps;numquant;mother.sneps"
      "Mother - A numerical quantifier example about the mother relation")
     ("sneps:demo;sneps;numquant;owner.sneps"
      "Owner - A numerical quantifier example about dog owners")
     ("sneps:demo;snere;blocksworld;sneredemo.sneps"
      "SNeRE - A demo of the SNePS Rational Engine")
     ("sneps:demo;snere;arcinfo;arcdemo.sneps"
      "ARC/SNACTor - Connecting ARC/INFO and SNeRE (previously SNACTor)")
     ("sneps:demo;nlint;qa-demo.sneps"
      "Q&A - Using SNaLPS for simple natural language question answering")
     ("sneps:demo;nlint;tree-demo.sneps"
      "Parse Trees - Using SNaLPS to produce parse trees")
     ("sneps:demo;snepslog;pegasus.sneps"
      "Pegasus - Why winged horses lead to contradictions")
     ("sneps:demo;sneps;brachet-dist;brachet.sneps"
      "Contextual Vocabulary Acquisition: forming a definition for 'brachet'")
     ("sneps:demo;snepslog;pegasus.snlog"
      "Pegasus - Why winged horses lead to contradictions")
     ("sneps:demo;snepslog;steamroller.snlog"
      "Schubert's Steamroller")
     ("sneps:demo;snepslog;ruleintro.snlog"
      "Rule Introduction - Various examples")
     ("sneps:demo;snepslog;chap4.snlog"
      "Examples of various SNeRE constructs.")
     ("sneps:demo;snepslog;blackbeard1.snlog"
      "Assisted Nonprioritized Belief Revision with Manual tie-breaking.")
     ("sneps:demo;snepslog;blackbeard2.snlog"
      "Assisted Nonprioritized Belief Revision with Manual tie-breaking showing
Minimal Change of KB.")
     ("sneps:demo;snepslog;blackbeard3.snlog"
      "Fully Automatic Nonprioritized Belief Revision.")
     ("sneps:demo;snepslog;duck.snlog"
      "Prioritized Belief Revision with Manual Tie-Breaking.")
     ("sneps:demo;snepslog;meetings.snlog"
      "Automatic Belief Revision Based on Explicit Object-Language Ordering.")
     ("sneps:demo;snepslog;sourcedMeetings.snlog"
      "Automatic Belief Revision Based on Explicit Object-Language Source-Ordering.")
     ("sneps:demo;snepslog;agents.snlog"
      "Automatic Belief Revision Based on Fluents vs. Non-fluents.")
     #|
     ("sneps:demo;defaults;birds.sneps"
      "Birds - An example for default rules (all demos dealing with~
     ~%        defaults require Maria Cravo's version of SNePS)")
     ("sneps:demo;defaults;yale-shooting.sneps"
      "Yale Shooting Problem - A solution using default rules")
     ("sneps:demo;defaults;yale-shooting.snlog"
      "Yale Shooting Problem - A solution using default rules (all demos~
     ~%        with defaults require Maria Cravo's version of SNePS)")
     |#
     )))

(defvar *available-sneps-demos-menu*
  (remove-if-not #'(lambda (demo-file)
		     ;; Explorers have problems with pathnames containing
		     ;; logical hosts, hence let's use search
		     (search ".sneps" demo-file
			     :from-end t
			     :start2 (cl:- (length demo-file) 8)))
		 *available-demos-menu*
		 :key #'menu-item-value)
  "A menu of SNePS demos")

(defvar *available-snepslog-demos-menu*
  (remove-if-not #'(lambda (demo-file)
		     (search ".snlog" demo-file
			     :from-end t
			     :start2 (cl:- (length demo-file) 6)))
		 *available-demos-menu*
		 :key #'menu-item-value)
  "A menu of SNePSLOG demos")
