;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: outnet.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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


; ==========================================================================
;
; outnet
; ------
;
;       arguments     : file - <filename> 
;
;       returns       : <nothing>
;
;       description   : "User function" to be called from SNePS environment.
;                       It dumps the current network to the FILE.
;
;       side-effects  : It prints a message.
;
;                                          written : jgn 10/10/83
;                                          modified: ejm 06/01/84
;                                                    ssc 02/13/87
;                                                    njm 09/26/88
;                                                    hc  29/06/93
;                                                    hc  07/18/93
;
(defsnepscom outnet ((file) (top) t)
  (with-open-file (outunit (cl-user:sneps-translate file) :direction :output
		   :if-does-not-exist :create
		   :if-exists :new-version)
    (let ((*package* (find-package 'snepsul))
	  (*print-pretty* nil)
	  (*print-level* nil)
	  (*print-length* nil))
      (format outunit "|SNePS network 2.7|~%")
      (outindices outunit)
      (outrelations outunit)
      (outpaths outunit)
      (outcontexts outunit)
      (outnodes outunit)
      (outsysvars outunit)))
  (format t "~%~%Network dumped to file: ~A~%" file)
  (values))


(defun outindices (outunit)
  (format outunit "~%;; Node/Context-ID indices:~%")
  (dolist (node-name-prefix '(b m v p tm tv tp))
    (format outunit "~s " (get 'gennewnode node-name-prefix)))
  (format outunit "~s~%" (get 'gennewcontext 'c)))

;
;
;
; ==========================================================================
;
; outrelations
; ------------
;
;       arguments     : outunit - <output port> 
;
;       returns       : Sequence of relations 
;
;       description   : Prints a list of the defined relations to
;                       the output port.
;
;       nonlocal-vars : SNePS variable "relations"
;
;       side-effects  : Additions to output file. 
;
;                                          written : jgn 10/10/83
;                                          modified: hc  06/29/93
;
(defun outrelations (outunit)
  (let ((relations (value.sv 'relations)))
    (format outunit "~%~d ;; Relation definitions:"
	    (length relations))
    (dolist (relation relations)
      (print.r relation outunit))
    (terpri outunit)))

(defun outpaths (outunit)
  (let ((paths (remove-if-not #'(lambda (r) (get r :pathdef))
			      (value.sv 'relations))))
    (format outunit "~%~d ;; Path definitions:"
	    (length paths))
    (dolist (path paths)
      (print-path.r path outunit))
    (terpri outunit)))
 
;
;
;
; ==========================================================================
;
; outcontexts
; -----------
;
;       arguments     : outunit - <output port> 
;
;       returns       : Sequence of the current list of contexts 
;
;       nonlocal-vars : System variable "contexts"
;
;       description   : Prints a list of the network contexts to output port.
;
;       side-effects  : Additions to output file. 
;
;                                          written : njm 09/26/88
;                                          modified: hc  06/29/93
;
(defun outcontexts (outunit)
  (let ((contexts (allct)))
    (format outunit "~%~d ;; Context definitions:" (length contexts))
    (dolist (context contexts)
      (print.ct context outunit))
    (terpri outunit)))

;
;
;
; ==========================================================================
;
; outnodes
; --------
;
;       arguments     : outunit - <output port> 
;
;       returns       : Sequence of the current list of nodes 
;
;       nonlocal-vars : System variable "nodes"
;
;       description   : Prints a list of the network nodes to output port. 
;
;       side-effects  : Additions to output file. 
;
;                                          written : jgn 10/10/83
;                                          modified: hc  06/29/93
;                                                    hi  03/24/99
;       modification: Since (isless.n n1 n2) is true in case 
;                     (> (order n1) (order n2)), then need to
;                     reverse the nodes list before printing it.
;                     This is mainly because read.n assigns orders
;                     according to the position of the node in the infile.
;                     HI, 3/24/99.
;
(defun outnodes (outunit)
  (let ((nodes (value.sv 'nodes)))
    (format outunit "~%~d ;; Node definitions:" (length nodes))
    (dolist (node (reverse nodes))
      (print.n node outunit))
    (terpri outunit)))

;
;
; ==========================================================================
;
; outsysvars
; ----------
;
;       arguments     : outunit - <output port> 
;
;       returns       : Irrelevant, but returns a list of system 
;                       variables.
;
;       nonlocal-vars : The system variables
;
;       description   : Prints the values of the system variables 
;                       to the output port.
;
;       side-effects  : Additions made to output file 
;
;                                          written : jgn 10/10/83
;                                          modified: hc  06/29/93
;                                                    hi  03/28/99 out *nodes
;
(defun outsysvars (outunit)
  (let ((variables (value.sv 'variables))
	(excluded-variables
	 '(;; context hashtable is unprintable and gets
	   ;; automatically reconstructed:
	   contexts
	   ;; this is redundant because they get automatically
	   ;; constructed by `inrelations':
	   relations
	   ;; are these any interesting?
	   ;;command lastcommand errorcommand
	   )))
    (format outunit "~%~d ;; SNePSUL variable definitions:"
	    (cl:- (length variables) (length excluded-variables)))
    (dolist (variable variables)
      (unless (member variable excluded-variables)
	(print.sv variable outunit)))
    (terpri outunit)))



    
    




