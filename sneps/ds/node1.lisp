;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: node1.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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


; =============================================================================
;
; print.n
; -------
;
;       arguments     : n - <node>
;                       outunit - <unit>
;
;       returns       : nil
;
;       description   : It Prints a <node> to "outunit"
;                       so that it can later be reconstructed with read.n
;
;       side-effects  : It prints the <node>
;
;                                        written :  ejm 10/04/83
;                                        modified:  scs 02/16/87
;                                                   ssc 02/25/87
;                                                   njm 09/26/88
;                                                   hc  06/29/93
;                                                   hi  03/20/99
;
(defun print.n (node outunit)
  (with-readable-nodes-and-contexts
      (format outunit "~%~S~%~S ~S ~S~%~S~%~S~%~S~%~S~%~S"
	      (node-na node)
	      (node-type node)
              ;(node-order node) ;03/20/99
	      (node-perm node)
	      (node-freevars node)
	      (node-fcableset node)
	      ;;(node-gi-node node)    ;; won't survive outnetting
	      ;;(node-activation node) ;; shouldn't survive outnetting
	      (node-asupport node)
	      (node-jsupport node)
	      (node-contexts node)
	      (when (eq (node-type node) :var)
		;; if other than variable, uses too much room
		(node-snepslog node))
	      )))

;
;
; =============================================================================
;
; read.n
; ------
;
;       arguments     : inunit - <unit>
;
;       returns       : <node>
;
;       description   : It reads and reconstructs a <node> from "inunit"
;                       assuming that the information was printed by print.n.
;
;       side-effects  : It constructs, or possibly just fills the <node>.
;                       Right now it does NOT update any system variables
;                       (more efficient, it's only used in in/outnet).
;
;                                        written :  ejm 10/04/83
;                                        modified:  scs 02/20/87
;                                        modified:  njm 09/27/88
;                                                   njm 10/04/88
;                                                    hc 06/29/93
;                                                    hi 03/20/99
;
(defun read.n (inunit &optional count)
  (let* ((name (read inunit))
	 (type (read inunit))
	 (node (or (node name)
		   (newnode name type))))
    ;; Reconstruct node:
    (setf (node-na node) name)
    (setf (node-type node) type)
    (setf (node-perm node) (read inunit))
    (setf (node-freevars node) (read inunit))
    (setf (node-fcableset node) (read inunit))
    ;;(setf (node-gi-node node) (read inunit))
    ;;(setf (node-activation node) (read inunit))
    (setf (node-asupport node) (read inunit))
    (setf (node-jsupport node) (read inunit))
    (setf (node-contexts node) (read inunit))
    (when count
      (setf (node-order node) count))
    (setf (node-snepslog node) (read inunit))
    ;; Assume that this is only used for in/outnet, hence,
    ;; we don't have to handle system variables such as
    ;; `varnodes' or `assertions'.
    ))



    
    




