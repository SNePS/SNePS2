;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MULTI; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: exports.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

;; This file is part of SNePS.

;; $BEGIN LICENSE$

;; 
;; The contents of this file are subject to the University at
;; Buffalo Public License Version 1.0 (the "License"); you may
;; not use this file except in compliance with the License. You
;; may obtain a copy of the License at http://www.cse.buffalo.
;; edu/sneps/Downloads/ubpl.pdf.
;; 
;; Software distributed under the License is distributed on an
;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;; or implied. See the License for the specific language gov
;; erning rights and limitations under the License.
;; 
;; The Original Code is SNePS 2.7.
;; 
;; The Initial Developer of the Original Code is Research Foun
;; dation of State University of New York, on behalf of Univer
;; sity at Buffalo.
;; 
;; Portions created by the Initial Developer are Copyright (C)
;; 2011 Research Foundation of State University of New York, on
;; behalf of University at Buffalo. All Rights Reserved.
;; 
;;  
;; 
;; 


;; $END LICENSE$


(in-package :multi)


;;; Globals used in process registers
(defvar *QUEUES*)
(defvar *NAME*)
(defvar *PRIORITY*)
(defvar *use-one-queue-only*)

(export '(dp evnts in-trace ev-trace unin-trace unev-trace regfetch
	  regstore new initiate schedule multip *QUEUES* *NAME*
	  *PRIORITY* *use-one-queue-only* clear-all-queues
	  new-process-name process-change-slots))

;;; 23 Feb 2011 ABCL-specfic code to fix bug:
;;; When SNePS commands were later defined for these symbols via 
;;; defsnepscom, ABCL 0.24.0 was crashing when defsnepscom called 
;;; shadowing-import. Importing them with shadowing-import initially 
;;; solves the problem. - JPB
#+abcl (shadowing-import '(in-trace ev-trace unev-trace unin-trace) (find-package 'snepsul))
#-abcl (import '(in-trace ev-trace unev-trace unin-trace) (find-package 'snepsul))



(in-package :dequeue)

(export '(empty front delete-front insert-rear new insert-front
		in-queue queue-length))






    
    




