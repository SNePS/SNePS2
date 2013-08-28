;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                         State University of New York

;; Version: $Id: non-rule.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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




(in-package :snip)


; =============================================================================
;
; non-rule
; --------
;
;       description   : This is the node activation process for non-rule
;                       nodes.
;
;                                        written :  rgh 11/18/85
;                                        modified:  rgh  3/08/86
;                                                   rgh  3/22/86
;                                                   rgh  4/13/86
;                                                   njm  4/27/89
;
;
(defun non-rule (*NAME* *NODE* *KNOWN-INSTANCES* *REPORTS* *REQUESTS* 
		 *INCOMING-CHANNELS* *OUTGOING-CHANNELS*
		 *PENDING-FORWARD-INFERENCES* *PRIORITY*)
  (declare (special multi::curnt%))
  (catch 'Stop-Handled-by-Contradiction-Handler
    (cond (multi:*use-one-queue-only*
	   (process-reports.non-rule)
	   (process-requests.non-rule)
	   (process-forward-inferences.non-rule))
	  ((not (isnew.repset *REPORTS*))
	   (process-reports.non-rule))
	  (t (process-requests.non-rule)
	     (process-forward-inferences.non-rule))))
  (multi:process-change-slots multi::curnt%
			      (vector *NAME* *NODE* *KNOWN-INSTANCES*
				      *REPORTS* *REQUESTS*
				      *INCOMING-CHANNELS*
				      *OUTGOING-CHANNELS*
				      *PENDING-FORWARD-INFERENCES*
				      *PRIORITY*)))

(setf (get 'non-rule 'multi::lregs%)
      '(*NAME* *NODE* *KNOWN-INSTANCES* *REPORTS* *REQUESTS* 
	*INCOMING-CHANNELS* *OUTGOING-CHANNELS* *PENDING-FORWARD-INFERENCES*
	*PRIORITY*))
;
;
; =============================================================================



    
    




