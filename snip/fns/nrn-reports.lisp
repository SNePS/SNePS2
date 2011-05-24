;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: nrn-reports.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


(in-package :snip)


; =============================================================================
;
; clear-incoming-reports
; ----------------------
;
;       returns       : <report set>
;
;       nonlocal-vars : the *REPORTS* register of the current node
;
;       description   : clears the *REPORTS* register
;
;       side-effects  : updates the above register as described
;
;                                        written :  rgh 10/06/85
;                                        modified:  rgh 10/15/85
;
;
(defmacro clear-incoming-reports ()
  `(setq *REPORTS* (new.repset)))
;
;
; =============================================================================
;
; process-reports.non-rule
; ------------------------
;
;       nonlocal-vars : various registers of the current *NODE*
;                         *PENDING-FORWARD-INFERENCES*, *NODE*
;
;       description   : checks each incoming report, and does the
;                       following:
;                         - if the report is that of a previously
;                            unknown instance, that instance is built,
;                            asserted, and sent to all interested
;                            parties.  If there were no interested, open
;                            outgoing channels, the report is placed in
;                            the *PENDING-FORWARD-INFERENCES* register.
;                            unless it was received through a match
;                            channel, in which case it need not be passed
;                            on.  (Since any nodefun it may be passed on to
;                            should already have heard about it from the
;                            same source this one did.)
;                         - the incoming reports registers are cleared
;                         - if there are any pending forward inferences, the
;                            nodefun is rescheduled (to be handled after all
;                            reports have been processed.
;
;       side-effects  : the above mentioned registers are updated
;
;                                        written :  rgh 10/06/85
;                                        modified:  rgh 10/15/85
;                                                   rgh 11/18/85
;                                                   rgh 11/24/85
;                                                   rgh 11/29/85
;                                                   rgh  2/23/86
;                                                   rgh  3/09/86
;                                                   rgh  3/30/86
;
;
(defun process-reports.non-rule ()
  (let ((reports *REPORTS*))
    (declare (special *PRIORITY*))
    (clear-incoming-reports)
    (do ()
	((isnew.repset reports))
	(process-one-report.non-rule (choose.repset reports))
	(setq reports (others.repset reports)))
    (cond ((not (isnew.repset *PENDING-FORWARD-INFERENCES*))
	   (setq *PRIORITY* 'LOW)
	   (initiate (activation.n *NODE*))))))
;
;
; =============================================================================
;
; process-one-report.non-rule
; ---------------------------
;
;       arguments     : report - <report>
;
;       nonlocal-vars : the *KNOWN-INSTANCES*, *NODE*, and PENDING-FORWARD-
;                       INFERENCES: registers, and *ADDED-NODES*
;
;       description   : if the instance reported by "report" is a previously
;                       unknown one, a new node is built and asserted, and
;                       the report is passed on to OUTGOING-CHANNELS:
;
;       side-effects  : *KNOWN-INSTANCES* and *PENDING-FORWARD-INFERENCES*
;                       registers, and *ADDED-NODES*, are updated
;
;                                        written :  rgh  3/30/86
;                                        modified:  scs  4/20/88
;                                        modified:  scs  6/22/88
;
;
(defun process-one-report.non-rule (report)
  (let ((instance (rep-to-instance report)))
    (declare (special *ADDED-NODES*))
    (when (unknown.inst instance)
      (setq *ADDED-NODES* (insert.ns (node.rep report) *ADDED-NODES*)
	    *KNOWN-INSTANCES* (insert.iset instance *KNOWN-INSTANCES*))
      (if (not (broadcast-one-report
		 (make.rep
		   (subst.rep report)
		   (support.rep report)
		   (sign.rep report)
		   *NODE*
		   nil
		   (context.rep report))))
	  (setq *PENDING-FORWARD-INFERENCES*
		(insert.repset report *PENDING-FORWARD-INFERENCES*))))))
;
;
; =============================================================================
;
; broadcast-one-report
; --------------------
;
;       arguments     : rep - <report>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *OUTGOING-CHANNELS* register
;
;       description   : broadcasts "rep" to each of the <channel>s in
;                       *OUTGOING-CHANNELS*
;                            returns "true" if "rep" was actually sent
;                            through at least one of the channels, "false"
;                            otherwise.
;                       the signature of the report is updated
;
;                                        written :  rgh 07/29/85
;                                        modified:  rgh 08/21/85
;                                                   rgh 10/15/85
;                                                   rgh  2/10/86
;                                                   rgh  3/09/86
;                                                   rgh  3/30/86
;

(defun broadcast-one-report (rep)
  (let (anysent)
    (do.chset (ch *OUTGOING-CHANNELS* anysent)
       (when (isopen.ch ch)
	 (setq anysent (or (try-to-send-report rep ch) anysent))))))
;
;
; =============================================================================



    
    




