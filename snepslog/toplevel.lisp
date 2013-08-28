;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: toplevel.lisp,v 1.3 2013/08/28 19:07:26 shapiro Exp $

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


(defvar *snepslog-prompt* '\:)



;;; Note: this function was altered by SCS on 8/25/04 for the following reason:
;;; Previously, if one set *infertrace* to nil in SNePSLOG,
;;;    and then got into lisp and back into SNePSLOG,
;;;    *infertrace* was reset to :surface.
;;; Now, that won't happen.
;;; The reason this is so complicated is that
;;;    the default non-null value of *infertrace* should be t in SNePS,
;;;    but :surface in SNePSLOG.

(defun snepslog (&key 
		 (inunit *standard-input*)
		 (outunit cl-user:*default-output-stream*)
		 (hello-text
		  (format
		   nil
		   "~%   Welcome to SNePSLOG (A logic interface to SNePS)~
    ~2%Copyright (C) ~a by Research Foundation of~
     ~%State University of New York. SNePS comes with ABSOLUTELY NO WARRANTY!~
     ~%Type `copyright' for detailed copyright information.~
     ~%Type `demo' for a list of example applications.~%"
		   *copyright-years*))
		 (bye-text "Bye"))
"Enters in the  SNePSLOG environment"
  (in.environment
    :variables ((*package* (find-package 'snepslog))
		(*print-length* nil)
		(*print-level* nil)
		(*print-pretty* t)
		(old-infertrace snip:*infertrace*)
		(snepslog::*SNePSLOGRunning* t))
    :declarations ((special snepslog:*SNePSLOGRunning*))
    :functions ((surface 'snepslog:surface)
		(slight-surface #'snepslog:slight-surface)
		(node-intern #'snepslog:node-intern)
		(sneps-node? #'snepslog:sneps-node?))
    :eval (progn
	    ;; set *infertrace* to :surface unless it was set to nil
	    (setq snip:*infertrace*
	      (and snip:*infertrace* :surface))
	    (snepslog-init hello-text) 
	    (snepslog-loop)
	    (format outunit "~%~A~%" bye-text))
    :always.do.this (progn 
		      ;; reset *infertrace* to old-infertrace
		      ;;    unless it was set to nil
		      (setq snip:*infertrace*
			     (and snip:*infertrace* old-infertrace))
			   (snepslog:snepslogreadoff)
			   nil)))

(defun snepslog-init (&optional hello)
  "Initializes the sneps environment"
  (if (stringp hello) 
      (format outunit "~%~A~%" hello))
  ;; There is a new canonical relation that should be kept.
  (setq *initial-relations*
	(adjoin 'snepsul:r *initial-relations*))
  (when *sneps-setup-flag*
    (sneps-setup)
    (setq *sneps-setup-flag* nil)))






    
    
(Defun Snepslog-Loop ()
  "Executes a read eval print loop"
  (declare (special crntct))
  (let (command demo-start-time oldtime newtime result eof?)
    (declare (special demo-start-time))
    (with-demo-control (inunit ((snepslog:snepslog-read nil)
			        (pseudolisp-read nil)))
      (unwind-protect
	  (catch :snepslog-end
	    (loop
	      (catch 'sneps-error
    (when (context-okinconsistent crntct)
      (format outunit "*"))
		(format outunit "~A " *snepslog-prompt*)
		(setq demo-start-time nil)
		(multiple-value-setq (command eof?)
		  (snepslog:snepslog-read inunit))
		;; GC time=0 until function found.
		(setq oldtime (or demo-start-time
				  (list (get-internal-run-time) 0))
		      result (topsneval command)
		      newtime (list (get-internal-run-time) 0))
		(format outunit "~&~%")
		(snepslog:snepslog-print result outunit)
		(set.sv 'lastcommand command)
		(sneps-timer oldtime newtime))
	      ;;(when eof? (return))
	      ))))))





