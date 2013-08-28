;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: nrn-finfers.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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
; process-forward-inferences.non-rule
; -----------------------------------
;
;       returns       : <boolean>
;
;       nonlocal-vars : the PENDING-FORWARD-INFERENCES: register of the
;                       current node
;
;       description   : goes through the PENDING-FORWARD-INFERENCES: register
;                       and processes each report in it.
;
;       side-effects  : the PENDING-FORWARD-INFERENCES: register is cleared
;                       and the INCOMING-CHANNELS: and the OUTGOING-CHANNELS:
;                       registers may be updated.
;
;                                        written :  rgh 10/06/85
;                                        modified:  rgh  3/22/86
;                                                   njm 10/23/88
;                                                   scs 9/14/06 use correct new pkg for converse arcs
;
;
(defun process-forward-inferences.non-rule ()
  (let ((repset *PENDING-FORWARD-INFERENCES*))
    (setq *PENDING-FORWARD-INFERENCES* (new.repset))
    (do ()
	((isnew.repset repset))
      (process-one-forward-inference.non-rule (choose.repset repset))
      (setq repset (others.repset repset)))))
;
;
; =============================================================================
;
; process-one-forward-inference.non-rule
; --------------------------------------
;
;       arguments     : rep - <report>
;
;       returns       : <boolean>
;
;       nonlocal-vars : the *NODE* and *INCOMING-CHANNELS* registers of the
;                       current process
;
;       description   : handles the processing of a single forward-inference
;                       report, sending it to possibly interested rules and
;                       calling forward-match to find possibly interested
;                       antecedent nodes.
;
;       side-effects  : possibly adds channels to *OUTGOING-CHANNELS* 
;
;                                        written :  rgh 10/05/85
;                                        modified:  rgh  2/02/86
;                                                   rgh  3/09/86
;                                                   rgh  4/24/86
;                                                   njm 10/23/88
;                                                   njm 06/30/89
;                                                    hi 03/31/99
;
; looks for whenever-do's. hi
;
(defun process-one-forward-inference.non-rule (rep)
  (let* ((crntct (context.rep rep))
	 (sub (subst.rep rep))
	 (updated-rep (make.rep sub (support.rep rep) (sign.rep rep)
				*NODE* nil crntct)))
    (setq *INCOMING-CHANNELS*
          (insert.feedset
	    (make.feeder (make.restr sub) crntct (signature.rep rep) 'OPEN)
	    *INCOMING-CHANNELS*))
    (push-forward updated-rep (sneps:in-context.ns (nodeset.n *NODE*
							      'sneps:ant-) t))

    (push-forward updated-rep (sneps:in-context.ns (nodeset.n *NODE*
							      'sneps:&ant-) t))

    (push-forward updated-rep (sneps:in-context.ns (nodeset.n *NODE*
							      'when-) t))
    (push-forward updated-rep 
		  (sneps:in-context.ns (nodeset.n *NODE* 'sneps:whenever-) t))
    (push-forward updated-rep
		  (sneps:in-context.ns (sneps:remove-if-not.ns
					 #'(lambda (n) (not (or (is-and.n n) (is-nor.n n))))
					 (nodeset.n *NODE* 'sneps:arg-)) t))

    ;; in a numerical quantifier rule, a report from consequent node
    ;; is sent to the rule node
    (let ((numq-rule-nodes nil))
      (do.ns (rule-node (nodeset.n *NODE* 'sneps:cq-))
	  (if (is-num-quant.n rule-node)
	      (setq numq-rule-nodes
		    (append numq-rule-nodes (list rule-node)))))
      (push-forward updated-rep (sneps:in-context.ns numq-rule-nodes t)))

    (cond ((and (not (is-node-to-node.rep rep)) (enough-resources))
	   (decrease-resources)
	   (do.supmatchingset (m (forward-match-in-context *NODE* crntct sub))
	     (unless (eq (tnode.supmatching m) *NODE*)
	       (let (ch)
		 (setq ch (make.ch (target-sub.supmatching m)
				   (source-sub.supmatching m)
				   crntct
				   (tnode.supmatching m)
				   'OPEN))
		 (install-channel ch) 
		 (send-reports (makeone.repset updated-rep) ch))))))))
;
;
; =============================================================================
;
; push-forward
; ------------
;
;       arguments     : report  - <report>
;                       nodeset - <nodefun set>
;                  
;
;       returns       : <boolean>
;
;       description   : opens channels to all the nodes in "nodeset" and
;                       sends "report" to each of them
;
;       side-effects  : *OUTGOING-CHANNELS* register is updated by call to
;                       "install-channel"
;
;                                        written :  rgh 10/05/85
;                                        modified:  rgh  2/02/86
;                                                   njm 10/23/88
;
;
(defun push-forward (report nodeset)
  (do ((ns nodeset (others.ns ns))
       (ch))
      ((isnew.ns ns))
      (setq ch (make.ch (new.sbst)
			(new.sbst)
			(context.rep report)
			(choose.ns ns)
			'OPEN))
      (install-channel ch)
      (send-reports (makeone.repset report) ch)))
;
;
; =============================================================================



    
    




