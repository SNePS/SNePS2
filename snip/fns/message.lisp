;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: message.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; try-to-send-report
; ------------------
;
;       arguments     : rep - <report>
;                       ch - <channel>
;
;       returns       : <boolean>
;
;       description   : tests "rep" against the <filter> of "ch", and
;                       filters all supports not passing "ch".
;                       sends all those versions which pass through "ch".
;
;                                        written :  rgh 07/29/85
;                                        modified:  rgh 08/19/85
;                                                   rgh 08/21/85
;                                                   scs 06/17/88
;                                                   njm 10/23/88
;
;

;;; Interchange the two arguments of and
;;;    for faster failure.
(defun try-to-send-report (rep ch)
  (let ((newsup (filter.sup (support.rep rep) (context.ch ch))))
    (when (and newsup
	       (filter (subst.rep rep) (filter.ch ch)))
      (send-reports (makeone.repset (make.rep (subst.rep rep)
					      newsup
					      (sign.rep rep)
					      (signature.rep rep)
					      (node.rep rep)
					      (context.rep rep)))
		    ch))))


(defparameter *depthCutoffForward* 10
  "Maximum height of SNIP subgoal.")

(export '*depthCutoffForward*)
(import '*depthCutoffForward* :snepsul)

					;
;
; =============================================================================
;
; send-reports
; ------------
;
;       arguments     : repset - <report set>
;                       ch - <channel>
;
;       returns       : <boolean>
;
;       description   : sends all the <report>s in "repset" through "ch"
;
;       implementation: it is assumed that all of the <report>s in "repset"
;                        have passed through the <filter> and the <context>
;                        of "ch"
;
;                                        written :  rgh  7/29/85
;                                        modified:  rgh  8/19/85
;                                                   rgh 11/24/85
;                                                   rgh  3/08/86
;                                                   rgh  4/20/86
;                                                   scs  6/22/88
;                                                   njm 10/23/88
;
					;
(defun send-reports (repset ch)
  (let ((d (destination.ch ch)))
    (cond ((is-user.dest d)
	   (let ((pr *user-process*)
		 (swrep (switch-reports repset ch)))
	     (regstore pr
		       '*reports*
		       (union.repset (regfetch pr '*reports*) swrep))
	     (regstore pr '*priority* 'high)
	     (initiate pr)
	     swrep))
	  (t
	   (when *depthCutoffForward*
	     (setf repset (filter-repset repset)))
	   (unless (isnew.repset repset)
	     (activate.n d)
	     (let ((pr (activation.n d))
		   (swrep (switch-reports repset ch)))
	       (regstore pr
			 '*reports*
			 (union.repset (regfetch pr '*reports*) swrep))
	       (regstore pr '*priority* 'high)
	       (initiate pr)
	       swrep))))))


(defun filter-repset (repset)
  "Retuns a repset like REPSET,
       but without any that exceed *depthCutoffForward*."
  (let ((filteredrepset (new.repset)))
    (do.repset (rep repset filteredrepset)
	       (if
		   (or (> (sneps::node-height (signature.rep rep))
			  *depthCutoffForward*)
		       (> (max-height-sub (subst.rep rep))
			  *depthCutoffForward*))
		   (remark (format
			    nil
			    "~&Forward inference cutoff of ~A{~A} beyond *depthcutoffforward* = ~A~%"
			    (signature.rep rep)
			    (subst.rep rep)
			    *depthCutoffForward*)
			   nil nil)
		 (setf filteredrepset
		   (insert.repset rep filteredrepset))))))

;
; =============================================================================
;
; switch-reports
; --------------
;
;       arguments     : repset - <report set>
;                       sw - <switch>
;
;       returns       : <report set>
;
;       description   : switches variable context for all <reports> in "repset"
;                        from that of the sending nodefun to that of the
;                        receiving node.
;
;                                        written :  rgh 07/29/85
;                                        modified:  rgh 08/02/85
;
;
(defun switch-reports (repset ch)
  (let ((result (new.repset)))
    (do.repset (next-rep repset result)
	       (let ((newrep (switch-one-report next-rep ch)))
		 (if newrep
		     (setq result (insert.repset newrep result)))))))
;
;
; =============================================================================
;
; switch-one-report
; -----------------
;
;       arguments     : rep - <report>
;                       ch - <channel>
;
;       returns       : <report>
;
;       description   : switches the variable context of the report "rep".
;                       If the instance contained in the report is a previously
;                       unkown one, a new node is built.
;                       The new support is added to the node.
;
;
;                                        written :  rgh 07/30/85
;                                        modified:  rgh  8/05/85
;                                                   rgh  3/09/86
;                                                   scs  6/22/88
;                                                   cpf/njm 10/24/88
;                                                   scs  05/05/99  
;                                                   scs/flj 06/20/04
;   Latest change:
;;; If about to build (min 0 max 0 arg P),
;;;    and P is (min 0 max 0 Q),
;;;    returns Q instead of building ~(~Q)).
;
;
(defun switch-one-report (rep ch)
  (declare (special *name*))
  (let* ((d (destination.ch ch))
	 (signature (signature.rep rep))
	 (situation
	  (cond ((is-user.dest d) 'to-user)
		((ismemb.ns d (all-consequents signature)) 'to-cq)
		((let ((*node* d)) (is-ant-to-rule.rep rep)) 'to-rule)
		((let ((*node* d)) (is-cq-to-rule.rep rep)) 'from-cq)
		(t 'to-nonrule)))
	 (newsub (case situation
		   ((to-nonrule)
		    (match::substituteinterms (switch.ch ch) (subst.rep rep)))
		   ((to-rule to-user from-cq) (subst.rep rep))
		   (to-cq (match::substituteinterms (match::initbind
						     d) (subst.rep
						     rep))))))
    (unless (match::violates-uvbr newsub)
      (let* ((patnode (case situation
			((to-user to-rule from-cq) signature)
			((to-nonrule to-cq) d)))
	     (newnode
	      (if (quantified-vars.n patnode)
		  (choose.ns
		   (sneps:find-or-build
		    (sneps::clean-quantifiers.cs
		     (apply-subst.cs newsub (n-to-downcs patnode)))))
		(match::applysubst patnode newsub)))
	     (support
	      (funcall (if (eql signature newnode) #'identity #'non-hyp)
		       (case situation
			 ((to-rule from-cq) (support.rep rep))
			 ((to-user to-cq)
			  (if (and (eq *name* 'rule)
				   (eq *type* 'nor)
				   (instanciated-quantified-vars.n
				    signature ch newsub))
			      (change-tag-support (support.rep rep))
			    (support.rep rep)))
			 ((to-nonrule)
			  (if (or (instanciated-quantified-vars.n
				   signature ch newsub)
				  (nor-to-nor-with-different-args signature d))
			      (change-tag-support (support.rep rep))
			    (support.rep rep)))))))
    
	(declare (special newnode))
;;;    (format t "~% situation = ~s    substitution = ~s ~% " situation newsub)
;;;    (format t "~% newnode = ~s    new support = ~s ~% " newnode support)
	(setq newsub (adjoin (cons patnode newnode) newsub :test #'equal))
	(when (isneg.rep rep)
	  (setf newnode (sneps:negate.n newnode)))
	(when (and (or (ismol.n newnode) (ispat.n newnode))
		   (not (and (equal *name* 'rule)
			     (equal *type* 'nor)
			     (isneg.rep rep)
			     (eql 1
				  (cardinality.ns (all-consequents signature)))
			     (eq newnode *node*))))
	  (addsupport.n support newnode)
	  (snebr:ck-contradiction newnode (context.ch ch) 'sneps:snip))
	(make.rep newsub
		  support
		  (sign.rep rep)
		  signature
		  newnode
		  (context.rep rep))))))
;
;
; =============================================================================
;
; fill-out-substitution
; ---------------------
;
;       arguments     : rsub - <substitution>
;                       dest - <node>
;
;       returns       : <substitution>
;
;       description   : adds to a switched report any additional bindings
;                        which may apply to the destination nodefun "dest"
;
;                                        written :  rgh 07/30/85
;                                        modified:  rgh 08/05/85
;
;
(defun fill-out-substitution (rsub dest)
  (let ((result (new.sbst)))
    (do.sbst (mb rsub result)
       (when (dominates.n dest (mvar.mb mb))
	 (setq result (putin.sbst mb result))))))
;
;
; =============================================================================
;
; install-channel
; ---------------
;
;       arguments     : ch - <channel>
;
;       returns       : <channel set>
;
;       nonlocal-vars : OUTGOING-CHANNELS:
;
;       description   : if "ch" is not already in OUTGOING-CHANNELS:, it
;                       is put in.  If "ch" is in OUTGOING-CHANNELS:, but
;                       is closed, it is reopened.
;
;       side-effects  : updates the OUTGOING-CHANNELS: register of the
;                       current node.
;
;                                        written :  rgh 10/06/85
;                                        modified:  rgh 11/30/85
;                                                   rgh  4/13/86
;
;
(defun install-channel (ch)
  (let ((dest (destination.ch ch)))
    (setq *OUTGOING-CHANNELS* (insert.chset ch *OUTGOING-CHANNELS*))
    (unless (is-user.dest dest) (activate.n dest))))


;
;                        ESTA FUNCAO e' PARA SAIR DAQUI
; =============================================================================
;
; instanciated-quantified-vars.n 
; ------------------------------
;
;       arguments     : node  - <node>
;                       ch    - <channel>
;                       subst - <substitution>
;
;       returns       : <boolean>
;
;       description   : if "ch" does not "instanciate" any quantified
;                       variable of 'node' returns NIL, otherwise
;                       returns T.
;
;                                        written :  njm  11/24/88
;                                        modified:  
;                                                   
;
;
(defun instanciated-quantified-vars.n (node ch subst)
  (let ((vars (quantified-vars.n node)))
    (when vars
      (or (do.ns (var vars nil)
	    (when (match:bindingOF var (filter.ch ch))
	      (return t)))
	  (do.ns (var vars nil)
	    (when (match:bindingOF var subst)
	      (return t)))))))

;
;
; =============================================================================


;
;                        ESTA FUNCAO e' PARA SAIR DAQUI
; =============================================================================
;
; nor-to-nor-with-different-args  
; ------------------------------
;
;       arguments     : n1 - <node>
;                       n2 - <node>
;
;       returns       : <boolean>
;
;       description   : if "n1" and "n2" are NOR nodes with different number of
;                       arguments returns T, otherwise returns T.
;
;                                        written :  njm  11/25/88
;                                        modified:  
;                                                   
;
;
(defun nor-to-nor-with-different-args  (n1 n2)
  (and (is-nor.n n1)
       (is-nor.n n2)
       (not (eql (cardinality.ns (nodeset.n n1 'sneps:arg))
		 (cardinality.ns (nodeset.n n2 'sneps:arg))))))



    
    




