;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: rule-requests.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; process-requests.rule
; ---------------------
;
;       nonlocal-vars : various registers of the current *NODE*
;                         *NODE*, *REQUESTS*
;
;       description   : processes all requests received by the current node
;
;       side-effects  : registers affected:  *REQUESTS*, OUTGOING-CHANNELS:,
;                         *RULE-USE-CHANNELS*
;
;       implementation: 
;
;                                        written :  rgh  2/02/86
;                                        modified:  rgh  2/09/86
;                                                   rgh  2/22/86
;                                                   scs  4/22/88
;                                                   ssc  5/10/89
;
;
(defun process-requests.rule ()
  (let ((remark-sent nil) pathfrom-result)
    (declare (special pathfrom-result remark-sent))
    (do.set (ch *REQUESTS*)
      (if (is-cq-to-rule.req ch)
	  (process-rule-use-request.rule ch)
	  (process-one-request.non-rule ch)))
    (setq *REQUESTS* (new.chset))))
;
;
; =============================================================================
;
; antecedents
; -----------
;
;       arguments     : rule - <node>
;                       cq - <node>
;
;       returns       : <node set>
;
;       description   : returns a <node set> of those nodes which are in
;                       rule antecedent position relative to "cq"
;
;                                        written :  rgh  3/31/86
;                                        modified:  scs  4/22/88
;                                        modified:  scs  6/21/88
;                                                     dk 6/2/93
;  a DO arc is an antecedent of a DO-IF transformer and the WHEN of WHEN-DO
;
;                                                     hi 3/31/99
;  same for whenever-do. hi
;
(defun antecedents (rule cq)
  "Returns a <node set> of nodes in antecedent position relative to cq in the rule."
  (cond ((is-and.n rule) nil)
	((is-nor.n rule) nil)
	(t (or (nodeset.n rule 'sneps::ant)
	       (nodeset.n rule 'sneps::&ant)
	       (when (nodeset.n rule 'sneps::if)
		     (nodeset.n rule 'sneps::do))
	       (nodeset.n rule 'sneps::when)
	       (nodeset.n rule 'sneps::whenever)
	       (remove.ns cq (nodeset.n rule 'sneps::arg))))))
;
;
; =============================================================================
;
; process-rule-use-request.rule
; ------------------------------
;
;       arguments     : request - <request>
;
;       nonlocal-vars : *RULE-USE-CHANNELS*, *NODE*, *USABILITY-TEST* registers
;
;       description   : handles a request from a consequent of the current
;                        rule node
;
;       side-effects  : registers affected:  *RULE-USE-CHANNELS* of the
;                        current node, and INCOMING-CHANNELS: of the
;                         consequent node
;
;       implementation: 
;
;                                        written :  rgh  2/02/86
;                                        modified:  rgh  2/09/86
;                                                   rgh  3/31/86
;                                                   rgh  4/03/86
;                                                   njm 10/24/88
;                                                   njm 11/11/88
;                                                   choi 9/19/90
;
(defun process-rule-use-request.rule (request)
  ;;"Modified to facilitate knowledge shadowing by using the instance set."
  (let* ((cqch (install-rule-use-channel request))
	 (sup (sneps:node-asupport *NODE*))
	 (filter (filter.ch request))
	 (dest (destination.ch request))
	 (crntct (context.ch request)))
    (declare (special crntct))
    (cond ((and (isassert.n *NODE*) (funcall *USABILITY-TEST* 'POS))
           (cond ((member *TYPE* '(AND NOR))
		  (funcall *RULE-HANDLER*
			   (make.rep filter sup 'POS dest nil crntct)
			   cqch))
		 ((eq *TYPE* 'DO-IF)
		  (funcall *RULE-HANDLER* (antecedents *NODE* dest) request))
		 (t (send-ant-requests.rule cqch))))
	  (t (let ((rule-sbst 
		    (restrict-binding-to-pat (filter.ch request) *NODE*))
		   (restr nil))
	       ;; the current rule (pattern) is activated only when 
	       ;; there is no instance for this rule
	       (when (or (is-wh-question rule-sbst)
			 (not (apply-known-rule-instance cqch)))
		 (setq restr (make.restr rule-sbst))
		 (when (not-working-on restr crntct request)
		   (send-rule-use-requests restr crntct dest))))))))


(defun apply-known-rule-instance (cqch)
  ;;"Check if there is any known rule instance to the given cqch."
  (let ((ch (channel.cqch cqch))
	(is-there-instance nil))
    (do.set (instance *KNOWN-INSTANCES*)
       (when (acceptable-rule-instances instance ch)
	 (setq is-there-instance t)
	 (if (eq *TYPE* 'num-quantifier)
	   (send-ant-requests.rule cqch))))
    is-there-instance))

;
;
; =============================================================================
;
; install-rule-use-channel
; ------------------------
;
;       arguments     : request - <channel>
;                       ants - <node set>
;
;       returns       : <cq-channel>
;
;       nonlocal-vars : *RULE-USE-CHANNELS*, *NODE*, INCOMING-CHANNELS:
;
;       description   : If there is not already an existing rule use channel
;                       whose channel is "request", one is inserted in
;                       *RULE-USE-CHANNELS*.  If there is one already, but it
;                       is closed, it is reopened.  A similar modification is
;                       made to the INCOMING-CHANNELS: register of the node
;                       which is the destination of the channel.  The newly
;                       inserted or reopened cq-channel is returned.
;
;       side-effects  : updates the registers mentioned above
;
;                                        written :  rgh  2/02/86
;                                        modified:  rgh  2/09/86
;                                                   rgh  3/31/86
;                                                   rgh  4/03/86
;                                                   rgh  4/13/86
;                                                   scs  6/20/88
;                                                   njm 10/24/88
;
;
;
(defun install-rule-use-channel (request)
  ;;"Build a new cqch for a request, and insert it to the
  ;; *RULE-USE-CHANNELS* (ruc) register of *NODE*.
  ;;     <ruc> = ( <cqch> <cqch> .... )
  ;;     <cqch> = ( <channel> <antecedents> <ruiset> )   
  ;; Three kinds of data structures for <ruiset>:
  ;;     <ruiset> =  <ruis> | <ptree-ruiset> | <sindex-ruiset>     
  ;; The data structure of <ruiset> for the linear RUI set method:
  ;;     <ruis> = ( <rui> <rui> .... )
  ;;     <rui> = ( <sbst> <poscount> <negcount> <fns> <remark-flag> ) 
  ;; The data structure of <ruiset> for P-tree:
  ;;     <ptree-ruiset> = (<ptree> <adj-node-list> <pnode-ruiset>)
  ;;     <adj-node-list> = ((<pnode> <pnode>) (<pnode> <pnode>) ....)
  ;;           'adj-node-list' contains adjacent ptree node pairs
  ;;     <pnode-ruiset> = ((<pnode> <ruis>) (<pnode> <ruis>) ....)
  ;;          in 'pnode-ruiset', each ptree node is associated with its
  ;;          corresponding ruiset.  'pnode-ruiset' is initially empty.
  ;; The data structure of <ruiset> for S-indexing:
  ;;    <sindex-ruiset> = ((<sindex> <rui>) (<sindex> <rui>) ...)
  ;;          'sindex-ruiset' is initially empty.                   "

  (let* ((dest (destination.ch request))
	 (destsub (restrict-binding-to-pat (filter.ch request) dest))
	 (ch (make.ch destsub
		      (switch.ch request)
		      (context.ch request)
		      dest
		      (valve.ch request)))
	 (ants (antecedents *NODE* dest))
	 cqch)
    (cond
     
     ;; P-tree is built for numerical quantifier rules even if all
     ;; antecedents have the same set of variables, mainly because 
     ;; they have the characteristics of both conjunctiveness and 
     ;; non-conjunctiveness.
     ((eq *TYPE* 'num-quantifier)
      (let* ((cqs (nodeset.n *NODE* 'sneps::cq))
	     (ptree (ptree-for-num-quant ants cqs))
	     (adj-node-list (ptree-to-adj-list ptree))
	     (ptree-ruiset (list ptree adj-node-list nil)))
	(setq cqch (make.cqch ch (append ants cqs) ptree-ruiset))))

      ;; if a rule's antecedents have the same set of variables,
      ;; apply the S-indexing method.
      ((or (null ants) (is-all-pat-same-vars ants))
       (setq cqch (make.cqch ch ants nil)))
      
      ;; P-tree is built for and-entailment rules whose antecedents
      ;; do not have the same set of variables.
      ((eq *TYPE* 'and-entailment)
       (let* ((ptree (ptree-for-and-ent ants))
	      (adj-node-list (ptree-to-adj-list ptree))
	      (ptree-ruiset (list ptree adj-node-list nil)))
	 (setq cqch (make.cqch ch ants ptree-ruiset))))

      ;; a linear ruiset is built for non-conjunctive rules whose
      ;; antecedents do not have the same set of variables.
      (t (let* ((fants (nodeset-to-fnodeset ants))
		(rui (make.rui destsub 0 0 fants nil)))
	   (setq cqch (make.cqch ch ants (makeone.ruis rui))))))
    
    (setq *RULE-USE-CHANNELS* (insert.cqchset cqch *RULE-USE-CHANNELS*))
    (activate.n dest)
    cqch))


(defun is-all-pat-same-vars (pat-list)
  ;;"check if all patterns have the same set of variables"
  (if (null pat-list) t
  (let ((var-list (sneps::all-vars.n (first pat-list)))
	(result t))
    (do* ((pl pat-list (rest pl))
	  (pat (first pl) (first pl)))
	 ((or (null pl) (null result)))
      ;; Should use `iseq.ns' instead of `equiv-set', but I don't know
      ;; whether everybody builds proper variable node sets, hence, 
      ;; for safety I'll keep this kludge (hc, Jul-18-94).
      (if (not (equiv-set (sneps::all-vars.n pat) var-list))
	  (setq result nil)))
    result)))


(defun equiv-set (s1 s2)
  "test if two sets have the same elements"
  (and (subsetp s1 s2)
       (subsetp s2 s1)))

;
;
; =============================================================================
;
; apply-known-instances.rule
; --------------------------
;
;       arguments     : cqch - <cq-channel>
;
;       returns       : <boolean>
;
;       nonlocal-vars : *KNOWN-INSTANCES* register
;                       any-instances-applied
;
;       description   : applies known rule instances to the given "cqch",
;                       returning "true" if any are successfully applied,
;                       and "false" otherwise.
;
;                                        written :  rgh  3/31/86
;                                        modified:  rgh  4/03/86
;
;
(defun apply-known-instances.rule (cqch)
  (declare (special any-instances-applied))
  (do ((instances *KNOWN-INSTANCES* (others.iset instances)))
      ((isnew.iset instances))
    (when (try-to-apply-instance.rule (choose.iset instances) cqch)
      (setq any-instances-applied t))))
;
;
; =============================================================================



    
    




