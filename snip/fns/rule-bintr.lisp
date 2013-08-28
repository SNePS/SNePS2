;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: rule-bintr.lisp,v 1.2 2013/08/28 19:07:28 shapiro Exp $

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
;  ****************************************************************************
;
;             ESTE FICHEIRO CONTEM FUNCOES A DISTRIBUIR POR OUTROS
;
;  ****************************************************************************
;
; =============================================================================
;
; is-case-of-introduction
; -----------------------
;
;       arguments     : ch - <channel>
;
;       returns       : <boolean>
;
;       description   : 
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  scs/flj 6/27/06
;
;
(defun is-case-of-introduction (ch)
  (and (isrule)
       (or (is-from-user.req ch)
	   (is-rule-to-ant.req ch)
	   (is-rule-to-cq.req ch))))
;
;
; =============================================================================
;
;
; isrule
; ------
;                                             
;       arguments     : none
;
;       returns       : <boolean>
;
;       description   : if *NAME* of *NODE* is rule and the its *TYPE* is 
;                       AND, NOR, OR-ENTAILMENT or AND-ENTAILMENT it returns
;                       true; NIL otherwise
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  njm 11/07/88
;
;
(defun isrule ()
  (declare (special *NAME*))
  (and (equal *NAME* 'SNIP::RULE)
       (member *TYPE* '(AND NOR OR-ENTAILMENT AND-ENTAILMENT))))

; =============================================================================
;
;  
;
; initiate-introduction.rule
; --------------------------
;                                             
;       arguments     : ch - <channel>
;
;       returns       : 
;
;       description   : 
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  njm 11/08/88
;                        
;
(defun initiate-introduction.rule (ch)
  (cond ((sneps:quantified-vars.n *NODE*)
	 ;; Can't do univeral/existental intro yet, hence 
	 ;; avoid this business of asserting pattern nodes
	 nil)
	(t (case *TYPE*
	     (OR-ENTAILMENT (initiate-vent-introduction ch))
	     (AND-ENTAILMENT (initiate-&ent-introduction ch))
	     (AND (initiate-introduction-without-hypothesis ch))
	     (NOR (initiate-introduction-without-hypothesis ch))))))
;
;
; =============================================================================
;
;  
;
; initiate-&ent-introduction
; --------------------------
;                                             
;       arguments     : ch - <channel>
;
;       returns       : 
;
;       description   : 
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  hc  10/01/92
;
;
(defun initiate-&ent-introduction (ch)
  (let* ((substituted-ants
	  (let (subst-ant (subst-ants (new.ns)))
	    (do.ns (ant (antecedents *NODE* nil) subst-ants)
	      (setq subst-ant (match::applysubst ant (filter.ch ch)))
	      (cond ((ismol.n subst-ant)
		     (setq subst-ants (insert.ns subst-ant subst-ants)))
		    ;; If any of the antecedents remains a pattern node even
		    ;; after substitution we cannot do the introduction:
		    (t (return-from initiate-&ent-introduction nil))))))
	 (aants (assert-all-ants substituted-ants))
	 (cqts (consequents-ent *NODE*))
	 (extended-ct (fullbuildcontext aants (makeone.cts (context.ch ch))))
	 (ich (install-introduction-channel ch aants cqts))
	 (restr (make.restr (filter.ch ch))))
    (when-intensional-contexts
     ;; To do this right we would need to link the hypothetical context to
     ;; its super context so nodes added later to the supercontext will get
     ;; inherited properly
     (let ((extended-ct-name
	    (gentemp "C-&=>-INTRO-" (find-package 'snepsul))))
       (sneps:name.ct extended-ct extended-ct-name)
       (setq extended-ct extended-ct-name)))
    (remark "~%Let me assume that" (makeone.ns (choose.ns aants)) restr)
    (do.ns (aant (others.ns aants))
      (remark "and  assume  that " (makeone.ns aant) restr))
    (send-cq-requests.rule ich extended-ct)))
;
; =============================================================================
;
;  
;
; initiate-vent-introduction
; --------------------------
;                                             
;       arguments     : ch - <channel>
;    
;       returns       : 
;
;       description   : 
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  njm 11/08/88
;                                                    hc 10/01/92
;
(defun initiate-vent-introduction (ch)
  (let ((ants (antecedents *node* nil))
	(cqts (consequents-ent *node*))
	(restr (make.restr (filter.ch ch))))
    ;; This introduction rule, as written,
    ;;    is P1=>Q |- {P1, ..., Pn} v=> Q
    ;;    which is unsound unless there is only one antecedent.
    ;; So only use it if there is only one antecedent.
    (when (cl:= (cardinality.ns ants) 1)
      (do.ns (ant ants)
	     (let ((substituted-ant
		    (match::applysubst ant (filter.ch ch))))
	       ;; Check that the current antecedent is not a pattern node, if it is
	       ;; we cannot start an introduction for this particular antecedent:
	       (when (ismol.n substituted-ant)
		 (let ((ct (sneps:getcontext (new.ns)))
		       (crntct (gentemp "CvANT" (find-package 'snepsul))))
		   (declare (special crntct))
		   (sneps::name.ct ct crntct)
		   (let* ((aant (sneps:assert.n substituted-ant))
			  (ich (install-introduction-channel
				ch
				(makeone.ns aant) cqts))
			  (extended-ct
			   (fullbuildcontext
			    (makeone.ns aant) (makeone.cts (context.ch ch)))))
		     (when-intensional-contexts
		      ;; To do this right we would need to link the
		      ;; hypothetical context to its super context so nodes
		      ;; added later to the supercontext will get inherited
		      ;; properly
		      (let ((extended-ct-name
			     (gentemp "C-v=>-INTRO-" (find-package 'snepsul))))
			(sneps:name.ct extended-ct extended-ct-name)
			(setq extended-ct extended-ct-name)))
		     (remark "~%Let me assume that" (makeone.ns aant) restr)
		     (send-cq-requests.rule ich extended-ct)))))))))
      
  
;
; =============================================================================
;
;  
;
; initiate-introduction-without-hypothesis
; ----------------------------------------
;                                             
;       arguments     : ch - <channel>
;
;       returns       : 
;
;       description   : 
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  
;
;
(defun initiate-introduction-without-hypothesis (ch)
  (let* ((cqts (all-consequents *NODE*))
	 (ctx (context.ch ch))
	 (ich (install-introduction-channel ch NIL cqts)))
    (send-cq-requests.rule ich ctx)))
;
;
; =============================================================================;  
;
; assert-all-ants
; ---------------
;                                             
;       arguments     : 
;
;       returns       : 
;
;       description   : 
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  njm 11/07/88
;                                        modified:  scs 03/31/08
;
;
(defun assert-all-ants (ants)
  "Assert all ants in a new context with a new name."
  (let ((ct (sneps:getcontext (new.ns)))
	 (crntct (gentemp "C&ANT" (find-package 'snepsul))))
    (declare (special crntct))
    (sneps::name.ct ct crntct)
    (do.ns (ant ants ants)
      (sneps:assert.n ant))))
;
; =============================================================================
;
;  
;
; send-cq-requests.rule
; ---------------------
;                                             
;       arguments     : 
;
;       returns       : 
;
;       description   : 
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  njm 11/07/88
;
;
(defun send-cq-requests.rule (ich extended-ct)
  (let* ((ch (channel.ich ich))
	 (cqts (consequents.ich ich)))                  
    (setq *INTRODUCTION-CHANNELS*
	  (update.ichset (make.ich ch
				   (context.ich ich)
				   cqts                            
				   (request-all (ruiset.ich ich)))
			 *INTRODUCTION-CHANNELS*))
    (broadcast-cq-request  ch
                           cqts
			   extended-ct))) 


;
; =============================================================================
;                                      
;                                               
;
; broadcast-cq-request
; --------------------
;                                             
;       arguments     : 
;
;       returns       : 
;
;       description   : 
;
;
;                                        written :  cpf 11/04/88
;                                        modified:  
;
;
(defun broadcast-cq-request (req ns ctx)
  (do.ns (n ns t)
    (send-introduction-request
      (make.ch (restrict-binding-to-pat (filter.ch req) n)
	       (new.sbst)
	       ctx
	       *NODE*
	       'OPEN)
      n)))
;
;
; =============================================================================
;
; send-introduction-request
; -------------------------
;
;       arguments     : req - <request>
;                       n - <node>
;                       
;       nonlocal-vars : the *REQUESTS* register of the activation process of
;                       the node "n" of the current node process
;
;       description   : actually sends the (request) message "req" to the
;                       node "n".  
;
;       side-effects  : inserts "req" into the *REQUESTS* register of "n"
;
;
;                                        written :  njm/cpf 11/07/88
;
(defun send-introduction-request (req n)
  (let (pr)
    (activate.n n)
    (setq pr (activation.n n))
    (regstore pr
	      '*REQUESTS*
	      (insert.chset req (regfetch pr '*REQUESTS*)))
    (regstore pr '*PRIORITY* 'LOW)
    (initiate pr)))
                     
;                  
; =============================================================================
;
; install-introduction-channel
; ----------------------------
;
;       arguments     : 
;                       
;
;       returns       : 
;
;       nonlocal-vars : *INTRODUCTION-CHANNELS*, *NODE*, INCOMING-CHANNELS:
;
;       description   : If there is not already an existing introduction channel
;                       whose channel is "request", one is inserted in
;                       *INTRODUCTION-CHANNELS*.  If there is one already, but it
;                       is closed, it is reopened.  A similar modification is
;                       made to the INCOMING-CHANNELS: register of the node
;                       which is the destination of the channel.  The newly
;                       inserted or reopened ich-channel is returned.
;
;       side-effects  : updates the registers mentioned above
;
;                                        written :  cpf 11/04/88
;                                        modified:  njm 11/07/88
;                                                   
;
;
;
(defun install-introduction-channel (request ants cqts)
  (let* ((dest (destination.ch request))
	 (destsub (unless (is-from-user.req request)
		    (restrict-binding-to-pat (filter.ch request) dest)
		    (filter.ch request)))
	 (ich (make.ich (make.ch destsub
				 (switch.ch request)
				 (context.ch request)
				 dest
				 (valve.ch request))
			ants
			cqts
			(makeone.ruis (make.rui destsub 0 0 (nodeset-to-fnodeset cqts) nil)))))
    (setq *INTRODUCTION-CHANNELS* (insert.ichset ich *INTRODUCTION-CHANNELS*))
    (unless (is-from-user.req request)
      (activate.n dest))
    ich))
;                     
;
; =============================================================================
;
; consequents-ent
; ---------------
;
;       arguments     : rule - <node>
;                
;
;       returns       : <node set>
;
;       description   : returns the set of all nodes which are in consequent
;                       position relative to "ant" in "rule"
;
;                                        written :  cpf 11/04/88
;                                        modified:  
;                                        modified:  
;
;
(defun consequents-ent (rule)
  (union.ns (nodeset.n rule 'cq) (nodeset.n rule 'dcq)))

;
;
; =============================================================================
;
; not-working-on-introdution
; --------------------------
;
;       arguments     : restr - <restriction>
;                       ct    - <context>
;
;       returns       : <boolean>
;
;       nonlocal-vars : the *INTRODUCTION-CHANNELS* register of the current node
;
;       description   : returns "true" if there is an introction-channel to the
;                       current node which will supply instances satisfying
;                       the restriction "restr" in the context "ct"; "false"
;                       otherwise.
;
;                                        written :  njm 11/10/88
;                                        modified:  
;                                                   
;   
;
(defun not-working-on-introdution (restr ct)
  (do.set (ich *INTRODUCTION-CHANNELS* t)
    (let ((ch (channel.ich ich)))
      (when (and (equivalent.restr restr (ch-to-restr ch))
		 (issubset.ct ct (context.ch ch)))
	(return nil)))))
;
;
; =============================================================================



    
    




