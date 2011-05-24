;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEBR; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: contrad.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :snebr)


;
;
; =============================================================================
;
;
; ck-contradiction
; ----------------
;
;
;       arguments     : newnode - <node>
;                       context - <context>
;                       flag    - 'ASSERTION | 'SNIP
;
;       returns       : <node> | ----- 
;
;       description   : Takes as arguments a node (newnode) and a context 
;                       (context) and checks whether newnode contradicts any
;                       other node in the network. 
;                       If the node contradicts a node in the network then 
;                       the restriction of the contexts are updatted. 
;                       If the contradicted node belongs to the belief space  
;                       defined by context then the user is called
;                       to solve the contradiction. 
;
;       Algorithm     : In order for a contradiction to exist there must exist
;                       a node in the network that either is negated by 
;                       newnode  node or negates newnode.
;
;                        1. If there exists a node that negates newnode then
;                           we look at the node (or nodes) that negate it.
;                        2. If there exists a node negated by newnode then 
;                           we look at such node.
;
;
;                                  written :  jpm    11/09/82
;                                  modified:  njm    10/06/88
;                                  modified:  mrc    10/28/88
;                                  modified:  flj&hi 8/18/99
;                                  modified:  scs/flj 6/28/04
;
;
;;; 6/04 Change to call sneps:contradictory-nodes.n instead of negation-or-negated-nd
(defun ck-contradiction (newnode context flag)
  ;; Temporarily, this function calls SNeBR with only
  ;;    the first contradictory node it finds.
  ;; This agrees with the old ck-contradiction, which assumed
  ;;    that there was only one
  (let ((contr-nodes (sneps:contradictory-nodes.n newnode)))
    (sneps:do.ns (contr-nd contr-nodes)
      (when (and contr-nd
		 (exists-contradiction contr-nd context))
	(ck-contradiction-1 newnode contr-nd context flag)))
    newnode))

(defun ck-contradiction-1 (newnode contr-nd context flag)
  (let ((contrsup (sneps:ctcs-to-cts (sneps:node-asupport contr-nd))))
    (update-contexts (ctcs-to-cts (sneps:node-asupport newnode)) contrsup)
    (sneps:mark-inconsistent context) 
    (when (and (isassert.n newnode) (isassert.n contr-nd)
               (not (sneps::context-okinconsistent context)))
      (if (eq flag 'sneps:assertion)
    	  (sneps-contr-handler newnode contr-nd)
      	(snip-contr-handler newnode contr-nd context)))
    (when (and (isassert.n newnode) (isassert.n contr-nd))
      (ok-update-contexts (ctcs-to-cts (sneps:node-asupport newnode)) contrsup)
      (sneps:mark-okinconsistent context))))
      
;
; =============================================================================
;
;
; negation-or-negated-nd 
; ----------------------
;
;
;       arguments     : newnode - <node>
;
;       returns       : <node> | NIL 
;
;       description   : Returns the node in the network that contradicts 
;                       `newnode', if it exists.
;
;       Algorithm     : In order for a contradiction node to exist there must
;                       exist a node in the network that either is negated by 
;                       newnode node or negates newnode.
;
;                        1. If there exists a node that negates newnode then
;                           we look at the node (or nodes) that negate it.
;                        2. If there exists a node negated by newnode then 
;                           we look at such node.
;
;
;                                  written :  jpm  11/09/82
;                                  modified:  njm  10/06/88
;
;
;
;
(defun negation-or-negated-nd  (newnode)
  (cond ((is-negated newnode) (negator-of newnode))
	((is-negation newnode) (negated-by newnode))
	(t nil))) 


;
;
; =============================================================================
;
;
; exists-contradiction  
; --------------------
;
;
;       arguments     : contr-nd - <node>
;                       context  - <context>
;
;       returns       : <boolean> 
;
;       description   : 
;
;
;                                  written :  njm  10/06/88
;                                  modified:  
;
;
;
;
(defun exists-contradiction (contr-nd context)
  (let ((sneps:crntct context))
    (declare (special sneps:crntct))
    (isassert.n contr-nd)))

;
; =============================================================================
;
; negator-of
; ----------
;
;
;       arguments     : node - <node>
;
;       returns       : <node> |  NIL
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun negator-of (node)
  (negator-of-1 (nodeset.n node 'arg-)))


(defun negator-of-1 (ns)
  (cond ((isnew.ns ns) nil)
	((is-negation (choose.ns ns)) (choose.ns ns))
	(t (negator-of-1 (others.ns ns)))))


;
; =============================================================================
;
; negated-by
; ----------
;
;
;       arguments     : node - <node>
;
;       returns       : <node set>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun negated-by (node)
  (choose.ns (nodeset.n node 'arg)))



;
; =============================================================================
;
;
; is-negated 
; ----------
;
;
;       arguments     : node - <node>
;
;       returns       : <boolean>
;
;       description   : Takes as argument a node and returns T is the node is
;                       negated and returns () otherwise.
;                       Uses the auxiliary function is-negated-1.
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun is-negated (n)
  (is-negated-1 (nodeset.n n 'arg-)))


(defun is-negated-1 (ns)
  (cond ((isnew.ns ns) nil)
	((is-negation (choose.ns ns)))
	(t (is-negated-1 (others.ns ns)))))


;
; =============================================================================
;
;
; is-negation 
; -----------
;
;
;       arguments     : node - <node>
;
;       returns       : <boolean>
;
;       description   : Takes as argument a node and returns T if the node is
;                       negating any other node and returns () otherwise.
;
;       
;
;
;                                  written :  jpm 11/30/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun is-negation (n)
  (and (nodeset.n n 'max)
       (eq (nodeaccess (choose.ns (nodeset.n n 'max)))
	   'snepsul::|0|)))


;
; =============================================================================
;
;
; ck-context-contradiction
; ------------------------
;
;
;       arguments     : newct - <context>>
;                       flag    - 'SET-CONTEXT | 'ADD-TO-CONTEXT
;
;       returns       : T | NIL 
;
;       description   : Takes as argument  a context 'newct' and
;                       checks if it is an inconsistent context.
;                       If so, it reports the inconsistency to
;                       the user, and returns T. Otherwise, it 
;                       returns NIL.
;
;
;                                  written :  mrc  10/20/88
;
;
;
(defun ck-inconsistency (newct)
  (if (sneps:isinconsis.ct newct)
      (sneps-inconsis-handler newct)))

;
; =============================================================================
;
; sneps-inconsis-handler 
; ----------------------
; 
;
;       arguments     :  ct  - <context>
;
;       returns       : 
;
;       description   : This function takes as argument a context 'ct', which
;                       is inconsistent, reports this inconsistency to the
;                       user, and gives the user two options (see 
;                       sneps-inconsis-options).
; 
;
;                                  written :  mrc  10/20/88
;                                  modified:  
;
;
;                                                
;
(defun sneps-inconsis-handler (ct)
  (declare (special sneps:outunit sneps:crntct))
  (progn (format sneps:outunit
		 "~%~%~T The context ~S ~
                   ~%~T  that you are trying to build is inconsistent."
		 (context-hyps ct))
	 (sneps-inconsis-options)))


;
; =============================================================================
;
;
; sneps-inconsis-options  
; ----------------------
;
;
;       arguments     : ----
;
;       returns       : ----
;
;       description   : Prints two options and reads the user's choice
;                       (see read-sneps-inconsis-option).
;
;       
;
;
;                                  written :  mrc 10/20/88 
;                                  modified:  
;
;
;
(defun sneps-inconsis-options  ()
  (declare (special sneps:outunit))
  (format sneps:outunit
	  "~%~%~%~T You have the following options:~
	   ~%~T~T 1. [y]es, create the context anyway, knowing that a contradiction is ~
                     derivable;~
	   ~%~T~T 2. [n]o,don't create the context.~
           ~%~T (please type y or n)")
  (not (user-says-yes)))
