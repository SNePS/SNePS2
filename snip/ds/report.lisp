;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: report.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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
; <report> ::= ( <substitution> <support> <sign> <signature> <node> <ct> )
;
; Where <ct> ::=  <context> | 'NIL
;
; A <report> is a report that a certain instance of a supported node N or of the
; negation of N is asserted in the network.
; <signature> is the node sending the report.
; When a report is sent into a channel, <node> is NIL.
; When a report emerges from a channel, and/or is placed in a *REPORTS* register,
;      If <sign> is POS, then <node> is the <substitution> instance of N;
;      If <sign> is NEG, then <node> is the <substitution> instance of the negation of N.
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.rep              :  <universal> --> <boolean>
;                is-ant-to-rule.rep  :  <report> --> <boolean>
;                is-rule-to-cq.rep   :  <report> --> <boolean>
;                is-node-to-node.rep :  <report> --> <boolean>
;
; CONSTRUCTORS   make.rep    :  <substitution> x <support> x <sign> x <signature> 
;                                  x <node> x [<context>] --> <report>  
;
; SELECTORS      subst.rep     :  <report> --> <substitution>
;                support.rep   :  <report> --> <support>
;                sign.rep      :  <report> --> <sign>
;                signature.rep :  <report> --> <signature>
;                node.rep      :  <report> --> <node>
;                context.rep   :  <report> --> <context>
;
; UTILITY        addbinding.rep :  <mbind> x <report> --> <report>
;
; TESTS          iseq.rep  :  <report> x <report> --> <boolean>
;                ispos.rep :  <report> --> <boolean>
;                isneg.rep :  <report> --> <boolean>
;                hasct.rep :  <report> --> <boolean>
;
; =============================================================================
;


;
; =============================================================================
;
; make.rep
; --------
;
;       arguments     : subst - <substitution>
;                       sup - <support>
;                       sign - <sign>
;                       sig - <signature>
;                       n - <node>
;                       ct - <context> 
;
;       returns       : <report>
;
;       description   : returns a <report> consisting of the components
;                        passed as arguments
;
;                                        written :  rgh  7/27/85
;                                        modified:  rgh  3/09/86
;                                        modified:  cpf 10/07/88
;
(defun make.rep (subst sup sign sig n ct)
  (vector subst sup sign sig n ct))
; 
;
; =============================================================================
;
; subst.rep
; ---------
;
;       arguments     : report  -  <report>
;
;       returns       : <substitution>
;
;       description   : selects the <substitution> of the report
;
;                                        written :  rgh 05/09/85
;                                        modified:  rgh 06/11/85
;
;
(defun subst.rep (report)
   (svref report 0))
;
;
; =============================================================================
;
; support.rep
; -----------
;
;       arguments     : report  -  <report>
;
;       returns       : <support>
;
;       description   : selects the <support> of the report
;
;                                        written :  cpf 10/07/88
;                                        modified:  
;
;
(defun support.rep (report)
  (svref report 1))
;
;
; =============================================================================
;
; sign.rep
; --------
;
;       arguments     : report - <report>
;
;       returns       : <sign>
;
;       description   : selects the <sign> of the report
;
;                                        written :  rgh 07/24/85
;                                        modified:  cpf 10/07/88
;
;
(defun sign.rep (report)
   (svref report 2))
;
;
; =============================================================================
;
; signature.rep
; -------------
;
;       arguments     : report  -  <report>
;
;       returns       : <signature>  ( = <node> )
;
;       description   : selects the <signature> of the report
;
;                                        written :  rgh 05/09/85
;                                        modified:  rgh 06/11/85
;                                        modified:  cpf 10/07/88
;
(defun signature.rep (report)
   (svref report 3))
;
;
; =============================================================================
;
; node.rep
; --------
;
;       arguments     : report  -  <report>
;
;       returns       : <node>
;
;       description   : selects the <node> of the report
;
;                                        written :  rgh  3/09/86
;                                        modified:  cpf 10/07/88
;
;
(defun node.rep (report)
  (svref report 4))
;
;
; =============================================================================
;
; context.rep
; -----------
;
;       arguments     : report  -  <report>
;
;       returns       : <context>
;
;       description   : selects the <node> of the report
;
;                                        written :  rgh  3/09/86
;                                        modified:  cpf 10/07/88
;
;
(defun context.rep (report)
  (svref report 5))
;
; =============================================================================
;
; is.rep
; ------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "u" is a <report>,
;                               "false" otherwise
;
;                                        written :  rgh  7/27/85
;                                        modified:  rgh  3/09/86
;                                        modified:  cpf 10/07/88
;
(defun is.rep (u)
  (and (vectorp u)
       (is.sbst (first u))
       (is.sup (second u))
       (is.sign (third u))
       (is.sig (fourth u))
       (or (null (svref u 4)) (is.n (svref u 4)))))
;
;

; =============================================================================
;
; is-ant-to-rule.rep
; ------------------
;
;       arguments     : report  -  <report>
;
;       returns       : <boolean>
;
;       description   : determines whether or not the report is from a node
;                       in antecedent position to a dominating rule node
;
;       implementation: checks to see if the report was sent by a node which
;                       is at the end of a ant, arg, or &ant arc, since these
;                       are the only arcs which point to the antecedent of a
;                       rule
;
;                                        written :  rgh  6/11/85
;                                        modified:  rgh  4/20/86
;                                                    hi  3/31/99
;
(defun is-ant-to-rule.rep (report)
  (let ((sender (signature.rep report)))
    (and (is.n sender)
	 (or (ismemb.ns sender (nodeset.n *NODE* 'ant))
	     (ismemb.ns sender (nodeset.n *NODE* 'arg))
	     (ismemb.ns sender (nodeset.n *NODE* 'when))
	     (ismemb.ns sender (nodeset.n *NODE* 'whenever))
	     (ismemb.ns sender (nodeset.n *NODE* '&ant))))))
;
;; =============================================================================
;
; is-cq-to-rule.rep
; -----------------
;
;       arguments     : report  -  <report>
;
;       returns       : <boolean>
;
;       description   : determines whether or not the report is from a node
;                       in consequent position to a dominating rule node
;
;       implementation: checks to see if the report was sent by a node which
;                       is at the end of a cq, arg, or dcq arc, since these
;                       are the only arcs which point to the antecedent of a
;                       rule
;
;                                        written :  njm/cpf 11/04/88
;                                        modified:  
;
;
(defun is-cq-to-rule.rep (report)
  (let ((sender (signature.rep report)))
    (and (is.n sender)
	 (or (ismemb.ns sender (nodeset.n *NODE* 'cq))
	     (ismemb.ns sender (nodeset.n *NODE* 'arg))
	     (ismemb.ns sender (nodeset.n *NODE* 'dcq))))))
;
;
; =============================================================================
;
; is-rule-to-cq.rep
; -----------------
;
;       arguments     : report  -  <report>
;
;       returns       : <boolean>
;
;       description   : determines whether or not the report is from a rule
;                       node to a node in consequent position of the rule
;
;       implementation: checks to see if the report was sent by a node which
;                       is at the end of a cq-, arg-, or dcq- arc, since
;                       these are the only arcs which point to a rule for
;                       which the current node is in consequent position
;
;                                        written :  rgh  6/11/85
;                                        modified:  rgh  4/20/86
;
;
(defun is-rule-to-cq.rep (report)
  (let ((sender (signature.rep report)))
    (and (is.n sender)
	 (or (ismemb.ns sender (nodeset.n *NODE* 'cq-))
	     (ismemb.ns sender (nodeset.n *NODE* 'arg-))
	     (ismemb.ns sender (nodeset.n *NODE* 'dcq-))))))
;
;
; =============================================================================
;
; is-node-to-node.rep
; -------------------
;
;       arguments     : report  -  <report>
;
;       returns       : <boolean>
;
;       description   : determines whether or not the report is from a node
;                       at the other end of a match channel
;
;       implementation: checks to see if the report is neither of the type
;                       ant-to-rule nor rule-to-cq, since they are the only
;                       other possible types of reports
;
;                                        written :  rgh  6/11/85
;                                        modified:  rgh  4/20/86
;
;
(defun is-node-to-node.rep (report)
  (and (is.n (signature.rep report))
       (not (is-ant-to-rule.rep report))
       (not (is-rule-to-cq.rep report))))
;
;
;
; =============================================================================
;
; addbinding.rep
; --------------
;
;       arguments     : mb - <mbind>
;                       rep - <report>
;
;       returns       : <report>
;
;       description   : adds the binding "mb" to the <substitution> of "rep"
;
;                                        written :  rgh  8/05/85
;                                        modified:  rgh  3/09/86
;
;
(defun addbinding.rep (mb rep)
  (make.rep (putin.sbst mb (subst.rep rep))
	    (support.rep rep)
	    (sign.rep rep)
	    (signature.rep rep)
	    (node.rep rep)
	    (context.rep rep)))
;
;
; =============================================================================
;
; iseq.rep
; --------
;
;       arguments     : r1 - <report>
;                       r2 - <report>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "r1" and "r2" are equal
;
;                                        written :  rgh 11/30/85
;                                        modified:  rgh  3/09/86
;                                        modified:  scs  3/29/88
;                                        modified:  cpf 10/07/88
;
(defun iseq.rep (r1 r2)
  (and (iseq.sbst (subst.rep r1) (subst.rep r2))
       (iseq.sup (support.rep r1) (support.rep r2))
       (iseq.sign (sign.rep r1) (sign.rep r2))
       (iseq.n (signature.rep r1) (signature.rep r2))
       (sneps:iseq.ct (context.rep r1) (context.rep r2))
       (let ((n1 (node.rep r1)) (n2 (node.rep r2)))
	 (or (and (null n1) (null n2))
	     (and (not (null n1))
		  (not (null n2))
		  (iseq.n n1 n2))))))
;
;
; =============================================================================
;
; ispos.rep
; ---------
;
;       arguments     : report - <report>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "report" is a positive <report>,
;                       "false" otherwise
;
;                                        written :  rgh  2/12/86
;                                        modified:
;
;
(defun ispos.rep (report)
  (eq (sign.rep report) 'POS))
;
;
; =============================================================================
;
; isneg.rep
; ---------
;
;       arguments     : report - <report>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "report" is a negative <report>,
;                       "false" otherwise
;
;                                        written :  rgh  2/12/86
;                                        modified:
;
;
(defun isneg.rep (report)
  (eq (sign.rep report) 'NEG))
;
;
; =============================================================================
;
; hasct.rep
; ---------
;
;       arguments     : report - <report>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "report" has context information
;                       other than nil, "false" otherwise
;
;                                        written :  njm/cpf 10/26/88
;                                        modified:
;
;
(defun hasct.rep (report)
  (sneps:is.ct (context.rep report)))
;
;
; =============================================================================
