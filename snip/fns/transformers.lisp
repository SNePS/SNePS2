;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: transformers.lisp,v 1.2 2013/08/28 19:07:28 shapiro Exp $

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
; Data type transformer functions for SNIP
;
; =============================================================================
;
; rep-to-instance       : <report> --> <instance>
; ch-to-restr           : <channel> --> <restriction>
; matchingset-to-repset : <matching set> x <sign> x <signature>
;                                   --> <report set>
; node-to-process       : <node> --> <process>
; nodeset-to-fnodeset   : <node set> --> <flagged node set>
; matchingset-to-iset   : <matching set> x <sign> --> <instance set>
;
; =============================================================================
;
; rep-to-instance
; ---------------
;
;       arguments     : report - <report> 
;
;       returns       : <instance> 
;
;       description   : returns the <instance> reported by the <report>
;
;
;                                        written :  rgh 08/18/85
;                                        modified:  cpf 10/19/88
;
;
(defun rep-to-instance (report)
   (make.inst (subst.rep report) (support.rep report) (sign.rep report)))
;
;
; =============================================================================
;
; ch-to-restr
; -----------
;
;       arguments     : channel - <channel>
;
;       returns       : <restriction>
;
;       description   : returns the <restriction> which represents the
;                       bindings sought by the <channel>
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defun ch-to-restr (channel)
   (make.restr (filter.ch channel)))
;
;
; =============================================================================
;
; supmatchingset-to-repset
; ------------------------
;
;       arguments     : ms   - <supmatching set>
;                       sign - <sign>
;                       sig  - <signature>
;
;       returns       : <report set>
;
;       description   : forms a <report set> consisting of <report>s which
;                        are made up of the source substitutions of "ms"
;                        along with the common "sign" and "sig".
;
;                       THIS FUNCTION IS INTENDED TO BE USED FOR FILTERING
;                       REPORTS DURING INFERENCE, AND ASSUMES THAT EACH
;                       MATCHING IN "ms" HAS NULL TARGET SUBSTITUTION AND
;                       NULL MNODEREP SET.
;
;                                        written :  rgh 07/30/85
;                                        modified:  rgh 08/02/85
;                                        modified:  cpf/njm 10/25/88
;
;
;(defun supmatchingset-to-repset (ms sign sig)
;  (let ((result (new.repset)))
;    (do.supmatchingset (m ms result)
;      (setq result
;	    (insert.repset (make.rep (source-sub.supmatching m)
;				     (target-sup.supmatching m)
;				     sign
;				     sig
;				     (tnode.supmatching m)
;				     nil)
;			   result)))))
;
;
; =============================================================================
;
; node-to-process
; ---------------
;
;       arguments     : n - <node>
;
;       returns       : <process>
;
;       description   : creates the appropriate activation process for the
;                        node "n"
;
;                                        written :  rgh 11/11/85
;                                        modified:  rgh  2/02/86
;                                                   rgh  2/10/86
;                                                   rgh  3/31/86
;;						     dk  4/17/91
;                                                    dk  6/2/93
; added the do-if transformer
;                                                    dk  6/3/93
; added the when-do transformer
;                                                    hi  3/31/99
; added the whenever-do transformer

(defun node-to-process (n)
   (cond ((is-v-ent.n n) (make-v-ent n))
         ((is-&-ent.n n) (make-&-ent n))
         ((is-thresh.n n) (make-thresh n))
	 ((is-nor.n n) (make-nor n))
	 ((is-and.n n) (make-and n))
         ((is-and-or.n n) (make-and-or n))
         ((is-num-ent.n n) (make-num-ent n))
         ((is-num-quant.n n) (make-num-quant n))
         ((is-non-deriv.n n) (make-non-deriv n))
	 ((is-do-if.n n) (make-do-if n))   ; the do-if rule
 	 ((is-when-do.n n) (make-when-do n))   ; the when-do rule
         ((sneps::is-whenever-do.n n) (make-whenever-do n))
	 ((sneps::is-act.n n) (make-act n))  ; changed to include act-processes
         (t (make-non-rule n))))
;
;
;
;
; =============================================================================
;
; nodeset-to-fnodeset
; -------------------
;
;       arguments     : nodeset - <node set>
;
;       returns       : <flagged node set>
;
;       description   : returns a <flagged node set> containing all of the
;                       <node>s is "nodeset", and with all <truth flags> set
;                       to 'UNKNOWN
;
;                                        written :  rgh  2/08/86
;                                        modified:  rgh  2/13/86
;                                        modified:  njm 10/18/88
;
;
(defun nodeset-to-fnodeset (nodeset)
  (let ((result (new.fns)))
    (do.ns (n nodeset result)
       (setq result (putin.fns (make.fn n (new.sup) 'UNKNOWN) result)))))
;
;
; =============================================================================
;
; supmatchingset-to-iset
; ----------------------
;
;       arguments     : mset - <supmatching set>
;                       sign - <sign>
;
;       returns       : <instance set>
;
;       description   : returns an <instance set> containing substitutions
;                       from the source substitutions of the <supmatching>s of
;                       "mset" paired with "sign"
;
;                                        written :  rgh  3/30/86
;                                        modified:  scs  3/15/88
;                                        modified:  njm/cpf 10/18/88
;
;
(defun supmatchingset-to-iset (mset sign)
  (let ((result (new.iset)))
    (do.supmatchingset (m mset result)
       (setq result
	     (insert.iset (make.inst (source-sub.supmatching m)
				     (target-sup.supmatching m)
				     sign)
			  result)))))
;
;
; =============================================================================



    
    




