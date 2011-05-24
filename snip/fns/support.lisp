;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: support.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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
; combine-ots
; -----------
;
;       arguments     : ot1 - <ot>
;                       ot2 - <ot>
;
;       returns       : <ot>
;
;       description   : returns the origin tag resulting from combining
;                       'ot1' and 'ot2'.
;
;       side-effects  : 
;
;       implementation: 
;
;
;
;                                        written :  jpm  6/23/82
;                                        modified:  njm 10/20/88 
;
;
;      
;                                      
(defmacro combine-ots (ot1 ot2)
  "returns the origin tag resulting from combining 'ot1' and 'ot2'"
  `(cond ((or (equal ,ot1 'sneps:ext)
	      (equal ,ot2 'sneps:ext)) 'sneps:ext)
	 (t 'sneps:der)))


;
; =============================================================================
;
; combine-ots*
; ------------
;
;       arguments     : ot ... ot - <ot>s
;
;       returns       : <ot>
;
;       description   : returns the <ot> resulting from combining the <ot>s
;                       passed as arguments.
;
;       side-effects  : 
;
;       implementation: 
;
;
;
;                                        written :  jpm 11/30/82
;                                        modified:  njm 10/20/88
;
;
;
(defun combine-ots* (otlst)
  "returns the <ot> resulting from combining the <ot>s passed as arguments"
    (if (member 'sneps:ext otlst :test #'equal)
	'sneps:ext
	'sneps:der))
;
; =============================================================================
;
; combine-and-intr-ots* 
; ---------------------
;
;       arguments     : ot ... ot - <ot>s
;                       ct ... ct - <ct>s
;
;       returns       : <ot>
;
;       description   : returns the <ot> resulting from combining the <ot>s
;                       of the conjunction arguments. 
;
;       side-effects  : 
;
;       implementation: 
;
;
;
;                                        written :  cpf/njm 12/15/88
;                                        modified:  
;
;
;
(defun combine-and-intr-ots* (otlst ctlist)
  "returns the <ot> resulting from combining the <ot>s passed as arguments
   for the and-introduction"
    (if (or (member 'sneps:ext otlst :test #'equal)
	    (not-all-ct-equal ctlist))
	'sneps:ext
	'sneps:der))


;
;
; =============================================================================
;
; combinations-of 
; --------------
;
;
;       arguments     : sup - <context cable set>
;                       lst-of-sups - list of <context cable set>
;
;       returns       : <context cable set>
;
;       description   : Returns all combinations between the elements of
;                       "lst-of-lst" and the elements of lst
;                       Example: if lst = (1 2) and lst-of-lst = ((3 4) (5))
;                       then the result = ((1 3 5) (1 4 5) (2 3 5) (2 4 5))
;
;                                        written :  jpm  6/23/82
;                                        modified:  njm 10/20/88 
;
;
(defun combinations-of (sup lst-of-sups)
  (if (null lst-of-sups)
      (sup-to-lists sup)
      (combinations-of-1 sup lst-of-sups)))


(defun combinations-of-1 (sup lst-of-sups)
  (if (isnew.sup sup)
      nil
      (append (comb-ot-ct (ot.sup sup)
			  (ctset.sup sup)
			  (combinations-of (first lst-of-sups) (rest lst-of-sups)))
	      (combinations-of-1 (others.sup sup) lst-of-sups))))


;
;
; =============================================================================
;
; comb-ot-ct  
; ----------
;
;
;       arguments     : ot  - <ot>
;                       cts - <context set>
;                       suplist - <list of lists>
;
;       returns       : <list of lists>
;
;       description   : combines 'ot' and each elements of 'cts' with 
;                       'suplist' a list-of-lists.
;
;       example       : if ot = ot1
;                          cts = (c1 c2 c3)
;                          suplist = ((ot2 ot3) (c4 c5) (ot5 ot6) (c6 c7))
;
;                       then it returns
;                          ((ot2 ot3 ot1) (c4 c5 c1) (ot5 ot6 ot1) (c6 c7 c1)
;                           (ot2 ot3 ot1) (c4 c5 c2) (ot5 ot6 ot1) (c6 c7 c2)
;                           (ot2 ot3 ot1) (c4 c5 c3) (ot5 ot6 ot1) (c6 c7 c3))
;
;
;
;                                        written :  njm 10/20/88
;                                        modified:   
;
;
;
(defun comb-ot-ct (ot cts suplist)
  (let ((result nil))
    (dolist (ct cts result)
      (do* ((sl suplist (rest (rest sl)))
	    (ots1 (first sl) (first sl))
	    (cts1 (second sl) (second sl)))
	   ((null sl))
	(setq result (append (list (cons ot ots1))
			     (list (insert.cts ct cts1))
			     result))))))
      

;
; =============================================================================
;
; sup-to-combinations
; -------------------
;
;
;       arguments     : sup - <support>
;
;       returns       : <list of lists>
;
;       description   : modifies 'sup' switching each tag by the list of the tag.
;
;       example       : if sup = (ot1 (c4 c5) ot2 (c6 c7))
;
;                       then it returns
;                          ((ot1) (c4) (ot1) (c5) (ot2) (c6) (ot2) (c7))
;
;
;                                        written :  njm 11/15/88
;                                        modified:  
;
;
(defun sup-to-combinations (sup)
  (do* ((s1 sup (others.sup s1))
	(ot1 (ot.sup s1) (ot.sup s1))
	(cts1 (ctset.sup s1) (ctset.sup s1))
	(listsupport nil))
       ((isnew.sup s1) listsupport)
    (dolist (ct cts1)
      (setq listsupport (append (list (list ot1))
				(list (list ct))
				listsupport)))))
  

;
; =============================================================================
;
; sup-to-lists   
; ------------
;
;
;       arguments     : sup - <support>
;
;       returns       : <list of lists>
;
;       description   : modifies 'sup' switching each tag by the list of the tag.
;
;       example       : if sup = (ot1 (c4 c5) ot2 (c6 c7))
;
;                       then it returns
;                          ((ot1) (c4 c5) (ot2) (c6 c7))
;
;
;                                        written :  njm 10/20/88
;                                        modified:  
;
;
(defun sup-to-lists (sup)
  (do* ((s1 sup (others.sup s1))
	(ot1 (ot.sup s1) (ot.sup s1))
	(cts1 (ctset.sup s1) (ctset.sup s1))
	(listsupport nil))
       ((isnew.sup s1) listsupport)
    (setq listsupport (append (list (list ot1))
			      (list cts1)
			      listsupport))))





;
; =============================================================================
;
; fns-to-suplist    
; --------------
;
;
;       arguments     : fns - <flag node set>
;
;       returns       : list of <support>s
;
;       description   : 
;
;
;
;                                        written :  njm 10/20/88
;                                        modified:  scs 07/13/89
;
;
(defun fns-to-suplist (fns)
  (let ((result nil))
    (do.fns (fn fns result)
      (when (support.fn fn)
	(setq result (cons (support.fn fn) result))))))


;
; =============================================================================
;
; not-all-ct-equal 
; ----------------
;
;
;       arguments     : ctlist - <context list>
;
;       returns       : <boolean>
;
;       description   : 
;
;
;
;                                        written :  cpf/njm 12/15/88
;                                        modified:  
;
;
(defun not-all-ct-equal (ctlist)
  (do ((ct (first ctlist))
       (ctl (rest ctlist) (rest ctl)))
      ((null ctl))
    (unless (equal ct (first ctl))
      (return t))))

;
; =============================================================================
;
; non-hyp
; -------
;
;
;       arguments     : sup -- <support>
;
;       returns       : <support>
;
;       description   : 
;
;
;
;                                        written :  scs 05/05/99
;                                        modified:  
;
;
(defun non-hyp (sup)
  "Returns a support just like SUP,
   but with every origin tag of HYP changed to DER."
  (substitute 'sneps:der 'sneps:hyp sup))




    
    




