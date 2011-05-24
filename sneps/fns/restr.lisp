;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: restr.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :sneps)


; ==============================================================================
; addrestr
; --------
;
;        arguments     : r - <context set> 
;                        c - <context>
;
;        returns       : <context>
;
;        description   : It adds the context "r" to the restriction slot of 
;                        context "c".
;
;
;                                        written :  njm  09/14/88 
;                                        modified:
;
;
(defmacro addrestr (r c)
  "It adds the context 'r' to the restriction slot of context 'c'.
   Returns the context 'c'."   
  `(prog2 (setf (context-restriction ,c)
                (sigma (union.cts ,r (context-restriction ,c))))
	  ,c))

;
; =============================================================================
;                               DEITAR FORA ?
;
;
; newrestriction  
; --------------
;
;
;       arguments     : hyps - <node set>
;
;       returns       : <context set>
;
;       description   : Computes the restriction associated with a recently
;                       created context defined by the assertions 'hyps'.
;                       Returns the computed restriction set.
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
;(defun newrestriction (hyps) 
;  (if (null hyps)
;      nil
;      (sigma (psi (apply
;		    #'append
;		    (mapcar #'(lambda (n)
;				(context-restriction (exists.ct (list n))))
;			    hyps))
;		  (exists.ct hyps)))))
;




;
; =============================================================================
;
;
; psi  
; ---
;
;
;       arguments     : fullrs - <context set>
;                       ct     - <context>
;
;       returns       : <context set>
;
;       description   : Implements the psi function of the SWM logic.
;                       'fullrs' is the union of all the restrictions
;                       associated with the contexts defined by each 
;                       single hypothesis of 'ct'.
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun psi (fullrs ct)
  (cond ((isnew.cts fullrs) (new.cts))
	((issubset.ct (choose.cts fullrs) ct)
	 (makeone.cts (getcontext (new.ns))))
	((isdisjoin (choose.cts fullrs) ct)
	 (insert.cts (choose.cts fullrs) (psi (others.cts fullrs) ct)))	
	(t (insert.cts
	     (fullbuildcontext (compl.ns (context-hyps (choose.cts fullrs))
					 (context-hyps ct))
			       (new.cts))
	     (psi (others.cts fullrs) ct)))))



;
; =============================================================================
;
;
; sigma  
; -----
;
;
;       arguments     : ctset - <context set>
;
;       returns       : <context set>
;
;       description   : Implements the sigma function of the SWM logic.
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun sigma (ctset)
  (let ((empty (getcontext (new.ns))))
    (if (ismemb.cts empty ctset)
	(makeone.cts empty)
	(sigma-1 ctset ctset))))


(defun sigma-1 (wset oset)
  (cond ((isnew.cts wset) (new.cts))
	((existsubset (choose.cts wset) oset)
	 (sigma-1 (others.cts wset) oset))
	(t (insert.cts (choose.cts wset) (sigma-1 (others.cts wset) oset)))))


;
; =============================================================================
;
;
; existsubset  
; -----------
;
;
;       arguments     : c  - <context>
;                       cs - <context set>
;
;       returns       : <boolean>
;
;       description   : Returns T if 'c' is a superset of any context
;                       present in 'cs', NIL otherwise.
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun existsubset (c cs)
  (cond ((isnew.cts cs) nil)
	((and (issubset.ct (choose.cts cs) c)
	      (not (iseq.ct c (choose.cts cs)))) t)
	(t (existsubset c (others.cts cs)))))


;
; =============================================================================
;
;
; isdisjoin  
; ---------
;
;
;       arguments     : c1 - <context>
;                       c2 - <context>
;
;       returns       : <boolean>
;
;       description   : Returns T if 'c1' and 'c2' have not any hypothesis in
;                       common, NIL otherwise.
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;


(defun isdisjoin (c1 c2)
  (isnew.cts (intersect.ns (context-hyps c1) (context-hyps c2))))


;
; =============================================================================
;
;
; updateall  
; ---------
;
;
;       arguments     : ct - <context>
;
;       returns       : ???
;
;       description   : 'ct' is an inconsistent context. This function upadtes
;                       the restriction sets of all contexts with this information.
;
;       
;
;
;                                  written :  mrc 10/20/88 
;
;
;
(defun updateall (ct)
  (updateall-1 ct (allct)))

(defun updateall-1 (ct cts)
  (cond ((isnew.cts cts) t)
	((isdisjoin ct (choose.cts cts)) (addrestr (makeone.cts ct) (choose.cts cts))
					 (updateall-1 ct (others.cts cts)))
	(t (updateall-1 ct (others.cts cts)))))



    
    




