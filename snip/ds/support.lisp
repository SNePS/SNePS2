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
; <support> ::= <context cableset>
;
;
;
; -----------------------------------------------------------------------------
;  
; RECOGNIZERS    is.sup            : <universal> --> <boolean>
;
; CONSTRUCTORS   new.sup           :           --> <support>
;                insert.sup        : <ot> x <context> x <support> --> <support>
;                merge.sup         : <support> x <support> --> <support>
;
; SELECTORS      ot.sup            : <support> --> <ot>
;                ctset.sup         : <support> --> <contextset>
;                others.sup        : <support> --> <support>
;                getcontextset.sup : <ot> x <support> --> <contextset>
;
; TESTS          isnew.sup         : <support> --> <boolean>
;                iseq.sup          : <support> x <support> --> <boolean>
;                isincluded.sup    : <support> x <support> --> <boolean>
;
; UTILITIES      filter.sup        : <support> x <context> --> <support>
;                cardinality.sup   : <support> --> <non neg integer>
;
; =============================================================================
;
; isnew.sup
; ---------
;
;       arguments     : sup - <support>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "sup" is a new <support>, "false"
;                       otherwise
;
;                                        written :  njm/cpf 10/19/88
;                                        modified:  
;
;
(defmacro isnew.sup (sup)
  `(sneps:isnew.ctcs ,sup))
;
; =============================================================================
;
; is.sup
; ------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <support>, "false"
;                       otherwise
;
;                                        written :  cpf 10/06/88
;                                        modified:  
;
;
(defmacro is.sup (u)
  `(sneps:is.ctcs ,u))

; =============================================================================
;
; new.sup
; -------
;
;       arguments     : none
;
;       returns       : <support>
;
;       description   : returns an empty <support>.
;
;                                        written :  njm 10/19/88
;                                        modified:  
;
;
(defmacro new.sup ()
  `(sneps:new.ctcs))

;
; =============================================================================
;
; insert.sup
; ----------
;
;       arguments     : ot   - <ot>
;                       ct   - <context>
;                       sup - <support>
;
;       returns       : <support>
;
;       description   : returns a <support> in which was introduced the
;                       new information about ot and ct.
;
;                                        written :  cpf 10/18/88 
;                                        modified:
;
;
(defmacro insert.sup (ot ct sup)
  `(sneps:insert.ctcs ,ot ,ct ,sup))
;
;
; =============================================================================
;
; merge.sup
; ---------
;
;       arguments     : sup1 - <support>
;                       sup2 - <support>
;
;       returns       : <support>
;
;       description   : returns a <support> in which the union of both
;                       <support>s "sup1" and "sup2".
;
;                                        written :  cpf 10/18/88 
;                                        modified:
;
;
(defun merge.sup (sup1 sup2)
  (do* ((sup sup1 (sneps:others.ctcs sup))
	 (ot (sneps:ot.ctcs sup) (sneps:ot.ctcs sup))
	 (cts (sneps:contextset.ctcs sup) (sneps:contextset.ctcs sup)))
	((sneps:isnew.ctcs sup) sup2)
     (dolist (ct cts)
       (setq sup2 (sneps:insert.ctcs ot ct sup2)))))
;
; =============================================================================
;
; ot.sup
; ------
;
;       arguments     : sup - <support>
;
;       returns       : <ot>
;
;       description   : returns the <ot> of "sup"
;
;                                        written :  cpf 10/18/88 
;                                        modified:
;
;
(defmacro ot.sup (sup)
  `(sneps:ot.ctcs ,sup))
;
;
; =============================================================================
;
; ctset.sup
; ---------
;
;       arguments     : sup - <support>
;
;       returns       : <ct>
;
;       description   : returns the <ct> of "sup"
;
;                                        written :  cpf 10/18/88
;                                        modified:
;
;
(defmacro ctset.sup (sup)
  `(sneps:contextset.ctcs ,sup))
;
; =============================================================================
;
;
; cardinality.sup
; ---------------
;
;       arguments     : su1 - <support>
;                       
;       returns       : <non negative integer>
;
;       description   : Returns the length of a <support>
;                       
;
;                                        written : cpf 10/20/88 
;                                        modified:  
;
(defmacro cardinality.sup (sup)
  `(list-length ,sup))
;
;
; =============================================================================
;
; others.sup
; ----------
;
;       arguments     : sup - <support>
;
;       returns       : <support>
;
;       description   : returns the sup without the first support.
;                       The first support is the list <ot> <contextset>.
;
;                                        written :  cpf 10/18/88 
;                                        modified:  
;
;
(defmacro others.sup (sup)
  `(sneps:others.ctcs ,sup))

;
;
; =============================================================================
;
; iseq.sup
; --------
;
;       arguments     : sup1 - <support>
;                       sup2 - <support>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "sup1" and "sup2" are equal,
;                       "false" otherwise
;
;                                        written :  cpf 10/18/88 
;                                        modified:  
;
;
(defun iseq.sup (sup1 sup2)
  (and (eql (cardinality.sup sup1) (cardinality.sup sup2))
       (do* ((sup sup1 (others.sup sup))
	     (ot (sneps:ot.ctcs sup) (sneps:ot.ctcs sup))
	     (cts1 (sneps:contextset.ctcs sup) (sneps:contextset.ctcs sup))
	     (cts2 (sneps:getcontextset.ctcs ot sup2) (sneps:getcontextset.ctcs ot sup2))
	     (return t))
	    ((or (null return)
		 (sneps:isnew.ctcs sup)) return)
	 (unless (sneps:isnew.cts (sneps:compl.cts cts1 cts2))
	   (setq return nil)))))

;
;
; =============================================================================
;
; isincluded.sup
; --------------
;
;       arguments     : sup1, sup2 - <support>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "sup1" is included in "sup2"
;                       "false" otherwise
;
;                                        written :  cpf 10/18/88 
;                                        modified:  
;
;
(defun isincluded.sup (sup1 sup2)
  (do* ((sup sup1 (sneps:others.ctcs sup))
	(ot (sneps:ot.ctcs sup) (sneps:ot.ctcs sup))
	(cts1 (sneps:contextset.ctcs sup) (sneps:contextset.ctcs sup))
	(cts2 (sneps:getcontextset.ctcs ot sup2)(sneps:getcontextset.ctcs ot sup2))
	(return t))
       ((or (null return)
	    (sneps:isnew.ctcs sup)) return)
    (unless (sneps:isnew.cts (sneps:compl.cts cts1 cts2))
      (setq return nil))))

;
;
; =============================================================================
;
; getcontextset.sup
; -----------------
;
;       arguments     : ot  - <ot>
;                       sup - <support>
;
;       returns       : <contextset>
;
;       description   : returns the <contextset> associated with `ot' in `sup'.
;                       
;
;                                        written :  cpf 10/18/88 
;                                        modified:  
;
;
(defmacro getcontextset.sup (ot sup)
  `(sneps:getcontextset.ctcs ,ot ,sup))
;
;
; =============================================================================
;
; filter.sup
; ----------
;
;       arguments     : sup - <support>
;                       ct  - <context>
;
;       returns       : <support>
;
;       description   : Recieves a <support> and a <context> and returns a
;                       new <support> which context information is included
;                       in context `ct'.
;
;                       
;                                        written :  njm/cpf 10/18/88 
;                                        modified:  njm     05/13/89
;
;
(defmacro filter.sup (sup ct)
  `(sneps:filter.ctcs ,sup , ct))
      
;
;
; =============================================================================
;
; addsupport.n
; ------------
;
;       arguments     : sup - <context cable set>
;                       n  - <node>
;
;       returns       : <support>
;
;       description   : merges the new support to the previous existing support
;                       of node 'n'
;
;                       
;                                        written :  njm/cpf 10/24/88 
;                                        modified:  
;
;
(defun addsupport.n (sup n)
  (setf (sneps:node-asupport n)
	(merge.sup sup (sneps:node-asupport n))))
      
;
;
; =============================================================================












    
    




