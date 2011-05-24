;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: ctcableset.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


; =============================================================================
; Data Type:   <context cableset> ::= 
;             ( <ot> <context set> .... <ot> <context set> )
;
; =============================================================================



; =============================================================================
;
; Data Type:   <context cableset> 
; 
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.ctcs :    <universal>  --> <boolean>
;                
; CONSTRUCTORS   new.ctcs :               --> <context cableset>;
;                insert.ctcs :  <ot> x <context> x <context cable set> 
;                                         --> <context cableset>
;
; SELECTORS      getcontextset.ctcs : <ot> x <context cableset> --> <context set>
;                ot.ctcs :            <context cableset> --> <ot>
;                contextset.ctcs :    <context cableset> --> <context set>
;                others.ctcs :        <context cableset> --> <context cableset>
;
; TESTS          isnew.ctcs :         <context cableset> --> <boolean>
;
; UTILITY        filter.ctcs:         <support> x <context> --> <support>
;                describe.ctcs :
;                read.ctcs :
;                print.ctcs :
;
; =============================================================================





;
; =============================================================================
;
; new.ctcs 
; --------
;
;       arguments     : none
;
;       returns       : <context cable set>
;
;       description   : Creates a <new context cableset>.
;
;                                        written :  njm 09/16/88 
;                                        modified:
;
;
(defmacro new.ctcs ()
  "Creates a <new context cableset>"
  `()) 
;
;
; =============================================================================
;
; is.ctcs
; -------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <context cable set>,
;                       "false" otherwise. 
;
;       implementation: it does not check if all the elements of "u" are
;                       <ot>-<context set> pairs, it just checks the first
;                       one.
;
;
;
;                                        written :  njm 09/16/88    
;                                        modified:
;
;
(defmacro is.ctcs (u)
  "returns True if `u' is a <context cable set>, False otherwise. 
   It does not check if all the elements of `u' are <ot>-<context set> pairs, 
   it just checks the first one"
  `(or (null ,u)
       (and (listp ,u)
	    (is.ot (first ,u))
	    (is.cts (second ,u)))))
;
;
; =============================================================================
;
; isnew.ctcs
; ----------
;
;       arguments     : ctcs - <context cable set>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "ctcs" is a <new context cableset>,
;                       "false" otherwise.
;
;
;                                        written :  njm 09/16/88 
;                                        modified:
;
;
(defmacro isnew.ctcs (ctcs)
  "returns True if `ctcs' is a <new context cableset>, False otherwise"
  `(null ,ctcs))
;
; 
; ==========================================================================
;
; getcontextset.ctcs
; ---------------------
;
;     arguments     : o - <ot>
;                   : ctcs - <context cable set>
;
;     returns       : <context set>
;
;     description   : It returns the <context sets> corresponding to the 
;                     <ot> "o" in the <context cable set> "ctcs".
;                     It returns nil if there is no <set of context sets> with
;                     <ot> "o".
;
;                                      written :  njm 09/16/88 
;                                      modified: 
;
;
(defun getcontextset.ctcs (o ctcs)
  "It returns the <context sets> corresponding to the <ot> `o' in the
   <context cable set> `ctcs'. It returns nil if there is no
   <set of context sets> with <ot> `o'"
  (cond ((null ctcs) nil)
	((eq (first ctcs) o) (second ctcs))
	(t (getcontextset.ctcs o (rest (rest ctcs))))))


;
; =============================================================================
;
; insert.ctcs 
; -----------
;
;       arguments    : o   - <ot>
;                      ct  - <context>
;                      ctcs - <context cable set>
;
;       returns      : <context cable set>
;
;       description  : Looks for the <ot> `o' in the <context cable set> `ctcs'
;                    and if it is there, then it inserts the <context> 
;                    `ct' in the <context set> stored
;                    under the <ot> `o' in `ctcs'.
;                    Otherwise it inserts the new tag `o' and its 
;                    (makeone.cts `ct') at the proper place in the context 
;                    cableset `ctcs'. 
;
;                                        written :  njm 09/16/88 
;                                        modified:  scs/flj  6/20/04
;
;
(defun insert.ctcs (o ct ctcs)
  "Looks for the <ot> `o' in the <context cable set> `ctcs' and if it is there,
   then it inserts the <context> `ct' in the <context set> stored under the
   <ot> `o' in `ctcs'.
   Otherwise it inserts the new tag `o' and its (makeone.cts `ct') at the 
   proper place in the context cableset `ctcs'"
  ;; ct is not inserted if it is a (not necessarily proper) superset
  ;; of a context already in ctcs.
  (if ctcs
      (if (some #'(lambda (oldcts)
		    (and (is.cts oldcts)
			 (some #'(lambda (oldct)
				   (issubset.ct oldct ct))
			       oldcts)))
		ctcs)
	  ctcs
	(let ((sctcs (getcontextset.ctcs  o ctcs)))
	  (if sctcs
	      (substitute (insert.cts ct sctcs)
			  sctcs ctcs)
	    (append (list o (makeone.cts ct)) ctcs))))
    (list o (makeone.cts ct))))

;
; 
; =============================================================================
;
; ot.ctcs
; -------
;
;       arguments     : ctcs - <context cable set>
;
;       returns       : <ot> 
;
;       description   : Returns the first <ot> of "ctcs".
;
;                                        written :  njm 09/16/88
;                                        modified:
;
;
(defmacro ot.ctcs (ctcs)
  "Returns the first <ot> of `ctcs'"
  `(first ,ctcs))

;
; =============================================================================
;
; contextset.ctcs
; ---------------
;
;       arguments     : ctcs - <context cable set>
;
;       returns       : <context set> 
;
;       description   : Returns the first <context set> of "ctcs".
;
;                                        written :  njm 09/16/88
;                                        modified:
;
;
(defmacro contextset.ctcs (ctcs)
  "Returns the first <context set> of `ctcs'"
  `(second ,ctcs))



;
;
; =============================================================================
;
; others.ctcs 
; -----------
;
;       arguments     : ctcs - <context cable set>
;
;       returns       : <context cable set> 
;
;       description   : Returns a <context cable set> identical to "ctcs" but 
;                       without the first <ot>-<set of context sets> pair.
;
;                                        written :  njm 09/16/88
;                                        modified:
;
;
(defmacro others.ctcs (ctcs)
  "Returns a <context cable set> identical to `ctcs' but without the first
   <ot>-<set of context sets> pair"
  `(rest (rest ,ctcs)))



;
;
; =============================================================================
;
; describe.ctcs
; -------------
;
;       arguments     : ctcs - <context cable set>
;
;       returns       : <sequence>
;
;       description   : It returns a <sequence> which is a description of 
;                       the <context cable set> "ctcs" to be printed.
;
;                                        written :  njm 09/16/88  
;                                        modified:
;
(defun describe.ctcs (ctcs)
  "It returns a <sequence> which is a description of the <context cable set>
   `ctcs' to be printed"
  (declare (special ctcs))
  (cond ((isnew.ctcs ctcs) nil)
	(t (cons (describe.ot (ot.ctcs ctcs))
		 (cons (describe.cts (contextset.ctcs ctcs))
		       (describe.ctcs (others.ctcs ctcs)))))))


;
; =============================================================================
;
; read.ctcs
; ---------
;
;       arguments     : inunit - <unit>
;
;       returns       : <context cable set>
;
;       description   : It reads a <context cable set> from "inunit".
;
;       implementation : all contexts must already exist.
;
;
;                                        written :  njm 09/17/88  
;                                        modified:  hc  06/30/93
;
(defmacro read.ctcs (inunit)
  ;; This function should be obsolete
  (read inunit))

;
;
; =============================================================================
;
; print.ctcs
; ----------
;
;       arguments     : ctcs - <context cable set>
;                       outunit - <unit>
;
;       returns       : nil
;
;       description   : It Prints a <context cable set> to "outunit".
;
;       side-effects  : It prints the <context cable set>
;
;                                        written :  njm 09/16/88  
;                                        modified:  hc  06/30/93
;
(defun print.ctcs (ctcs outunit)
  ;; This function should be obsolete
  (with-readable-nodes-and-contexts
      (print ctcs outunit)))

;
;
; =============================================================================
;
; filter.ctcs
; -----------
;
;       arguments     : ctcs - <ctcs>
;                       ct  - <context>
;
;       returns       : <support>
;
;       description   : Recieves a <ctcs> and a <context> and returns a
;                       new <ctcs> which context information is included
;                       in context `ct'.
;
;                       
;                                        written :  njm/cpf 10/18/88 
;                                        modified:  
;
;
(defun filter.ctcs (sup ct)
  (do* ((s sup (others.ctcs s))
	(ot (ot.ctcs s) (ot.ctcs s))
	(cts (contextset.ctcs s) (contextset.ctcs  s))
	(newctcs (new.ctcs)))
       ((isnew.ctcs s) newctcs)
    (dolist (c cts)
      (if (issubset.ct c ct)
	  (setq newctcs (insert.ctcs ot c newctcs))))))
      
;
;
; =============================================================================





















    
    




