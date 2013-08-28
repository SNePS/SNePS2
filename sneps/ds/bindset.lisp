;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: bindset.lisp,v 1.2 2013/08/28 19:07:24 shapiro Exp $

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




(in-package :sneps)


; =============================================================================
; Data Type:  <bind set> ::= {<bind>  <bind> ... <bind>}
;         
; R[<bind set>] ::= ( R[<bind>] R[<bind>]  ... R[<bind>] )
;
; =============================================================================
;
; ABSTRACTION:
;
;    A <bind set> is a <set> of <bind>s. A <bind set> can be thought of as
; a contraint on a <node> during a search, where each <bind> is
; a constraint on a <var> relevant to the search.
;
;    The search can be triggered by the snepsul-function "find" or by the 
; unification lisp function "match". In the case of "find", a
; <find node description> is given, and the goal is to find all <node>s
; in the <net> that comply to that <find node description>. In the case of
; "match", a <node> is given, and the goal is to find all <node>s in the <net>
; that unify the original <node>. 
;
;    The search is generally conducted by:  
;  1) considering a <cable> at a time in the original <find node description> 
;     or <node>;
;  2) "find"ing or "match"ing the <node>s in the <net> that comply to that 
;     <cable> (current potential matching set); and,
;  3) intersecting the current potential matching set with the previous 
;     potential matching set (the intersection of the potential 
;     matching sets considering the previous <cable>s).
;
;    The potential matching sets are <node bind set>s. 
; A <node bind set> is a <set> of the form 
;            {<node bind> <node bind> ... <node bind>}
; containing the set of <node>s found according to some search criteria.
; Each <node> is constrained by some <bind set>, forming a <node bind>,
; which is a <pair> of the form 
;            <node> - <bind set> .
; A <bind set> is a <set> of the form
;            {<bind> <bind> ... <bind>} .
; Each <bind set> represents a constraint on the corresponding <node>,
; and is composed of one or more smaller constraints called <bind>s.
; A <bind> is a <pair> of the form
;            <var> - <node set> 
; and represents a constraint on a particular <var>. In case of "find",
; it is a <node set> representing all possible <node>s that can match
; that particular <svar> ( <var> ). In case of "match", it represents a 
; singleton <node set> representing  the <node> that can match the 
; particular <variable node> ( <var> ).
; 
;
; REPRESENTATION:
;
;    The representation of a <bind set> is an unordered sequence of the 
; representations of its <bind>s.
;
;                                         ejm, vhs 07/06/84
;
; =============================================================================
;
; new.bs 
; ------
;
;       returns       : <bind set>
;
;       description   : creates a <new bind set>
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defmacro new.bs ()
   `())
;
;
; =============================================================================
;
; is.bs 
; -----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean> 
;
;       description   : Returns "true" if "u" is a <bind set>, 
;                       "false" otherwise.
;
;       implementation: It does not check if all elements of "u" are <bind>s,
;                       it checks just the first one.
;
;                                        written:  rmo 07/28/83
;                                        modified: ejm 08/30/83, 10/11/83
;
;
(defmacro is.bs (u)
   `(or (null ,u)
        (and (listp ,u)
             (is.b (first ,u)))))
;
;
; =============================================================================
;
; isnew.bs 
; --------
;
;       arguments     : bs - <bind set> 
;
;       returns       : <boolean> 
;
;       description   : Returns "true" if "bs" is a <new bind set>,
;                       "false" oterwise.
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
(defmacro isnew.bs (bs)
   `(null ,bs))
;
;
; =============================================================================
;
; replace.bs    
; ----------
;
;       arguments     : b  - <bind>
;                       bs - <bind set>
;
;       returns       : <bind set>
;
;       description   : if there is no <bind> in "bs" with the same <var>
;                       as the one in "b", it inserts "b" in "bs";
;                       if there is one, it replaces it with "b".
;
;                                        written : rmo 07/28/83
;                                        modified: ejm 08/30/83
;
(defun replace.bs (b bs)
  (let ((result-bs (list b)))
    (loop
      (if (isnew.bs bs) (return  result-bs))
      (if (iseq.v (var.b b) (var.b (choose.bs bs)))
	  (return (nconc result-bs
			    (others.bs bs))))
      (setq result-bs (cons (choose.bs bs) result-bs)
	    bs (others.bs bs)))))
;
;
; =============================================================================
;
; make.bs
; -------
;
;       arguments     : <bind> <bind> ... <bind>
;
;       returns       : <bind set>
;
;       description   : returns a <bind set> composed of the <bind>s passed as
;                       arguments.
;
;       implementation: In order to avoid extra checking, it assumes 
;                       there is no more than one <bind> with
;                       the same <var> in the argument list.
;
;                                        written :  ejm 09/19/83
;                                        modified:
;
;
(defmacro make.bs (&rest b)
   `',b)
;
;
; =============================================================================
;
; nodeset.bs
; ----------
;
;       arguments     : v  - <var>
;                       bs - <bind set>
;
;       returns       : <node set>
;
;       description   : It returns the <node set> which is associated
;                       with "v" in "bs". If there is not a <bind> in "bs"
;                       with "v" as the <var> it returns a <new nodeset>.
;
;                                        written :  ejm 06/29/84
;                                        modified:
;
;
(defmacro nodeset.bs (v bs)
   `(nodeset.b (assoc ,v ,bs :test #'eq)))

;
;
; =============================================================================
;
; do.bs
; -----
;
;                                        written :  ssc  5/10/89
;                                        modified:
;
;
(defmacro do.bs ((var bsform &optional resultform) &body forms)
  `(dolist (,var ,bsform ,resultform) ,@forms))
;
;
; =============================================================================
;
; intersect1.bs
; -------------
;
;       arguments     : bs1 - <bind set> which is not a <new bind set>
;                       bs2 - <bind set> which is not a <new bind set>
;
;       returns       : <bind set> | nil
;
;       description   :    It returns the intersection of "bs1" and "bs2".
;                          It checks each <bind> of "bs1" against all
;                       the <bind>s of "bs2". 
;                          If there is no <bind> in "bs2" with an identical 
;                       <var>, or if there is one but whose <node set> value 
;                       is a <new node set>, the <bind> in "bs1" becomes part 
;                       of the result.
;                          If there is such a <bind> in "bs2" a <new bind> is 
;                       added to the result. The <new bind> is composed of
;                       that <var> together with a <node set> which is the 
;                       intersection of the <node set>s of the two <bind>s. 
;
;                                        written :  ejm 06/30/84
;                                        modified:  ssc  5/10/89
;
;
(defmacro intersect1.bs (bs1 bs2)
  `(let (result-bs result-b ns1 ns2)
     (do.bs (b1 ,bs1 (union.bs result-bs ,bs2))
       (setq    ns1 (nodeset.b b1)
		ns2 (nodeset.bs (var.b b1) ,bs2))
       (cond ((isnew.ns ns2) 
	      (setq result-b b1
		    result-bs (cons result-b result-bs)))
	     (t (setq result-b (new.b (var.b b1)
				      (intersect.ns ns1 ns2))
		      result-bs (cons result-b result-bs))))
       (unless (nodeset.b result-b) (return nil)))))
;
; =============================================================================
;
; intersect.bs
; ------------
;
;       arguments     : bs1 - <bind set>
;                       bs2 - <bind set>
;
;       returns       : <bind set> | failure
;
;       description   :    It returns the intersection of "bs1" and "bs2".
;                          The two <bind set>s are constraints on a
;                       potential <node>: "bs1" is the constraint on the
;                       <node> considering the previous <cable>s;
;                       "bs2" is the constraint on the <node> considering
;                       the <cable> being matched currently.
;                          The intersection of the two <bind set>s (constraints)
;                       is a <bind set> which is the minimal constraint that 
;                       satisfies simultaneously both original constraints.
;                       If such minimal constraint does not exist because
;                       the two original constraints are incompatible,
;                       the result of the intersection is "failure".
;                          Several cases are possible:
;                       1) If any of the two <bind set>s is a <new bind set>
;                          (no constraint) the result is the other.
;                          Notice that if the latter is also a <new bind set>,
;                          the result is a <new bind set> meaning no constraint,
;                          and not failure as one might think.
;                       2) If the two constraints are compatible the result is 
;                          their intersection. 
;                       3) If they are incompatible then the result is the 
;                          <atom> "failure", instead of "nil" which might be 
;                          the representation of the absence of constraints.
;
;                                        written :  ejm 06/30/84
;                                        modified:
;
;
(defmacro intersect.bs (bs1 bs2)
   `(cond ((isnew.bs ,bs1) ,bs2)
          ((isnew.bs ,bs2) ,bs1)
          ((intersect1.bs ,bs1 ,bs2))
          (t 'failure)))

;
; =============================================================================
;
; bind.bs
; --------
;
;       arguments     : v  - <var>
;                       bs - <bind set>
;
;       returns       : <bind> | false
;
;       description   : It returns the <bind> in "bs" which has "v" as the
;                       <var>. If there is no such <bind> it returns
;                       "false".
;
;                                        written :  ejm 06/29/84
;                                        modified:
;
;
(defmacro bind.bs (v bs)
   `(assoc ,v ,bs :test #'eq))
;
;
; =============================================================================
;
; union.bs 
; --------
;
;       arguments     : bs1 - <bind set>
;                       bs2 - <bind set>
;
;       returns       : <bind set> 
;
;
;       description   : It returns the union of "bs1" and "bs2".
;                       A <bind> in "bs2" which has the same <var> of a <bind>
;                       in "bs1" is forgotten, so the order of the arguments
;                       is meaningful. The reason for this is that the <bind>
;                       in "bs2" was already  taken into account to construct
;                       the correspondent <bind> in "bs1".
;
;
;                                        written:  rmo 08/04/83
;                                        modified: ejm 07/06/84
;
;
(defun union.bs (bs1 bs2)
   (cond ((isnew.bs bs2) bs1)
         ((bind.bs (var.b (choose.bs bs2)) bs1) 
          (union.bs bs1 (others.bs bs2)))
         (t (cons (choose.bs bs2) 
                     (union.bs bs1 (others.bs bs2))))))
;
;
; =============================================================================
;
; choose.bs
; ---------
;
;       arguments     : bs - <bind set>
;
;       returns       : <bind> 
;
;       description   : returns a <bind> of "bs"
;
;                                        written:  rmo 08/01/83
;                                        modified:
;
;
(defun choose.bs (bs)
   (first bs))
;
;
; =============================================================================
;
; others.bs 
; ---------
;
;       arguments     : bs - <bind set>
;
;       returns       : <bind set> 
;
;       description   : returns a <bind set> identical to "bs" but without
;                       the element that would be chosen by choose.bs.
;
;                                        written:  rmo 08/01/83
;                                        modified:
;
;
(defun others.bs (bs)
   (rest bs))
;
;
; =============================================================================
;
; ismemb.bs    
; ---------
;
;       arguments     : b  - <bind>
;                       bs - <bind set>
;
;       returns       : <boolean>   
;
;       description   : returns "true" if "b" is one of the <bind>s in "bs",
;                       "false" otherwise.
;
;                                        written : rmo 08/01/83
;                                        modified: ejm 08/30/83, 08/01/84
;
(defmacro ismemb.bs (b bs)
   `(member ,b ,bs :test #'equal))
;
;
; =============================================================================
;
; issubset.bs  
; -----------
;
;       arguments     : bs1 - <bind set>
;                       bs2 - <bind set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "bs1" is a subset of "bs2",
;                       "false" otherwise.
;
;
;                                        written:  rmo 08/01/83   
;                                        modified: ejm 07/10/84
;
;
(defmacro issubset.bs (bs1 bs2)
  `(do.bs (current-b ,bs1 t) 
     (unless (ismemb.bs current-b ,bs2) (return nil))))
;
;
; =============================================================================
;
; iseq.bs   
; -------
;
;       arguments     : bs1 - <bind set>
;                       bs2 - <bind set>
;
;       returns       : <boolean>
;
;       description   : Compares "bs1" and "bs2" as sets : returns "true" if
;                       "bs1" and "bs2" have identical <bind>s,
;                       "false" otherwise.  
;
;
;                                        written:  rmo 08/01/83
;                                        modified:
;
;
(defmacro iseq.bs (bs1 bs2)
   `(and (issubset.bs ,bs1 ,bs2)
         (issubset.bs ,bs2 ,bs1)))
;
;
; =============================================================================

 



    
    




