;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: nbindset.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
; Data Type:  <node bind set> ::= {<node bind> <node bind> ... <node bind>}
;         
; R[<node bind set>] ::= ( R[<node bind>] R[<node bind>]  ... R[<node bind>] )
;
; =============================================================================
;
; ABSTRACTION:
;
;    A <node bind set> is a <set> of <node bind>s. Each <node bind> can be 
; thought of as a <node> constrained by a <bind set> during a search. 
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
;    The representation of a <node bind set> is a unordered sequence of the 
; representations of its <node bind>s.
;
;                                         ejm, vhs 07/10/84
;
; =============================================================================
;
; new.nbs 
; -------
;
;       arguments     : none
;
;       returns       : <node bind set> 
;
;       description   : Creates a <new node bind set> . 
;
;
;                                        written:  rmo 08/02/83
;                                        modified:
;                                        translated: robert quinn 02/18/86
;
;
(defmacro new.nbs ()
   `())
;
;
; =============================================================================
;
; is.nbs  
; ------
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <node bind set>,
;                       "false" otherwise.
;
;       implementation: it does not check if all the elements of "u" are
;                       <node bind>s, it just checks the first one.
;
;                                        written:  rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro is.nbs (u)
   `(or (null ,u)
        (and (listp ,u)
             (is.nb (first ,u)))))
;
;
; =============================================================================
;
; isnew.nbs     
; ---------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "nbs" is a <new node bind set>, 
;                       "false" otherwise. 
;
;                                        written:  rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro isnew.nbs (nbs)
   `(null ,nbs))
;
;
; =============================================================================
;
; do.nbs    
; ------
;
;
;                                        written: ssc  5/10/89
;                                        modified:
;
;
(defmacro do.nbs ((var nbsform &optional resultform) &body forms)
  "iterator for nodebindsets."
  `(dolist (,var ,nbsform ,resultform) ,@forms))
;
;
; =============================================================================
;
; ismemb.nbs    
; ----------
;
;       arguments     : nb  - <node bind>
;                       nbs - <node bind set> 
;
;       returns       : <boolean>
;
;       description   : returns "true" if there is a <node bind> in "nbs"
;                       identical to "nb", "false" otherwise.
;
;
;                                        written:  rmo 08/02/83
;                                        modified: ejm 07/10/84
;                                        translated: rmq 02/18/86
;                                        modified: ssc  5/10/89
;
;
(defmacro ismemb.nbs (nb nbs)
   `(do.nbs (current-nb ,nbs nil)
      (when (iseq.nb ,nb current-nb) (return t)))) 
;
;
; =============================================================================
;
; compl.nbs     
; ---------
;
;       arguments     : nbs1 - <node bind set>
;                       nbs2 - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : It returns the <node bind set> composed of the
;                       <node bind>s that are in "nbs1" but not in "nbs2".
;
;       implementation: testing for a <new node bind set> in the case of nbs2 
;                       and not testing in the case of nbs1 makes the
;                       compiled version faster (IT WAS TESTED).
;
;                                        written:  ejm 06/30/84
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro compl.nbs (nbs1 nbs2)
  `(cond ((isnew.nbs ,nbs2) ,nbs1)
	 (t (mapcan #'(lambda (nb)
			(if (not (ismemb.nbs nb ,nbs2)) 
			  (list nb)))
		    ,nbs1))))
;
;
; =============================================================================
;
; union.nbs
; ---------
;
;       arguments     : nbs1 - <node bind set>
;                       nbs2 - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : returns the union of "nbs1" and "nbs2".
;
;       implementation: testing for a <new node bind set> in the case of nbs2 
;                       and not testing in the case of nbs1 makes the
;                       compiled version faster (IT WAS TESTED).
;
;                                        written:  rmo 08/02/83
;                                        modified: ejm 07/10/84
;                                        translated: rmq 02/18/86
;
;
(defmacro union.nbs (nbs1 nbs2)
   `(cond ((isnew.nbs ,nbs2) ,nbs1)
          (t (nconc (compl.nbs ,nbs1 ,nbs2) ,nbs2))))
;
;
; =============================================================================
;
; choose.nbs    
; ----------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node bind>
;
;       description   : returns a <node bind> of "nbs".
;
;                                        written:  rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro choose.nbs (nbs)
   `(first ,nbs))
;
;
; =============================================================================
;
; others.nbs    
; ----------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node bind set> 
;
;       description   : returns a <node bind set> identical to "nbs" but without
;                       the element that would be chosen by "choose.nbs".
;
;                                        written:  rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro others.nbs (nbs)
   `(rest ,nbs))
;
;
; =============================================================================
;
; intersect.nbs 
; -------------
;
;       arguments     : nbs1 - <node bind set>
;                       nbs2 - <node bind set>
;
;       returns       : <node bind set>
;
;       description   :    It returns the intersection of "nbs1" and "nbs2".
;                          Generally "nbs1" is the set of <node>s found so far,
;                       taking in consideration the previous <cable>s.
;                       On the other hand, "nbs2" is a set of <node>s just 
;                       found, considering the <cable> being matched currently.
;                       The <node>s in both <set>s are subjected to a certain 
;                       number of constraints -- the corresponding <bind set>s. 
;                       So, these two <set>s are indeed <node bind set>s, i.e.,
;                       <set>s of <node bind>s.
;                          The intersection
;                       of the two <node bind set>s is a <node bind set>
;                       where the <node>s are the <node>s of "nbs1" and "nbs2"
;                       whose constraints are not incompatible (see data
;                       structures <node bind> and <bind set>).
;                          Several cases are possible:
;                       1) A <node> which appears in one <node bind set> only,
;                          is not part of the result.
;                       2) A <node> which appears in both <node bind set>s and
;                          whose constraints are incompatible does not become
;                          part of the result either.
;                       3) A <node> which appears in both <node bind set>s and
;                          whose constraints are compatible becomes part of the
;                          result with a constraint that is the intersection
;                          of the two original constraints.
;
;                                        written:  ejm 06/30/84
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro intersect.nbs (nbs1 nbs2)
  `(if (and ,nbs1 ,nbs2)
     (mapcan #'(lambda (nb1)
		 (mapcan #'(lambda (nb2) (intersect.nb nb1 nb2))
			 ,nbs2))
	     ,nbs1)))
;
;
; =============================================================================
;
; issubset.nbs 
; -------------
;
;       arguments     : nbs1 - <node bind set>
;                       nbs2 - <node bind set>
;
;       returns       : <boolean> 
;
;       description   : Returns "true" if nbs1 is a subset of nbs2,
;                       "false" otherwise.
;
;                                        written:  rmo 08/02/83
;                                        modified: ejm 07/10/84
;                                        translated: rmq 02/18/86
;                                        modified: ssc  5/10/89
;
;
(defmacro issubset.nbs (nbs1 nbs2)
  `(do.nbs (current-nb ,nbs1 t) 
     (unless (ismemb.nbs current-nb ,nbs2) (return nil)))) 
;
;
; =============================================================================
;
; iseq.nbs 
; --------
;
;       arguments     : nbs1 - <node bind set>
;                       nbs2 - <node bind set>
;
;       returns       : <boolean>
;
;       description   : Compares "nbs1" and "nbs2" as <sets> of <node bind>s.
;                       If they are equal, the function returns "true",
;                       otherwise it returns "false".
;
;       implementation: NOT EFFICIENT!
;
;                                        written:  rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro iseq.nbs (nbs1 nbs2)
    `(and (issubset.nbs ,nbs1 ,nbs2)
          (issubset.nbs ,nbs2 ,nbs1)))
;
;
; =============================================================================



    
    




