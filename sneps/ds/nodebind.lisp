;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: nodebind.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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
; Data Type:  <node bind> ::= <node> - <bind set>
;         
; R[<node bind set>] ::= ( R[<node>] . R[<bind set>] )
;
; =============================================================================
;
; ABSTRACTION:
;
;    A <node bind> is a pair of a <node> and a <bind set>. A <node bind> can be 
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
;    The representation of a <node bind> is a dotted pair whose "car" is the
; representation of its <node> and whose "cdr" is the representation of its
; <bind set>.
;
;                                         ejm, vhs 07/10/84
;
;
;
; =============================================================================
;
; new.nb 
; ------
;
;       arguments     : n - <node>
;                       bs - <bind set>
;
;       returns       : <node bind>
;
;       description   : Creates a <node bind> from "n" and "bs".
;
;                                        written : rmo 08/02/83
;                                        modified:
;                                        translated: robert quinn 02/18/86
;
;
(defmacro new.nb (n bs)
   `(cons ,n ,bs))
;
;
; =============================================================================
;
; is.nb 
; -----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <node bind>, "false"
;                       otherwise.
;
;
;                                        written : rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
;(defmacro is.nb (u)
;    `(and (dtpr ,u) 
;          (is.n (car ,u))
;          (is.bs (cdr ,u))))
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defmacro is.nb (u)
    `(and (consp ,u) 
          (is.n (car ,u))
          (is.bs (cdr ,u))))
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
;
; =============================================================================
;
; node.nb 
; -------
;
;       arguments     : nb - <node bind>
;
;       returns       : <node> 
;
;       description   : Returns the <node> of "nb". 
;
;
;                                        written : rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defun node.nb (nb)
   (car nb))
;
;
; =============================================================================
;
; bindset.nb 
; ----------
;
;       arguments     : nb - <node bind>
;
;       returns       : <bind set> 
;
;       description   : Returns the <bind set> of "nb". 
;
;                                        written : rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro bindset.nb (nb)
   `(cdr ,nb))
;
;
; =============================================================================
;
; update.nb 
; ---------
;
;       arguments     : nb - <node bind>
;                       bs - <bind set>
;
;       returns       : <node bind>
;
;       description   : Returns a <node bind> composed of the <node> of "nb"
;                       and a <bind set> which is the union of "bs" with the
;                       <bind set> of "nb".
;
;
;                                        written : rmo 08/02/83
;                                        modified: ejm 08/30/83
;                                        translated: rmq 02/18/86
;
;
(defmacro update.nb (nb bs)
   `(cons (car ,nb)
          (union.bs ,bs (cdr ,nb))))
;
;
; =============================================================================
;
; intersect.nb
; ------------
;
;       arguments     : nb1 - <node bind>
;                       nb2 - <node bind>
;
;       returns       : (<node bind>) | nil
;
;       description   : It returns the intersection of "nb1" and "nb2"
;                       as the only member of a <list> (to be used my 
;                       mapcan).
;                       Generally "nb1" is one of the <node>s found so far
;                       (taking in consideration the previous <cable>s),
;                       subjected to a certain number of constraints -- the
;                       correspondent <bind set>. On the other hand, "nb2"
;                       is on of the <node>s newly found (considering the
;                       <cable> being matched currently), subjected also to
;                       a certain number of constraints. The intersection
;                       of the two <node bind>s is either a <node bind> or 
;                       nothing. If the two <node bind>s have different 
;                       <node>s the result is nil (a list with nothing). 
;                       If the <node>s are the same but the constraints are 
;                       incompatible the result is also nil. If the <node>s 
;                       are the same and their constraints are compatible 
;                       the result is a <list> with a <node bind> with the 
;                       same <node> and a constraint (<bind set>) that is 
;                       the intersection of the two original constraints 
;                       (<bind set>s).
;
;                                        written :  ejm 06/30/84
;                                        modified:
;                                        translated: rmq 02/18/86
;                                                    ssc  5/10/89
;
;
;
;
(defmacro intersect.nb (nb1 nb2)
   `(let (result-bs)
        (cond ((iseq.n (node.nb ,nb1) (node.nb ,nb2))
               (setq result-bs (intersect.bs (bindset.nb ,nb1)
                                             (bindset.nb ,nb2)))
               (cond ((eq result-bs 'failure) nil)
                     (t (list (new.nb (node.nb ,nb1) 
                                      result-bs)))))
              (t nil))))
;
;
; =============================================================================
;
; iseq.nb      
; -------
;
;       arguments     : nb1 - <node bind>
;                       nb2 - <node bind>
;
;       returns       : <boolean>
;
;       description   : Returns "true" if the two <node bind>s are equivalent,
;                       "false" otherwise.
;
;                                        written : rmo 08/02/83
;                                        modified:
;                                        translated: rmq 02/18/86
;
;
(defmacro iseq.nb (nb1 nb2)
   `(and (iseq.n (node.nb ,nb1) (node.nb ,nb2))
         (iseq.bs (bindset.nb ,nb1) (bindset.nb ,nb2))))
;
;
; =============================================================================



    
    




