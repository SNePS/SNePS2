;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: bind.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
; Data Type:  <bind> ::= <var> - <node set>
;         
; R[<bind>] ::= ( R[<var>] . R[<node set>] )
;
; =============================================================================
;
; ABSTRACTION:
;
;    A <bind> is a pair composed of a <var> and a <node set>. Each <bind> can
; be thought of as a constrain on a <var> relevant to a search. The <var> is
; either a <svar> or a <variable node>. In the latter case, the <node set> is
; composed by just one <node>.
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
;    The representation of a <bind> is a dotted pair whose "car" is the
; representation of its <var> and whose "cdr" is the representation of
; its <node set>.
;
;                                         ejm, vhs 07/10/84
;
;
;
; =============================================================================
;
; new.b
; -----
;
;       arguments     : var - <var> 
;                       ns -  <node set>
;
;       returns       : <bind>
;
;       description   : Creates a <newbind> from "var" and "ns".
;
;                                  written:  rmo 07/28/83
;                                  modified:
;
;
(defmacro new.b (var ns)
   `(cons ,var ,ns))
;
;
; =============================================================================
;
; is.b
; ----
;
;       arguments      : u - <universal>
;
;       returns        : <boolean>
;
;       description    : returns "true" if "u" is a <bind>, "false" otherwise.
;
;
;                                  written:  rmo 07/28/83
;                                  modified: ejm 09/07/83
;
;
;(defmacro is.b (u)
;  `(and (dtpr ,u)     
;         (is.v (car ,u)) 
;         (is.ns (cdr ,u))))
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defmacro is.b (u)
   `(and (consp ,u)
         (is.v (car ,u)) 
         (is.ns (cdr ,u))))
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
; =============================================================================
;
; expand.b
; --------
;
;        arguments        : bind - <bind>
;                           nodeset - <node set>
;
;        returns          : <bind>
;
;        description      : Given a <bind> "bind" and a <node set>
;                           "nodeset", the  function returns a <bind>
;                           whose <bind> is the "bind"'s <variable>
;                           and whose <node set> is the union of
;                           "nodeset" with the "bind"'s <node set>.
;
;                                  written:  rmo 07/28/83
;                                  modified:
;
(defmacro expand.b (bind nodeset)
   `(cons (car ,bind)
          (union.ns (cdr ,bind) ,nodeset)))
;
;
; =============================================================================
;
; var.b
; -----
;
;        arguments      : bind -  <bind>
; 
;        returns        : <var> 
; 
;        description    : returns the <var> of "bind".
;
;                                  written:  rmo 07/28/83
;                                  modified:
;
;
(defmacro var.b (bind)
   `(car  ,bind))
;
;
; =============================================================================
;
; nodeset.b
; ---------
;
;        arguments       : bind - <bind> 
;          
;        returns         : <node set>
; 
;        description     : returns the <node set> of "bind".
;                             
;
;                                  written:  rmo 07/28/83
;                                  modified:
;
(defmacro nodeset.b (bind)
   `(cdr ,bind))
;
;     
; =============================================================================
;
; iseq.b
; ------
;
;       arguments     : bind1 - <bind>
;                       bind2 - <bind>
;
;       returns       : <boolean>
;
;  
;       description   : Compares "bind1" and "bind2" as <bind>s : returns "true"
;                       if "bind1" and "bind2" have the same <var> and the same
;                       <node set>, "false" otherwise.
;
;                                  written:  rmo 07/28/83
;                                  modified:
;
(defmacro iseq.b (bind1 bind2)
   `(and (iseq.v  (var.b ,bind1) (var.b ,bind2))
         (iseq.ns (nodeset.b ,bind1) (nodeset.b ,bind2))))
;
;   
; =============================================================================




    
    




