;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: infixfns.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


; ==========================================================================
;
; = 
; -
;
;      arguments     : ns-sv - (<ns-exp> x <svar>)
;
;      returns       : <node set>
;
;      description   : The <svar> is assigned the <node set> resulting of the
;                      "snevaluation" of the <ns-exp>.
;
;                                         written:  CCC 08/02/83
;                                         modified: ejm 10/23/83
;                                                   njm 04/28/89
;                                                   hc  07/18/93

(defsnepscom = ((nsvar &optional sv)
		(top rearrange ns bns tbns fns))
  (=-fun nsvar sv))

(defun =-fun (nsvar sv)
  (let ((ns (nseval nsvar)))
    (cond ((issys.sv sv)
	   (sneps-error (format nil
				"Attempting to change value of system variable ~a"
				sv)
			"assigment operator"
			"="))
	  (t (set.sv sv ns)))
    (values ns (value.sv 'defaultct))))
 
; ==========================================================================
;
; + 
; -
;
;      arguments     : ns-ns (<ns-exp> x <ns-exp>)
;
;      returns       : <node set>
;
;      description   : The union of the two <node set>s resulting from
;                      "snevaluating" the two <ns-exp>s.
;                      The result is the set of nodes which are either in 
;                      the first or in the second <node set>.
;
;                                         written:  ejm 06/01/84
;                                         modified: njm 04/28/89
;                                                   hc  07/18/93
;                                        modified:  scs/flj  6/20/04
;
;
(defsnepscom + ((&rest ns-ns) =)
  (values (if ns-ns
	      (reduce #'union.ns
		      (mapcar #'(lambda (n) (buildeval n t))
			      ns-ns))
	    (new.ns))
	  (value.sv 'defaultct)))
 
; ==========================================================================
;
; & 
; -
;
;      arguments     : ns-ns (<ns-exp> x <ns-exp>)
;
;      returns       : <node set>
;
;      description   : The intersection of the two <node set>s resulting 
;                      from "snevaluating" the two <ns-exp>s.
;                      The result is the set of nodes which are both in 
;                      the first and in the second <node set>.
;
;                                         written:  ejm 06/01/84
;                                         modified: njm 04/28/89
;                                                   hc  07/18/93
;                                        modified:  scs/flj  6/20/04
;
;
(defsnepscom & ((&rest ns-ns) =)
    (values (if ns-ns
	      (reduce #'intersect.ns
		      (mapcar #'(lambda (n) (buildeval n t))
			      ns-ns))
	    (new.ns))
	  (value.sv 'defaultct)))
 
; ==========================================================================
;
; - 
; -
;
;      arguments     : ns-ns (<ns-exp> x <ns-exp>)
;
;      returns       : <node set>
;
;      description   : The set difference of the two <node set>s resulting 
;                      from "snevaluating" the two <ns-exp>s.
;                      The result is the set of nodes which are in the 
;                      first but not in the second <node set>.
;
;                                         written:  CCC 08/02/83
;                                         modified: ejm 10/25/83
;                                                   njm 04/28/89
;                                                   hc  07/18/93
;
(defsnepscom - ((&rest ns-ns) =)
  (values (compl.ns (nseval (first ns-ns)) (nseval (second ns-ns)))
	  (value.sv 'defaultct)))
 
; ==========================================================================
;
; exceptrels.ns 
; -------------
;
;      arguments     : ns   - <node set> 
;                      rels - <relation set>
;
;      returns       : <node set>
;
;      description   : Returns the <node set> obtained by restricting
;                      "ns" to those <node>s which do not have any arcs in 
;                      the <relation set> "rels".
;
;                                         written:  CCC 08/02/83
;                                         modified:
;
(defun exceptrels.ns (ns rels)
  (declare (special ns rels))
  (cond ((and ns rels)
	 (mapcan #'(lambda (n)
		     (declare (special n))
		     (if (not (anyrels.n n rels)) (list n)))
		 ns))
	(t ns)))
 
; ==========================================================================
;
; anyrels.n 
; ---------
;
;      arguments     : n    - <node>  
;                      rels - <relation set>
;
;      returns       : <boolean>
;
;      description   : If the <node> "n" has any arcs in the <relation set> 
;                      "rels", then it returns "true", otherwise returns 
;                      "false".
;
;                                         written:  CCC 08/02/83
;                                         modified: SCS 06/06/87
;
(defun anyrels.n (n rels)
  "Returns T if the node N has any outgoing arcs in the relation set RELS."
  (do ((rs rels (others.rs rs)))
      ((isnew.rs rs) nil)
    (if (nodeset.n n (choose.rs rs)) (return t))))
 
; ==========================================================================
;
; >
; -
;
;      arguments     : rs-sv  - (<rs-exp> x <svar>)
;
;      returns       : <relation set>
;
;      description   : The <s-var> is assigned the <relation set> obtained from 
;                      "rsevaluating" the <rs-exp>.
;
;                                         written:  CCC 08/15/83
;                                         modified: ejm 10/25/83
;                                                   njm 04/28/89
;                                                   hc  07/18/93
;
(defsnepscom > ((&rest rs-sv) (top rearrange rs))
  (values (set.sv (second rs-sv) (rseval (first rs-sv)))
	  (value.sv 'defaultct)))
 
; ==========================================================================
;
; _
; -
;
;      arguments     : ns-rs  - (<ns-exp> x <rs-exp>)
;
;      returns       : <node set>
;
;      description   : It "snevaluates" the <ns-exp>, and removes 
;                      from the resulting <node set> any <node>s which have
;                      an arc in the <relation set> obtained from "rsevaluating"
;                      the <rs-exp>.
;
;                                         written:  CCC 08/15/83
;                                         modified: ejm 10/25/83
;                                                   njm 04/28/89
;                                                   hc  07/18/93
;
(defsnepscom _ ((&rest ns-rs) =)
  (values (exceptrels.ns (nseval (first ns-rs)) (rseval (second ns-rs)))
	  (value.sv 'defaultct)))
 
; ==========================================================================
;
; !
; -
;
;       arguments     :  snepsul-exp - <ns-exp> (+ context)
;
;       returns       : <node set> x <context>
;
;       description   : It asserts the <molecular nodes> in the <node set>
;                       resulting from "snevaluating" the <ns-exp> in the
;                       context specified in the context description of
;                       <snepsul-exp>. It prints a warning for each
;                       non-<molecular node> in that <node set>.
;
;       side-effects  : It side-effects the system <svar> "assertions". 
;
;                                        written : CCC 
;                                        modified: ejm 10/25/83
;                                                  njm 10/12/88
;                                                  njm 04/28/89
;                                                  njm/hc 05/10/89
;                                                  hc  07/18/93
;
(defsnepscom ! ((&rest snepsul-exp) (top rearrange ns bns tbns fns))
  (let ((crntct (processcontextdescr snepsul-exp)))
    (declare (special crntct))
    (values (assert-nodes (nseval (getsndescr snepsul-exp)) crntct)
	    crntct)))



    
    




