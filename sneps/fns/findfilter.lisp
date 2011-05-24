;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: findfilter.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
; findassert
; ----------
;
;       arguments     : snepsul-exp - <snd>
;
;       returns       : result - <node set>
;
;       description   : This function uses the informations given by 
;                       snepsul-exp to find an assertion node set, in the
;                       current context. 
;                       
;
;       implementation: similar to the find, except call the finda1 in which 
;                       the results will be filter by the function 
;                       assertion-filter.
;
;                                          written : CCC 10/20/83
;                                          modified: CCC 11/21/83
;                                          modified: CPF 09/30/88
;                                                    njm 04/27/89
;                                                    hc  07/18/93
;
(defsnepscom findassert ((&rest snd) (top ns bns tbns fns))
  (let ((crntct (processcontextdescr snd)))
    (declare (special crntct))
    (values (in-context.ns (nbs-to-ns (find-filter (fevalsnd (getsndescr snd))
						   'assert-filter))
			   (iscontext-given snd))
	    crntct)))

;
; ==========================================================================
;
; assert-filter 
; ----------------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : filter the given nodebindset and return those that
;                       leading by assertion node.
;
;       implementation: use mapcan.
;
;                                          written : CCC 10/20/83
;                                          modified:
;
(defun assert-filter (nbs)
  (declare (special nbs))
  (mapcan #'(lambda (nb)
	      (declare (special nb))
	      (if (isassert.n (node.nb nb)) (list nb)))
	  nbs))
 
;
; ==========================================================================
;
; findconstant
; ------------
;
;       arguments     : snepsul-exp - <snd>
;
;       returns       : result - <node set>
;
;       description   : This function use the informations given by 
;                       snepsul-exp to find a constant node set, in
;                       the current context , which every node
;                       satisfies the specifications of the snepsul-exp.
;
;                       
;       implementation: similar to the find, except call the finda1 in which 
;                       the results will be filter by the function 
;                       constant-filter.
;
;                                          written : CCC 10/20/83
;                                          modified: CCC 11/21/83
;                                                    CPF 09/30/88
;                                                    njm 04/27/89
;                                                    hc  07/18/93
;
(defsnepscom findconstant ((&rest snd) findassert)
  (let ((crntct (processcontextdescr snd)))
    (declare (special crntct))
    (values (in-context.ns (nbs-to-ns (find-filter (fevalsnd (getsndescr snd))
						   'constant-filter))
			   (iscontext-given snd))
	    crntct)))

;
; ==========================================================================
;
; constant-filter 
; ----------------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : filter the given nodebindset and return those that
;                       leading by constant node.
;
;       implementation: use mapcan.
;
;                                          written : CCC 10/20/83
;                                          modified:
;
(defun constant-filter (nbs)
  (declare (special nbs))
  (mapcan #'(lambda (nb)
	      (declare (special nb))
	      (if (ismol.n (node.nb nb)) (list nb)))
	  nbs))
 
;
; ==========================================================================
;
; findpattern
; -----------
;
;       arguments     : snepsul-exp - <snd>
;
;       returns       : result - <node set>
;
;       description   : This function uses the informations given by 
;                       snepsul-exp to find a pattern node set, in the
;                       current context, which every node satisfies the
;                       specifications of the snepsul-exp.
;                       
;
;       implementation: similar to the find, except call the finda1 in which 
;                       the results will be filter by the function 
;                       pattern-filter.
;
;                                          written : CCC 10/20/83
;                                          modified: CCC 11/21/83
;                                                    CPF 09/30/88
;                                                    njm 04/27/89
;                                                    hc  07/18/93
;
(defsnepscom findpattern ((&rest snd) findassert)
  (let ((crntct (processcontextdescr snd)))
    (declare (special crntct))
    (values (in-context.ns (nbs-to-ns (find-filter (fevalsnd (getsndescr snd))
						   'pattern-filter))
			   (iscontext-given snd))
	    crntct)))

;
; ==========================================================================
;
; pattern-filter 
; --------------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : filter the given nodebindset and return those that
;                       leading by pattern node.
;
;       implementation: use mapcan.
;
;                                          written : CCC 10/20/83
;                                          modified:
;
(defun pattern-filter (nbs)
  (declare (special nbs))
  (mapcan #'(lambda (nb)
	      (declare (special nb))
	      (if (ispat.n (node.nb nb)) (list nb)))
	  nbs))
 
;
; ==========================================================================
;
; findbase
; --------
;
;       arguments     : snepsul-exp - <snd>
;
;       returns       : result - <node set>
;
;       description   : This function use the informations given by 
;                       snepsul-exp to find a base node set, in the
;                       current context, which every node satisfies
;                       the specifications of the snepsul-exp.
;                       
;
;       implementation: similar to the find, except call the finda1 in which 
;                       the results will be filter by the function 
;                       base-filter.
;
;                                          written : CCC 10/20/83
;                                          modified: CCC 11/21/83
;                                                    CPF 09/30/88
;                                                    njm 04/27/89
;                                                    hc  07/18/93
;
(defsnepscom findbase ((&rest snd) findassert)
  (let ((crntct (processcontextdescr snd)))
    (declare (special crntct))
    (values (in-context.ns (nbs-to-ns (find-filter (fevalsnd (getsndescr snd))
						   'base-filter))
			   (iscontext-given snd))
	    crntct)))
 
;
; ==========================================================================
;
; base-filter 
; -----------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : filter the given nodebindset and return those that
;                       leading by base node.
;
;       implementation: use mapcan.
;
;                                          written : CCC 10/20/83
;                                          modified:
;
(defun base-filter (nbs)
  (declare (special nbs))
  (mapcan #'(lambda (nb)
	      (declare (special nb))
	      (if (isbase.n (node.nb nb)) (list nb)))
	  nbs))
 
;
; ==========================================================================
;
; findvariable
; ------------
;
;       arguments     : snepsul-exp - <snd>
;
;       returns       : result - <node set>
;
;       description   : This function use the informations given by 
;                       snepsul-exp to find a variable node set, in the
;                       current context, which every node satisfies the
;                       specifications of the snepsul-exp.
;                       
;
;       implementation: similar to the find, except call the finda1 in which 
;                       the results will be filter by the function 
;                       variable-filter.
;
;                                          written : CCC 10/20/83
;                                          modified: CCC 11/21/83
;                                          modified: CPF 09/30/88
;                                                    njm 04/27/89
;                                                    hc  07/18/93
;
(defsnepscom findvariable ((&rest snd) findassert)
  (let ((crntct (processcontextdescr snd)))
    (declare (special crntct))
    (values (in-context.ns (nbs-to-ns (find-filter (fevalsnd (getsndescr snd))
						   'variable-filter))
			   (iscontext-given snd))
	    crntct)))

;
; ==========================================================================
;
; variable-filter 
; ----------------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : filter the given nodebindset and return those that
;                       leading by variable node.
;
;       implementation: use mapcan.
;
;                                          written : CCC 10/20/83
;                                          modified:
;
(defun variable-filter (nbs)
  (declare (special nbs))
  (mapcan #'(lambda (nb)
	      (declare (special nb))
	      (if (isvar.n (node.nb nb)) (list nb)))
	  nbs))
 
; ==========================================================================
;
; find-filter
; -----------
;
;       arguments     : fsnd - (((path.nbs) ... ) . ((path.sv) ... ))
;                       filter - the function of the filter
;
;       returns       : <node bind set>
;
;       description   : return a nodebinset which nodes are filtered
;                       node and satisfy the given fsnd.
;
;       implementation: similar to find1 except the result will pass 
;                       through a filter which select nodes.
;
;                                          written  : CCC 11/22/83
;                                          modified : ssc  5/10/89
;
(defun find-filter (fsnd filter) 
  (when fsnd
    (let* ((consts (car fsnd))
	   (qvars (cdr fsnd))
	   (result (checkconsts-filter consts filter)))
      (cond (consts
	     (if (and result qvars)
		 (checkqvars qvars result)
		 result))
	    (qvars (setq result (apply filter (list (ns-to-nbs (value.sv 'nodes)))))
		   (checkqvars qvars result))))))

 
; ==========================================================================
;
; checkconsts-filter
; ------------------
;
;       arguments     : consts - ((path.nbs) ... )
;
;       returns       : <boolean>
;
;       nonlocal-vars : result - <node bind set>
;
;       description   : return t if all consts has been checked, otherwise
;                       return nil if result become empty.
;
;       side-effects  : result contain the nodebindset which nodes are 
;                       filtered node and satisfy the given consts.
;
;       implementation: similar to checkconsts except the result will
;                       be pass through a filter which select node.
;
;                                          written  : CCC 11/22/83
;                                          modified : ssc  5/10/89
;
;
(defun checkconsts-filter (consts filter) 
  (do* ((result (apply filter (list (pathto (car (first consts))
					    (cdr (first consts)))))
		(intersect.nbs (apply filter
                                      (list (pathto (car (first consts1))
						    (cdr (first consts1)))))
                               result))
	(consts1 (rest consts) (rest consts1)))
       ((null result) nil)
    (unless consts1 (return result))))
 
; ==========================================================================
;
; insidefindassert
; ----------------
;
;       arguments     : snepsul-exp - <snepsul-exp>
;
;       returns       : <node bind set>
;
;       description   : call finda1 to get the nodebindset which satisfy 
;                       the given snepsul-exp
;
;       implementation: similar to findassert except return a nodebindset
;
;                                          written  : CCC 11/23/83
;                                          modified : scs 06/15/89
;
(defmacro insidefindassert (&rest snepsul-exp)
  `(let ((crntct (processcontextdescr ',snepsul-exp)))
     (declare (special crntct))
     (find-filter (fevalsnd (getsndescr ',snepsul-exp)) 'assert-filter)))
 
; ==========================================================================
;
; insidefindconstant
; ----------------
;
;       arguments     : snepsul-exp - <snepsul-exp>
;
;       returns       : <node bind set>
;
;       description   : call finda1 to get the nodebindset which satisfy 
;                       the given snepsul-exp
;
;       implementation: similar to findconstant except return a nodebindset
;
;                                          written  : CCC 11/23/83
;                                          modified : scs 06/15/89
;
(defmacro insidefindconstant (&rest snepsul-exp)
  `(let ((crntct (processcontextdescr ',snepsul-exp)))
     (declare (special crntct))
     (find-filter (fevalsnd (getsndescr ',snepsul-exp)) 'constant-filter)))
 
; ==========================================================================
;
; insidefindpattern
; ----------------
;
;       arguments     : snepsul-exp - <snepsul-exp>
;
;       returns       : <node bind set>
;
;       description   : call finda1 to get the nodebindset which satisfy 
;                       the given snepsul-exp
;
;       implementation: similar to findpattern except return a nodebindset
;
;                                          written  : CCC 11/23/83
;                                          modified : scs 06/15/89
;
(defmacro insidefindpattern (&rest snepsul-exp)
  `(let ((crntct (processcontextdescr ',snepsul-exp)))
     (declare (special crntct))
     (find-filter (fevalsnd (getsndescr ',snepsul-exp)) 'pattern-filter)))
 
; ==========================================================================
;
; insidefindbase
; ----------------
;
;       arguments     : snepsul-exp - <snepsul-exp>
;
;       returns       : <node bind set>
;
;       description   : call finda1 to get the nodebindset which satisfy 
;                       the given snepsul-exp
;
;       implementation: similar to findbase except return a nodebindset
;
;                                          written  : CCC 11/23/83
;                                          modified : scs 06/15/89
;
(defmacro insidefindbase (&rest snepsul-exp)
  `(let ((crntct (processcontextdescr ',snepsul-exp)))
     (declare (special crntct))
     (find-filter (fevalsnd (getsndescr ',snepsul-exp)) 'base-filter)))
 
; ==========================================================================
;
; insidefindvariable
; ----------------
;
;       arguments     : snepsul-exp - <snepsul-exp>
;
;       returns       : <node bind set>
;
;       description   : call finda1 to get the nodebindset which satisfy 
;                       the given snepsul-exp
;
;       implementation: similar to findvariable except return a nodebindset
;
;                                          written  : CCC 11/23/83
;                                          modified : scs 06/15/89
;
 
(defmacro insidefindvariable (&rest snepsul-exp)
  `(let ((crntct (processcontextdescr ',snepsul-exp)))
     (declare (special crntct))
     (find-filter (fevalsnd (getsndescr ',snepsul-exp)) 'variable-filter)))



    
    




