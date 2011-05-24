;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: cableset.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
; Data Type:  <cable set> ::=  { <cable> <cable> ... <cable> }
;
; R[<cable set>] = (<upcable> ... <upcable> <downcable> ... <downcable>)
;
;           where <upcable>   ::= <uprel>   - <nodeset>
;                 <downcable> ::= <downrel> - <nodeset>
;
; =============================================================================
; 
; =============================================================================
;
; new.cs 
; ------
;
;       arguments     : none
;
;       returns       : <cable set>
;
;       description   : Creates a <newcableset>.
;
;                                        written:  rmo 07/28/83
;                                        modified:
;
;
(defun new.cs ()
   `())
;
;
; =============================================================================
;
; is.cs
; -----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <cable set>,
;                       "false" otherwise. 
;
;       implementation: it does not check if all the elements of "u" are
;                       <cable>s, it just checks the first one.
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defmacro is.cs (u) 
   `(or (null ,u)
        (and (listp  ,u)
             (is.c (first ,u))))) 
;
;
; =============================================================================
;
; isnew.cs
; --------
;
;       arguments     : cs - <cable set>
;
;       returns       : <boolean> 
;
;       description   : returns "true" if "cs" is a <newcableset>,
;                       "false" otherwise.
;
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defmacro isnew.cs (cs)
   `(null ,cs))
;
; 
; =============================================================================
;
; insert.cs 
; ---------
;
;       arguments    : c - <cable>
;                      cs - <cable set>
;
;       returns      : <cable set>
;
;       description  : it updates "cs" with "c"
;                      There are 2 possibilities : 
;                         1) If "cs" does not contain a <cable> with the
;                            same <relation> of "c", the function will insert
;                            "c" in "cs";
;                         2) Otherwise, the function will insert all elements of
;                            the <node set> of "c" in the <node set> of that
;                            <cable> which are not there yet.
;
;             Examples: -> (insert.cs '(rel1 . (a b))
;                                     '((rel2 . (e f))))
;                          ( (rel2 . (e f)) (rel1 . (a b)) )
;
;                          (insert.cs '(rel1 . (a b))
;                                     '( (rel1 . (d r a))  (rel2 . (c a b)) ))
;                          ( (rel2 . (c a b))  (rel1 . (b d r a)) )
;                                 
;
;       implementation: dbucket - destination bucket
;                       sbucket - source bucket
;                       cc - current cable, i.e., first <cable> of dbucket
;                       rel - <relation> of c
;
;                                        written:  rmo 07/28/83
;                                        modified: ejm 08/19/83
;                                                  ssc  5/10/89
;
;

(defun insert.cs (c cs)
  (declare (special c cs))
  (cond ((assoc (relation.c c) cs :test #'eq)
	 (do* ((dbucket nil (cons cc dbucket))
	       (sbucket cs (rest sbucket))
	       (cc (first sbucket) (first sbucket))
	       (rel (relation.c c)))
	      ((null sbucket) (nreverse (cons c dbucket)))
	   (when (iseq.r (relation.c cc) rel)
	     (return
	       (nreverse
		 (nconc (nreverse (rest sbucket))
			(cons (update.c (nodeset.c c) cc)
			      dbucket)))))))
        (t (nreverse (cons c cs)))))
;
; 
; =============================================================================
;
; replace.cs 
; ----------
;
;       arguments    : c - <cable>
;                      cs - <cable set>
;
;       returns      : <cable set>
;
;       description  : it updates "cs" with "c"
;                      There are 2 possibilities : 
;                         1) If "cs" does not contain a <cable> with the
;                            same <relation> of "c", the function will insert
;                            "c" in "cs";
;                         2) Otherwise, the function will replace all elements of
;                            the old <node set> with the <node set> of "c".
;
;             Examples: -> (insert.cs '(rel1 . (a b))
;                                     '((rel2 . (e f))))
;                          ( (rel2 . (e f)) (rel1 . (a b)) )
;
;                          (insert.cs '(rel1 . (a b))
;                                     '( (rel1 . (d r a))  (rel2 . (c a b)) ))
;                          ( (rel2 . (c a b))  (rel1 . (a b)) )
;                                 
;
;       implementation: dbucket - destination bucket
;                       sbucket - source bucket
;                       cc - current cable, i.e., first <cable> of dbucket
;                       rel - <relation> of c
;
;                                        written:  scs 02/13/87
;                                        modified: ssc  5/10/89
;
;

(defun replace.cs (c cs)
  (cond ((assoc (relation.c c) cs :test #'eq)
	 (do* ((dbucket nil (cons cc dbucket))
	       (sbucket cs (rest sbucket))
	       (cc (first sbucket) (first sbucket))
	       (rel (relation.c c)))
	      ((null sbucket) (nreverse (cons c dbucket)))
	   (when (iseq.r (relation.c cc) rel)
	     (if (isnew.ns (nodeset.c c))
		 (return (nreverse
			   (nconc (nreverse (rest sbucket))
				  dbucket)))
		 (return (nreverse
			   (nconc (nreverse (rest sbucket))
				  (cons c dbucket))))))))
        (t (nreverse (cons c cs)))))
;
; 
; =============================================================================
;
; putin.cs
; --------
;
;       arguments     : c - <cable>
;                     : cs - <cable set>
;
;       returns       : <cable set>
;
;       description   : returns a <cable set> identical to "cs" but with the
;                       aditional <cable> "c".
;
;       note          : it does not check for repetitions. 
;
;                                        written :  ejm 10/21/83
;                                        modified:  flj  8/26/04 macro->defun
;
;
(defun putin.cs (c cs)
   (cons c cs))
;
;
; =============================================================================
;
; choose.cs
; ---------
;
;       arguments     : cs - <cable set>
;
;       returns       : <cable> 
;
;       description   : returns an element of "cs".
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defun choose.cs (cs)
   (first cs))
;
;
; =============================================================================
;
; others.cs 
; ---------
;
;       arguments     : cs - <cable set>
;
;       returns       : <cable set> 
;
;       description   : returns a <cable set> identical to "cs" but without
;                       the element that would be chosen by "choose.cs".
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defun others.cs (cs)
   (rest cs))
;
;
; =============================================================================
(defmacro do.cs ((var csform &optional resultform) &body forms)
  "iterator for cablesets."
  `(dolist (,var ,csform ,resultform) ,@forms))
; =============================================================================
;
; down.cs 
; ---------
;
;       arguments     : cs - <cable set>
;
;       returns       : <down cable set> 
;
;       description   : returns the <down cable set> consisting of those 
;                       <cable>s in "cs" whose <relation>s are 
;                       <descending relation>s. It can be nil.
;
;       implementation: It uses the fact that all of those <cable>s in "cs"
;                       whose <relation>s are <ascending relation>s will 
;                       be at the front of the <sequence> which 
;                       represents "cs", so that as soon as we find
;                       a <descending relation> we return the rest of "cs".
;
;                                        written:  vhs 07/27/84
;                                        modified: vhs 08/24/84
;                                        modified: scs 02/11/88
;
;
(defmacro down.cs (cs)
  `(do ((current-cs ,cs (others.cs current-cs)))
       ((or (null current-cs) (isdn.r (relation.c (choose.cs current-cs))))
	current-cs)))
;
;
; =============================================================================
;
; ismembrel.cs 
; ------------
;
;       arguments     : r  - <relation>
;                       cs - <cable set>
;
;       returns       : cable or nil 
;
;       description   : checks whether there is a <cable> in "cs" whose 
;                       <relation> is "r".  If there is, it
;                       returns the cable, otherwise it returns "false" .
;
;                                        written:  rmo 08/01/83
;                                        modified: scs/flj 6/27/04
;
;
(defmacro ismembrel.cs (r cs) 
  `(assoc ,r ,cs :test #'eq))

;
;
; =============================================================================
;
; nodeset.cs 
; ------------
;
;       arguments     : r  - <relation>
;                       cs - <cable set>
;
;       returns       : <nodeset> 
;
;       description   : Returns the <nodeset> in cs whose <relation> is "r".
;                       Returns nil if there is none
;
;                                        written:  scs 02/13/87
;                                        modified:
;
;
(defmacro nodeset.cs (r cs)
  `(cdr (assoc ,r ,cs)))
;
; =============================================================================
;
; ismemb.cs 
; ---------
;
;       arguments     : c - <cable>
;                       cs - <cable set>
;
;       returns       : <boolean> 
;
;       description   : checks whether "c" is a member of "cs" by checking
;                       if there is a <cable> in "cs" with the same <relation>
;                       and <node set> of "c".
;                       If there is, it returns t, otherwise it returns "false".
;
;
;                                        written:  rmo 07/28/83
;                                        modified: ejm 08/30/83
;                                                  ssc  5/10/89
;
;
;
(defmacro ismemb.cs (c cs)
  `(let ((cable (assoc (relation.c ,c) ,cs :test #'eq)))
     (cond ((null cable) nil)
	   (t (issubset.ns (nodeset.c ,c) (nodeset.c cable)))))) 
;
;
; =============================================================================
;
; issubset.cs  
; -----------
;
;       arguments     : cs1 - <cable set>
;                       cs2 - <cable set>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "cs1" is a subset of "cs2",
;                       "false" otherwise.
;
;
;                                        written:  rmo 07/28/83
;                                        modified: ssc  5/10/89
;
;
(defmacro issubset.cs (cs1 cs2)
  `(do.cs (current-c ,cs1 t) 
     (unless (ismemb.cs current-c ,cs2) (return nil))))
;
; 
; =============================================================================
;
; iseq.cs 
; -------
;
;       arguments     : cs1 - <cable set>
;                       cs2 - <cable set>
; 
;       returns       : <boolean>
;
;       description   : returns "true" if "cs1" has the same 
;                       components as "cs2", "false" otherwise.
;                       Note that the order of the components is irrelevant.
;
;
;                                        written:  rmo 07/28/63
;                                        modified:
;
;
(defmacro iseq.cs (cs1 cs2)
   `(and (issubset.cs ,cs1 ,cs2)
         (issubset.cs ,cs2 ,cs1)))
;
;
; =============================================================================
;
; describe.cs
; -----------
;
;       arguments     : cs - <cable set>
;
;       returns       : <sequence>
;
;       description   : It returns a <sequence> which is a description of 
;                       the <cable set> "cs" to be printed.
;
;                                        written :  ejm 06/05/84
;                                        modified:
;
(defmacro describe.cs (cs)
  `(mapcar #'(lambda (c) (describe.c c))
	   ,cs))
;
;
; =============================================================================
;
; apply-subst.cs
; --------------
;
;       arguments     : subst - <substitution>
;                       cs - <cable set>
;
;       returns       : <cable set>
;
;       description   : applies the <substitution> "subst" to the <cable set>
;                       "cs" and returns the result
;
;                                        written :  rgh 08/21/85
;                                        modified:
;
;
(defmacro apply-subst.cs (subst cs)
  `(prog (result next-c remaining-cs)
	 (setq remaining-cs ,cs)
	 (setq result (new.cs))
      begin
	 (if (isnew.cs remaining-cs) (return result))
	 (setq next-c (choose.cs remaining-cs))
	 (setq result
	       (putin.cs
		 (new.c (relation.c next-c)
			(apply-subst.ns ,subst (nodeset.c next-c)))
		 result))
	 (setq remaining-cs (others.cs remaining-cs))
	 (go begin)))
;
;
; =============================================================================
;
; clean-quantifiers.cs
; --------------------
;
;       arguments     : dcs - <downcable set>
;
;       returns       :  <downcable set>
;
;       description   : Returns a <downcable set> like dcs,
;                       but removes any quantifier wire to a constant node,
;                       and adds quantifier wires to any free variable.
;
;       implementation: Assume that if there are any free variables,
;                       they are all quantified by the same quantifier.
;
;                                        written :  scs  4/22/88
;                                        modified:  njm  2/10/89
;
;       modifications:  Used insert.cs instead of putin.cs because of message
;                       which said that the same node uses two quantifiers but
;                       it didn't
;
(defun clean-quantifiers.cs (dcs)
  "Returns a <downcable set> like dcs,
   but removes any quantifier wire to a constant node,
   and adds quantifier wires to any free variable."
  ;; written by: scs 4/22/88
  (let ((resultcs (new.cs)))
    (dolist (cable dcs resultcs)
      (let ((varns (remove-if-not.ns #'(lambda (n) (isvar.n n)) (nodeset.c cable))))
	(cond ((isquant.r (relation.c cable))
	       (unless (isnew.ns varns)
		 (setq resultcs (insert.cs (new.c (relation.c cable) varns) resultcs))))
	      (t (when varns
		   (setq resultcs
			 (insert.cs (new.c (quantifier-of.n (choose.ns varns)) varns)
				    resultcs)))
		 (setq resultcs (putin.cs cable resultcs))))))))




    
    




