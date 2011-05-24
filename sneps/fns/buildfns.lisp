;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: buildfns.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
; build          
; -----         
;
;      arguments     : snd - <snepsul node description> 
;
;      returns       : <node set>
;
;      description   :   Creates a permanent <node> whose descending 
;                      <relation>s and their corresponding <node set>s 
;                      are obtained by relationset evaluation and
;                      nodeset evaluation, respectively, of each
;                      <relation form> and <nodeset form> in "snd".  
;                      Attaches the new <node> to the network, and 
;                      returns that node.
;
;      side-effects  :   It adds the new <node> to "nodes", and attaches 
;                      its dominated <node>s to it via ascending <relation>s.
;                      The dominated <node>s will be created if they do not
;                      already exist. (See gennewnode, newnode, and 
;                      newtempnode)
;                                         written:  jms 8/04/83
;                                         modified: njm 4/27/89
;                                                   njm 5/12/89
;                                                   hc  7/18/93
;
(defsnepscom build ((&rest snd) (ns bns fns))
  (let ((crntct (value.sv 'defaultct)))
    (values (find-or-build (evalsnd snd t))
	    crntct)))
 
;
;
; ==========================================================================
;
; assert
; ------
;
;      arguments     : snd - <snepsul node description> 
;
;      returns       : <node set> x <context>
;
;      description   : Just expands to an asserted build (q.v.)
;                                         written:  scs 10/30/87
;                                         modified: cpf 10/03/88 (Lisbon)
;                                                   njm  4/27/89
;                                                   hc   7/18/93
;
(defsnepscom assert ((&rest snd) (top ns bns fns))
  (let ((crntct (processcontextdescr snd)))
    (declare (special crntct))
    (values (assert-nodes (cons 'build (getsndescr snd)) crntct)
	    crntct)))


; ==========================================================================
;
; adopt
; -----
;
;      arguments     : snd - <snepsul node description> 
;
;      returns       : <node set> x <context>
;
;      description   : Synonym of assert,
;                      but gets the user used to the difference
;                      between propositions and policies.
;                                         written:  scs 08/18/10
;                                         modified: 
;
(defsnepscom adopt ((&rest snd) (top ns bns fns))
  (let ((crntct (processcontextdescr snd)))
    (declare (special crntct))
    (values (assert-nodes (cons 'build (getsndescr snd)) crntct)
	    crntct)))


; ==========================================================================
;
; assert-nodes
; ------------
;
;       arguments     : snepsul-exp - <ns-exp>
;                       ctname - <svar>
;
;       returns       : <node set> x <context>
;
;       nonlocal-vars : assertions - system <svar>
;
;       description   : It asserts the <molecular nodes> in the <node set>
;                       resulting from "snevaluating" the <snepsul-exp>.
;                       It prints a warning for each non-<molecular node>
;                       in that <node set>.
;
;       modifications : This was good old ! macro. (not quite, ! macro is
;                       only supposed to take a single node).
;
;       side-effects  : It side-effects the system <svar> "assertions". 
;
;                                        written : CCC 
;                                        modified: scs 3/15/08
;                                                  ejm 10/25/83
;                                                  njm 10/12/88
;                                                  njm 04/28/89
;                                                  njm/hc 05/10/89
;                                                  hc 02/08/90 (second value)
;                                                  hc 07/10/90 (proper nodeset
;                                                               handling)
;
;

;;; To enable load to do a batch assert.
(defun assert-nodes (snepsul-exp ctname)
  (declare (special snepsul-exp crntct *assertLater* *newPropositions*))
  (let ((result (new.ns)))
    (do.ns (n (nseval snepsul-exp))
	   (cond ((ismol.n n)
		  (cond ((cl:> *assertLater* 0)
			 (push n *newPropositions*))
			(t (assert.n n ctname)
			   (set.sv 'assertions
				   (insert.ns n (value.sv 'assertions)))))
		  (setq result (insert.ns n result)))
		 (t (sneps-error
		     (format nil "cannot assert non-molecular node ~a" n)
		     'assert 'assert))))
    (values result ctname)))


;
; ==========================================================================
;
; build1        
; ------       
;
;      arguments     : cs - <cable set>
;
;      returns       : <node set>
;
;      description   :    It uses "cs" to build a permanent pattern or 
;                      molecular <node>, which is then attached to the 
;                      network (see build).
;
;                                         written:  jms 8/4/83
;                                         modified:
;
;
(defun build1 (cs)
    (cond ((isnew.cs cs) (new.ns))
          (t (makeone.ns (buildpermnode cs (getfreevars cs t))))))
 
;
;
;
; ==========================================================================
;
; attachnode
; ----------
;
;      arguments     : n  - permanent pattern or molecular <node>
;                      cs - <cable set>
;
;      returns       : <node>
;
;      description   :    Attaches "n" to the network by adding converse 
;                      (ascending) <relation>s between "n" and all of its 
;                      immediately dominated <node>s. 
;
;      side-effects  :    Affects the <node>s dominated by "n".
;
;                                         written:  jms 8/4/83
;                                         modified:
;
;
(defun attachnode (n cs)
    (cond ((isnew.cs cs) n)
          (t (attachnode1 n
                          (relation.c (choose.cs cs))
                          (nodeset.c (choose.cs cs)))
             (attachnode n (others.cs cs)))))
 
;
;
;
; ==========================================================================
;
; evalsnd
; -------
;
;      arguments     : snd       - <snepsul node description>
;                      buildperm - <boolean>
;
;      returns       : <cable set>
;
;      description   :   Builds the <cable set> for a new pattern or 
;                      molecular <node> from "snd". It evaluates each 
;                      <relationform> to obtain the <relation>, and 
;                      each <nodeset form> to obtain the corresponding 
;                      <node set>s. Returns to the top-level sneps 
;                      evaluator upon finding an illegal <relation>, 
;                      which is anything other than a single
;                      descending <relation> in the relation position.
;
;      implementation:  The prog variables defined here are used in the
;                       following functions:
;
;                          rel - evalsnd1 and  evalsnd2 - <relation set>
;                          ns  - evalsnd2               - <node set>
;
;                       Buildperm is set by build and tbuild, and indicates
;                       whether a permanent (true) or temporary (false)
;                       node is being built.
;
;                                         written:  jms 8/4/83
;
;
(defun evalsnd (snd buildperm)
    (declare (special snd buildperm))
    (prog (rel ns)
	  (declare (special rel ns))
          (return
           (cond ((isnew.snd snd)
                  (sneps-error "Attempted (t)build with no arguments"
                               (topfnname buildperm)
                               'evalsnd))
                 (t (evalsnd1 snd (new.cs) buildperm))))))
 
;
;
;
; ==========================================================================
;
; attachnode1
; -----------
;
;      arguments     : n   - <node>
;                      rel - descending <relation>
;                      ns  - <node set>
;
;      returns       : t
;
;      description   :   Attaches "n" to each of its dominated <node>s in 
;                      "ns" through the converse of <relation> "rel". (See 
;                      attachnode and attachnode1).
;
;      side-effects  :   Affects the <node>s in ns.
;
;                                         written:  jms 8/4/83
;                                         modified:
;
;
;
;
;
;
;
(defun attachnode1 (n rel ns)
    (cond ((isnew.ns ns))
          (t (updatenodeset.n (choose.ns ns) (converse.r rel) n)
             (attachnode1 n rel (others.ns ns)))))
 
;
;
;
; ==========================================================================
;
; evalsnd1
; --------
;
;      arguments     : snd - <snepsul node description>
;                      cs  - <cable set> 
;                      buildperm - <boolean>
;
;      returns       : <cable set>
;
;      description   :   Evaluates the next <relationform> in "snd", and 
;                      checks the resulting <relation set> for errors.
;
;      non-local vars:  rel - The <relation set> resulting from evaluating 
;                             the next <relationform>. (Defined in evalsnd)
;                       Buildperm - set by build and tbuild; indicates
;                                   whether a permanent (true) or temporary 
;                                   (false) node is being built.
;                       
;
;                                         written:  jms 08/04/83
;                                         modified: ejm 10/10/83
;
;
(defun evalsnd1 (snd cs buildperm)
    (declare (special rel buildperm))
    (cond ((isnew.snd snd) cs)
          (t (setq rel (rseval (relationform.snd snd)))
             (cond ((isnew.rs rel)
                    (sneps-error "Missing relation"
                                 (topfnname buildperm)
                                 'evalsnd1))
                   ((not (issingleton.rs rel))
                    (sneps-error "error -- build cannot take a relation path"
                                 (topfnname buildperm)
                                 'evalsnd1))
                   ((isup.r rel)
                    (sneps-error (format nil
					 "ascending relation is illegal in build: ~A"
					 (prin1-to-string rel))
                                 (topfnname buildperm)
                                 'evalsnd1))
                   (t (evalsnd2 snd cs buildperm))))))
 
;
;
;
; ==========================================================================
;
; getfreevars  
; -----------
;
;      arguments     : cs        - <cable set>
;                      buildperm - <boolean>
;
;      returns       : <node set>
;
;      description   :   Checks "cs" for the presence of pattern and variable
;                      <node>s. It forms and returns a <node set> containing 
;                      any free (unbound) variable <node>s which will be
;                      dominated by the <node> being built (represented
;                      by its <cable set> "cs"). 
;      non-local vars:  Buildperm - set by build and tbuild, and indicates
;                       whether a permanent (true) or temporary (false)
;                       node is being built.
;                                         written:  jms 8/4/83
;                                         modified: scs 5/18/88
(defun getfreevars (cs buildperm)
  (let (freevars bdvars qflag)
    (do.cs (cable cs freevars)
       (cond ((isquant.r (relation.c cable))
	      (cond (qflag
		     (sneps-error "Only 1 type of quantifier relation allowed "
				  (topfnname buildperm)
				  'getfreevars))
		    (t (setq bdvars (union.ns bdvars (nodeset.c cable))
			     freevars (compl.ns freevars bdvars)
			     qflag t))))
	     (t (do.ns (node (nodeset.c cable))
		   (cond ((isvar.n node)
			  (unless (ismemb.ns node bdvars)
				  (setq freevars (insert.ns node freevars))))
			 ((ispat.n node)
			  (setq freevars
				(union.ns freevars
				      (compl.ns (freevars.n node)
					    bdvars)))))))))))
 
;
;
;
; ==========================================================================
;
; evalsnd2
; --------
;
;      arguments     : snd - <snepsul node description>
;                      cs  - <cable set> 
;                      buildperm - <boolean>
;
;      returns       : <cable set>
;
;      description   :   Evaluates the next <nodesetform> in "snd".  If the
;                      resulting <node set> is not new, it builds the <cable> 
;                      from that <node set> and its <relation>, and adds the 
;                      <cable> to "cs".
;
;      non-local vars:  rel - the <relation> whose <nodesetform> is being
;                             evaluated. (Defined in evalsnd)
;                       ns  - the <node set> resulting from evaluation of
;                             "rel's" <nodesetform>. (Defined in evalsnd)
;                       Buildperm - set by build and tbuild, and indicates
;                       whether a permanent (true) or temporary (false)
;                       node is being built.
;
;                                         written:  jms 8/4/83
;
;
(defun evalsnd2 (snd cs buildperm)
    (declare (special rel ns buildperm))
    (setq ns (buildeval (nodesetform.snd snd) buildperm))
    (cond ((isnew.ns ns) (evalsnd1 (rest.snd snd) cs buildperm))
          (t (evalsnd1 (rest.snd snd) (insert.cs (new.c rel ns) cs) buildperm))))
 
;
;
; ==========================================================================
;
; buildpermnode
; -------------
;
;       arguments     :  cs       - <cable set>
;                        varnodes - <node set>
;
;       returns       :  permanent <pattern node> or <molecular node>
;
;       description   :   Builds a permanent <node> and attaches it to the
;                       network.  If "varnodes", a set of free (unbound) 
;                       variable nodes to be dominated by the <node> being
;                       built, is empty, it builds a <molecular node>, 
;                       otherwise a <pattern node> is built.
;
;       side-effects  :   See buildnode and gennewnode.
;
;                                          written  : jms 8/4/83
;                                          modified : ssc 02/24/87
;
;
(defun buildpermnode (cs varnodes)
    (let ((n (fcs-to-n (cond ((isnew.ns varnodes) (genpmol.n))
			     (t (genppat.n)))
		       (cs-to-fcs cs))))
         (if (ispat.n n) (setfreevars.n n varnodes))
         (attachnode n cs)))
 
;
;
; ==========================================================================
;
; buildeval
; ---------
;
;       arguments     :  nsform    - <snepsul exp>
;                        buildperm - <boolean>
;
;       returns       :  <node set>
;
;       description   :    Evaluates "nsform" (a <nodesetform>, represented 
;                        as a snepsul expression) to produce a <node set>.
;
;       side-effects  :   New <node>s may be built, and added to the 
;                       network, from the evaluation of "nsform".
;
;       implementation:  "buildperm" is true if a permanent <node> is being
;                       built (if a call was made to build), false if a
;                       temporary <node> is being built (tbuild was called).
;
;                                          written  : jms 10/1/83
;                                          modified : ssc 11/04/87
;                                          modified : scs 12/31/87 acc to scottlog
;                                          modified : scs 05/18/88
;
;
(defun buildeval (nsform buildperm)
  (declare (special nsform))
  (cond ((null nsform) nil)
	((numberp nsform) (buildeval (un-ize nsform) buildperm))
	((stringp nsform) (buildeval (string-to-symbol nsform) buildperm))
	((atom nsform)
	 (makeone.ns
		 (or
		  (when (node-p nsform) nsform)
		  (node nsform)
		  (newpbase.n nsform))))
	((is.com (first nsform))
	 (cond ((islegalbcom (first nsform) buildperm)
		(let ((ev-result (protect-eval (eval nsform))))
		  (if (atom ev-result)
		    (buildeval ev-result t)
		    ev-result)))
	       (t
		(sneps-error (format nil "illegal command: ~S" (first nsform))
			     (topfnname buildperm)
			     'buildeval))))
	(t (mapbuildeval nsform buildperm))))
 
;
; ==========================================================================
;
; islegalbcom
; -----------
;
;       arguments     :  com -       <command>
;                        buildperm - <boolean>
;
;       returns       :  <boolean>
;
;       description   :   Tests whether the function call embedded as a
;                       <nodesetform> in a build or tbuild is legal.
;
;       implementation:   Checks the plist of the function name for 
;                       specific function-type tags.
;                       Buildperm - set by build and tbuild; indicates
;                       whether a permanent (true) or temporary (false)
;                       node is being built.
;
;                                          written  : jms 10/1/83
;                                          modified :
;
;
(defun islegalbcom (com buildperm)
    (cond (buildperm (isbns.com com)) (t (istbns.com com))))
 
;
;
; ==========================================================================
;
; mapbuildeval
; ------------
;
;       arguments     :  nsform    - <snepsul expr>
;                        buildperm - <boolean>
;
;       returns       :  <node set>
;
;       description   :   Evaluates all of the <nodesetform>s in "nsform" 
;                       and returns the union of the resulting <node set>s.
;
;       side-effects  :   New nodes will be added to the network if 
;                       buildperm is true (a call to build was made).
;
;                                          written  : jms 10/1/83
;
;
(defun mapbuildeval (nsform buildperm)
  (let ((ns (new.ns)))
    (do.ns (nform nsform ns)
       (setq ns (union.ns (buildeval nform buildperm) ns)))))
 
;
;
; ==========================================================================
;
; unbindit
; --------
;
;       arguments     : ns - <node set> or <node bind set>
;
;       returns       : <node set>
(defun unbindit (ns)
    (declare (special ns))
    (cond ((is.nbs ns) (nbs-to-ns ns)) (t ns)))
 
(defun topfnname (buildflag)
    (declare (special buildflag))
    (cond (buildflag 'build) (t 'tbuild)))
 
(defun issingleton.rs (rs)
    (declare (special rs))
    (and (atom rs) (not (null rs))))

 
;
;
; additions to buildfns.l by dickhull
; =============================================================================
;
; find-or-build
; -------------
;
;       arguments     : cs - <cable set>
;
;       returns       : <node set>
;
;       description   : Tries to find a node in the network which exactly
;                       matches "cs".  If such a node is found, it is 
;                       returned (as a singleton node set).  Otherwise a
;                       new node with the given cable set is built.
;
;       side-effects  : (see build)
;
;       modifications : Use new function assert-nodes instead of !
;
;                                        written :  rgh  3/19/86
;                                        modified:  rgh  3/22/86
;                                                   ssc  2/02/89
;                                                   njm/hc 5/10/89
;                                                   ssc  5/12/89
;
;
(defun find-or-build (cs
		      &aux
		      (ccs		; canonical cableset
		       (canonicalCS cs))
		      (ns (findexact ccs)))
  (if (isnew.ns ns)
      (build1 ccs)
    ns))

;
; ==========================================================================


(defun canonicalCS (cs)
  "Returns a canonicalized version of the cableset cs."
  (cond
   (;; Negation
    (and
     (ismembrel.cs 'max cs)
     (cl:= (node-to-number.n (choose.ns (nodeset.cs 'max cs))) 0)
     (cl:= (cardinality.ns (nodeset.cs 'arg cs)) 1))
    (canonicalNeg (choose.ns (nodeset.cs 'arg cs))
		  cs))
   (;; and-entaliment with only one antecedent
    ;;   is changed to an or-entailment
    (and
     (ismembrel.cs '&ant cs)
     (cl:= (cardinality.ns (nodeset.cs '&ant cs)) 1)
     ;; but not a numerically-quantified wff
     (not (or (ismembrel.cs 'emin cs)
	      (ismembrel.cs 'emax cs))))
    (setf (car (ismembrel.cs '&ant cs)) 'ant)
    cs)
   (t ;; Default is to return user's version
    cs)))


(defun canonicalNeg (P origCS)
  "Returns a canonical cableset version of the negation cableset, origCS,
     which is the negation of the node P."
  (cond
   ((is-and-or.n P)
    (let* ((args (nodeset.n P 'arg))
	   (min (node-to-number.n (choose.ns (nodeset.n P 'min))))
	   (max (node-to-number.n (choose.ns (nodeset.n P 'max))))
	   (tot (cardinality.ns args)))
      (cond
       ((and (is-not.n P) (cl:= tot 1))
	;; P = ~Q
	;; ~~Q = Q
	(n-to-downcs (choose.ns args)))
       ((zerop min)
	;; ~andor(0,j){P1, ..., Pn}
	;;    = andor(j+1,n){P1, ..., Pn}
	(putin.cs
	 (new.c 'min 
		(makeone.ns (lisp-object-to-node (1+ max))))
	 (putin.cs
	  (new.c 'max
		 (makeone.ns (lisp-object-to-node tot)))
	  (putin.cs (new.c 'arg args)
		    (new.cs)))))
       ((cl:= max tot)
	;; ~andor(i,n){P1,..., Pn}
	;;    = andor(0,i-1){P1, ..., Pn}
	(putin.cs
	 (new.c 'min (makeone.ns (lisp-object-to-node 0)))
	 (putin.cs
	  (new.c 'max 
		 (makeone.ns (lisp-object-to-node (1- min))))
	 (putin.cs (new.c 'arg args)
		   (new.cs)))))
       (t
	;; in general, ~andor(i,j){P1, ..., Pn}
	;;               = thresh(i,j){P1, ..., Pn}
	(putin.cs (new.c 'thresh (nodeset.n P 'min))
		  (putin.cs (new.c 'threshmax (nodeset.n P 'max))
			    (putin.cs (new.c 'arg (nodeset.n P 'arg))
				      (new.cs))))))))
   ((is-thresh.n P)
    ;; in general, ~thresh(i,j){P1, ..., Pn}
    ;;               = andor(i,j){P1, ..., Pn}
    (putin.cs (new.c 'min (nodeset.n P 'thresh))
	      (putin.cs (new.c 'max
			       (or (nodeset.n P 'threshmax)
				   (makeone.ns
				    (lisp-object-to-node
				     (1- (cardinality.ns
					  (nodeset.n P 'arg)))))))
			(putin.cs (new.c 'arg (nodeset.n P 'arg))
				  (new.cs)))))
   (t					; Default case: original cableset.
    origCS)))



    
    




