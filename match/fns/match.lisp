;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: match.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

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




(in-package :match)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MATCH
;;;   with
;;; Unification Algorithm
;;; based on
;;; G. Escalada-Imaz and M. Ghallab,
;;;      A Practically Efficient and Almost Linear Unification Algorithm
;;;      Artificial Intelligence Vol 36, Number 2 (Sept. 1988), 249-263
;;;
;;; Modified for SNePS with UVBR by Stuart C. Shapiro, April, 1989
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro r1 (x)
  "Returns the value associated with x in the assoc list r1."
  ;; by scs 04/19/2011
  `(cdr (assoc ,x r1)))

(defsetf r1 (x) (newval)
  "Updates the assoc list r1 so that x is associated with newval."
  ;; by scs 04/19/2011
  (let ((locvar (gensym)))
    `(let ((,locvar ,newval))
       (cond ((assoc ,x r1)
	      (setf (cdr (assoc ,x r1)) ,locvar))
	     (t (setf r1 (cons (cons ,x ,locvar) r1))
		,locvar)))))

(defmacro r2 (x)
 "Returns the value associated with x in the assoc list r2."
 ;; by scs 04/19/2011
 `(cdr (assoc ,x r2)))

(defsetf r2 (x) (newval)
 "Updates the assoc list r2 so that x is associated with newval."
 ;; by scs 04/19/2011
 (let ((locvar (gensym)))
   `(let ((,locvar ,newval))
      (cond ((assoc ,x r2)
	      (setf (cdr (assoc ,x r2)) ,locvar))
	     (t (setf r2 (cons (cons ,x ,locvar) r2))
		,locvar)))))

(defmacro ir1 (x)
  "To make (ir1 x) a valid form, as in E-I&G"
  `(cdr (assoc ,x ir1)))

(defmacro ir2 (x)
  "To make (ir2 x) a valid form, as in E-I&G"
  `(cdr (assoc ,x ir2)))

(defvar *ssource* (make-hash-table))

(defvar *starget* (make-hash-table))

(defmacro s1 (x)
  "To make (s1 x) a right-hand and left-hand function, as in E-I&G"
  `(gethash ,x s1))

(defmacro s2 (x)
  "To make (s2 x) a right-hand and left-hand function, as in E-I&G"
  `(gethash ,x s2))

(defmacro rulep (trm)
  `(let ((t1 ,trm))
     (or (is-and-or.n t1)
	 (is-&-ent.n t1)
	 (is-v-ent.n t1)
	 (is-thresh.n t1)
	 (is-num-ent.n t1)
	 (is-non-deriv.n t1)
	 (is-num-quant.n t1))))

(defun uvbrtrap (x)
  "Returns T iff x is a list of the form (uvbr z), for some variable z."  
  ;; The binding pair (y . (uvbr z)) will only occur in the source binding, and
  ;; means that y is bound to itself, and z in the target binding is bound to y,
  ;; so no other variable in the target should be bound to y.
  (and (consp x) (eql (car x) 'uvbr)))

(defun seess ()
  "Prints the contents of the global hash table  *ssource* for debugging purposes."
      (maphash #'(lambda (key val) (print (list key val))) *ssource*))

(defun seest ()
  "Prints the contents of the global hash table  *starget* for debugging purposes."
      (maphash #'(lambda (key val) (print (list key val))) *starget*))

(defun here (t1 t2 r1 r2 init1 init2 t2istarget)
  "Homogeneous Equivalence Relation, Based on E-I&G, p 251"
  ;; Kludge to overcome some match problem that I haven't figured
  ;; out yet (hc, Feb.22, 90)
  (unless (and (sneps:node-p t1)
	       (sneps:node-p t2))
    (throw 'unifytrap nil))
  (if (and (eql t1 t2) (or (isbase.n t1) (ismol.n t1)))
      (list (if t2istarget (list r1 r2) (list r2 r1)))
      (cond ((and (isbase.n t1) (isbase.n t2)) (throw 'unifytrap 'nil))	; clash
	    ((isvar.n t1) (var-here t1 t2 r1 r2 init1 init2 t2istarget))
	    ((isvar.n t2) (var-here t2 t1 r2 r1 init2 init1 (not t2istarget)))
	    ((or (isbase.n t1) (isbase.n t2)) (throw 'unifytrap nil))	;clash
	    (t (if t2istarget
		   (pat-here t1 t2 r1 r2 init1 init2)
		   (pat-here t2 t1 r2 r1 init2 init1))))))

(defun pat-here (snode tnode rs rt inits initt)
  ;; snode and tnode are pattern or molecular nodes
  ;; written by scs 5/5/89
  (checkrulecompatability snode tnode)
  (let* ((t-fcs (down.fcs tnode))
	 (s-fcs (downfcs.pbi snode (relationset.fcs t-fcs)))
	 (tnss nil)
	 (snss nil))
    (do.fcs (ri trmi t-fcs)
      (unless (member ri '(min max thresh threshmax emin emax etot))
	(setf tnss (cons trmi tnss) snss (cons (second (member ri s-fcs)) snss))))
    (unifynss tnss snss rt rs initt inits)))

(defun checkrulecompatability (snode tnode)
  "snode and tnode must be non-rule nodes or rule nodes with the same connectives and with
   appropriate parameters such that some instance of snode implies that instance of tnode.
   If so, returns (its value will be ignored), else throws nil to unifytrap."
  ;; written by scs 5/5/89
  (cond ((is-and-or.n snode) (unless (and (is-and-or.n tnode)
					  (let ((ti (node-to-number.n (car (nodeset.n tnode 'min))))
						(tj (node-to-number.n (car (nodeset.n tnode 'max))))
						(tn (sneps:cardinality.ns (nodeset.n tnode 'arg)))
						(si (node-to-number.n (car (nodeset.n snode 'min))))
						(sj (node-to-number.n (car (nodeset.n snode 'max))))
						(sn (sneps:cardinality.ns (nodeset.n snode 'arg))))
					    (and (<= tn sn)
						 (= ti (max (- si (- sn tn)) 0))
						 (= tj sj))))
			       (throw 'unifytrap nil)))
	((is-&-ent.n snode) (unless (and (is-&-ent.n tnode)
					 (= (sneps:cardinality.ns (nodeset.n tnode '&ant))
					    (sneps:cardinality.ns (nodeset.n snode '&ant)))
					 (<= (sneps:cardinality.ns (nodeset.n tnode 'cq))
					     (sneps:cardinality.ns (nodeset.n snode 'cq)))
					 (<= (sneps:cardinality.ns (nodeset.n tnode 'dcq))
					     (sneps:cardinality.ns (nodeset.n snode 'dcq))))
			      (throw 'unifytrap nil)))
	((is-v-ent.n snode) (unless (and (is-v-ent.n tnode)
					 (= (sneps:cardinality.ns (nodeset.n tnode 'ant))
					    (sneps:cardinality.ns (nodeset.n snode 'ant)))
					 (<= (sneps:cardinality.ns (nodeset.n tnode 'cq))
					     (sneps:cardinality.ns (nodeset.n snode 'cq)))
					 (<= (sneps:cardinality.ns (nodeset.n tnode 'dcq))
					     (sneps:cardinality.ns (nodeset.n snode 'dcq))))
			      (throw 'unifytrap nil)))
	((is-thresh.n snode) (unless (and (is-thresh.n tnode)
					  (let ((ti (node-to-number.n (car (nodeset.n tnode 'thresh))))
						(tj (if (nodeset.n tnode 'threshmax)
							(node-to-number.n (sneps:choose.ns (nodeset.n tnode 'threshmax)))
							(1- (cardinality.ns (nodeset.n tnode 'sneps:arg)))))
						(tn (sneps:cardinality.ns (nodeset.n tnode 'arg)))
						(si (node-to-number.n (car (nodeset.n snode 'thresh))))
						(sj (if (nodeset.n snode 'threshmax)
							(node-to-number.n (sneps:choose.ns (nodeset.n snode 'threshmax)))
							(1- (cardinality.ns (nodeset.n snode 'sneps:arg)))))
						(sn (sneps:cardinality.ns (nodeset.n snode 'arg))))
					    (and (<= tn sn)
						 (= ti (max (- si (- sn tn)) 0))
						 (= tj sj))))
			       (throw 'unifytrap nil)))
	((is-num-ent.n snode) (unless (and (is-&-ent.n tnode)
					   (= (sneps:cardinality.ns (nodeset.n tnode '&ant))
					      (sneps:cardinality.ns (nodeset.n snode '&ant)))
					   (>= (node-to-number.n (car (nodeset.n tnode 'thresh)))
					       (node-to-number.n (car (nodeset.n snode 'thresh))))
					   (<= (sneps:cardinality.ns (nodeset.n tnode 'cq))
					       (sneps:cardinality.ns (nodeset.n snode 'cq)))
					   (<= (sneps:cardinality.ns (nodeset.n tnode 'dcq))
					       (sneps:cardinality.ns (nodeset.n snode 'dcq))))
				(throw 'unifytrap nil)))
	((is-non-deriv.n snode) (unless (is-non-deriv.n tnode)
				    (throw 'unifytrap nil)))
	((is-num-quant.n snode) (unless (and (is-num-quant.n tnode)
					     (let ((ti (if (nodeset.n tnode 'emin) 
							   (node-to-number.n (car (nodeset.n tnode 'emin)))
							   0))
						   (tj (if (nodeset.n tnode 'emax)
							   (node-to-number.n (car (nodeset.n tnode 'emax)))
							   0))
						   (tn (sneps:cardinality.ns (nodeset.n tnode 'etot)))
						   (si (if (nodeset.n snode 'emin)
							   (node-to-number.n (car (nodeset.n snode 'emin)))
							   0))
						   (sj (if (nodeset.n snode 'emax)
							   (node-to-number.n (car (nodeset.n snode 'emax)))
							   0))
						   (sn (sneps:cardinality.ns (nodeset.n snode 'etot))))
					       (and (= ti (max (- si (max (- sn tn) 0)) 0))
						    (= tj sj)
						    (= (sneps:cardinality.ns (nodeset.n tnode '&ant))
						       (sneps:cardinality.ns (nodeset.n snode '&ant)))
						    (<= (sneps:cardinality.ns (nodeset.n tnode 'cq))
							(sneps:cardinality.ns (nodeset.n snode 'cq))))))
			       (throw 'unifytrap nil)))
	(t (when (rulep tnode) (throw 'unifytrap nil)))))

(defun unifynss (tnss snss tb sb initt inits)
  "tnss and snss are sequences of node sets, tb is a binding for tnss, sb is a binding for snss,
   initt is an initial binding for tnss, inits is an initial binding for snss.
   Returns a set of binding pairs trying to match every element of each element of snss
   against every element of the corresponding element of tnss"
  ;; written by: scs 5/12/88, modified 4/27/89
  (cond ((null tnss) (list (list sb tb)))
	((null (first tnss))
	 (unifynss (rest tnss) (rest snss) tb sb initt inits))
	(t (unifynss1 (first (first tnss)) tnss snss tb sb initt inits))))

(defun unifynss1 (tn tnss snss tb sb initt inits)
  "tn is a target node, tnss and snss are sequences of node sets,
   tb is a binding for tnss, sb is a binding for snss.
   Returns a set of binding pairs trying to match tn against every element of (first snss),
   and every other element of each element of tnss
   against every (other) element of the corresponding element of snss"
  ;; written by: scs 5/12/88, modified 4/27/89
  (let (bindgsfortn)
    (or
     (do.ns (sn (first snss) bindgsfortn)
	    (setq bindgsfortn
		  (nconc
		    (let (bindings)
		      (dolist (bndg
				(catch 'unifytrap
				  (here tn sn (copy-alist tb) (copy-alist sb) initt inits nil))
				bindings)
			(setq bindings (nconc (catch 'unifytrap
						(unifynss (cons (rest (first tnss)) (rest tnss))
							  (cons (remove sn (first snss)) (rest snss)) 
							  (second bndg)
							  (first bndg)
							  initt
							  inits))
					      bindings))))
		    bindgsfortn)))
     (throw 'unifytrap nil))))

(defun var-here (u term r1 r2 ir1 ir2 t2istarget)
  "Based on E-I&G, p 254"
  (cond ((r1 u)
	 (let ((y (r1 u)))		; term and y both want to bind to u
	   (cond ((eql term y)
		  (list (if t2istarget (list r1 r2) (list r2 r1))))
		 ((isvar.n term)
		  (if (and (uvbrtrap y) (eql (second y) term))
		      (list (if t2istarget (list r1 r2) (list r2 r1)))
		    (throw 'unifytrap nil))) ; uvbr
		 ((isbase.n term) (throw 'unifytrap nil)) ; uvbr
		 (t (setf (r1 u) 'loop)
		    (mapcar #'(lambda (bndg)
				(if t2istarget
				    (let ((r1 (first bndg))) (setf (r1 u) y) (list r1 (second bndg)))
				  (let ((r1 (second bndg))) (setf (r1 u) y) (list (first bndg) r1))))
			    (here y term r1 r2 ir1 ir2 t2istarget))))))
	((and (ir1 u) (not (eql u (ir1 u)))) ; An initial binding for u can also be bound to term, if that is a variable.
	 (cond ((eql term (ir1 u))
		(setf (r1 u) term)
		(list (if t2istarget (list r1 r2) (list r2 r1))))
	       (t (if (isvar.n term)
		      (cond ((and (null (r2 term)) (null (ir2 term)))
			     (setf (r1 u) (ir1 u) (r2 term) (ir1 u))
			     (list (if t2istarget (list r1 r2) (list r2 r1))))
			    (t (let ((y (ir1 u))
				     (z (or (r2 term) (ir2 term))))
				 (cond ((eql y z)
					(setf (r1 u) y (r2 term) z)
					(list (if t2istarget (list r1 r2) (list r2 r1))))
				       (t (setf (r1 u) 'loop (r2 term) z)
					  (mapcar #'(lambda (bndg)
						      (if t2istarget
							  (let ((r1 (first bndg))) (setf (r1 u) y) (list r1 (second bndg)))
							(let ((r1 (second bndg))) (setf (r1 u) y) (list (first bndg) r1))))
						  (here y term r1 r2 ir1 ir2 t2istarget)))))))
		    (let ((y (ir1 u)))
		      (setf (r1 u) 'loop)
		      (mapcar #'(lambda (bndg)
				  (if t2istarget
				      (let ((r1 (first bndg))) (setf (r1 u) y) (list r1 (second bndg)))
				    (let ((r1 (second bndg))) (setf (r1 u) y) (list (first bndg) r1))))
			      (here y term r1 r2 ir1 ir2 t2istarget)))))))
	(t
	 (when (ismemb.ns u (freevars.n term)) (throw 'unifytrap nil)) ; occurs check
	 (when (or (rassoc term r1) (assoc term r2)) ; added 2nd clause 2/23/07 scs
	   (throw 'unifytrap nil))	; uvbr
	 (setf (r1 u) term)
	 (when (isvar.n term)
	   ;; always put the uvbr trap in the source binding
	   (cond (t2istarget
		  (setf (r1 u) (list 'uvbr term))
		  (setf (r2 term) u))
		 (t (setf (r2 term) (list 'uvbr u)))))
	 (list (if t2istarget (list r1 r2) (list r2 r1))))))

(defun vere (v r1 r2 s1 s2)
  "Based on Valid Equivalence Relation, E-I&G, p 255"
  ;; Returns the term that matches v
  (let (z)
    (if (r1 v)
	(if (uvbrtrap (r1 v))
	    (setf z (cadr (r1 v)) (s1 v) v (s2 z) v (r1 v) 'done (r2 z) 'done z v)
	    (if (or (isbase.n (r1 v)) (ismol.n (r1 v)))
		(setf z (r1 v) (s1 v) z (r1 v) 'done)
		(if (ispat.n (r1 v))
		    (let ((y (r1 v)))
		      (setf (r1 v) 'done (s1 v) 'loop
			    z (term-vere y r2 r1 s2 s1) (s1 v) z))
		    (if (s1 v)
			(if (eql (s1 v) 'loop)
			    (throw 'unifytrap nil)	; loop
			    (setf z (s1 v)))
			(setf z (r1 v))))))
	(setf (r1 v) 'done z v (s1 v) z))
    z))

(defun term-vere (term r1 r2 s1 s2)
  "Based on E-I&G, p 256"
  ;; Assume term = (f t1 ... tk)
  ;; Returns the term that matches TERM.
  (let ((ans (new.cs)))
    (do.fcs (ri nsi (down.fcs term) (first (find-or-build ans)))
	    (do.ns (ti nsi)
		   (if (isvar.n ti)
		       (if (eql (r1 ti) 'done)
			   (if (eql (s1 ti) 'loop)
			       (throw 'unifytrap nil) ; loop
			     (setf ans
			       (insert.cs (new.c ri (makeone.ns (s1 ti)))
					  ans)))
			 (if (r1 ti)
			     (setf ans
			       (insert.cs 
				(new.c ri (makeone.ns (vere ti r1 r2 s1 s2)))
				ans))
			   (setf ans 
			     (insert.cs (new.c ri (makeone.ns ti)) ans))))
		     (if (ispat.n ti)
			 (setf ans
			   (insert.cs (new.c ri
					     (makeone.ns
					      (term-vere ti r1 r2 s1 s2)))
				      ans))
		       (setf ans 
			 (insert.cs (new.c ri (makeone.ns ti)) ans))))))))


; =============================================================================
;
;                                          modified :  mrc 10/24/89
;     Modifications:
;     1. Global variables: 
;           supporting-nodes - nodes to be taken into account when
;                              computing the support of a node
;                              inferred by path-based inference.
;           new-nodes        - nodes inferred by path-based
;                              inference and created by unify.
;           top-node         - first argument of unify.
;
;     2. This function now creates every node inferred by path-based
;        inference.
;
(defun unify (source target &optional initsbind inittbind)
  "If there are substitutions, ss and st, such that ss(initsbind(source)) => st(inittbind(target0),
   return a list of most general such pairs (ss.initsbind st.inittbind)."
  ;; Based on E-I&G, p 257
  (let ((supporting-nodes nil)
	(top-node source))
    (declare (special supporting-nodes  new-nodes top-node))
    (setq  new-nodes nil)
    (catch 'unifytrap
      (let (sublist)
	(dolist (rpair (here target source nil nil inittbind initsbind nil) sublist)
	  (clrhash *ssource*)
	  (clrhash *starget*)
	  (dolist (pair (second rpair))
	    (unless (eql (cdr pair) 'done) (vere (car pair) (second rpair) (first rpair) *starget* *ssource*)))
	  (dolist (pair (first rpair))
	    (unless (eql (cdr pair) 'done) (vere (car pair) (first rpair) (second rpair) *ssource* *starget*)))
	  (let (s tr)
	    (maphash #'(lambda (v trm) (unless (isvar.n trm) (setf s (cons (cons v trm) s)))) *ssource*)
	    (maphash #'(lambda (v trm) (setf tr (cons (cons v trm) tr))) *starget*)
	    (setq sublist (cons (list s tr) sublist))
	    (setq new-nodes
		  (cons (list (create-new-node  target tr supporting-nodes) tr)
			new-nodes))))
	sublist))))


; =============================================================================
;
; create-new-node 
; ---------------
;
;       arguments     : target            - <node>
;                     : tr                - (see unify)
;                     : supporting-nodes  - <node-set>
;
;       returns       : <node>
;
;       description   : If the support computed by compute-support is not nil
;                       Returns an node just like target, but with every dominated 
;                       occurrence of a variable in tr replaced by its term (see 
;                       applysubst), with the computed support. Otherwise returns 
;                       top-node (see unify).
;
;                                          written:  mrc 10/24/89
;
(defun create-new-node (target tr supporting-nodes)
  (declare (special top-node))
  (let* ((sup (compute-support tr supporting-nodes))
	 (new-node (if sup (applysubst target tr) top-node)))
    (if sup (snip:addsupport.n sup new-node))
    new-node))


;;; FUNCTIONS FOR MATCH
;;; The revised target/source distinction must be kept in mind
;;; -- a source node may have more wires than a target
;;;    if b is a unifier of sn and tn, then snb => tnb, but not the converse.

; =============================================================================
;
; match-in-context
; ----------------
;
;       arguments     : tnode     -  <node>
;                       ct        -  <context>
;                       initbind  -  <bndg>
;
;       returns       : <supmatching set>
;
;       description   : This function is similar to match but it considers
;                       the context "ct".
;                       Returns a set of supmatchings where each supmatching
;                       consists of:
;                        1. a node that matches the given node
;                        2. a source binding
;                        3. the set of source node supports that are
;                           subsets of <context> "ct".
;                        4. a target binding.
;
;
;                                        written : njm/cpf 10/19/88
;                                                  njm     05/13/89
;
;
(defun match-in-context (tnode ct &optional initbind)
  "Returns a set of supmatchings
   where each supmatching consists of:
     1. a node that matches the given node
     2. a source binding
     3. the set of source node supports that are subsets of <context> "ct".
     4. a target binding."
  (let* ((crntct ct)
	 (matchingset (match tnode initbind))
	 (supmatchingset (new.supmatchingset)))
    (declare (special crntct))
    (do.matchingset (matching matchingset supmatchingset)
      (when (sneps:in-context.ns (makeone.ns (tnode.matching matching)) t)
	(setq supmatchingset
	      (putin.supmatchingset
		(make.supmatching (tnode.matching matching) 
				  (target-sub.matching matching)
				  (sneps:filter.ctcs (sneps:node-asupport (tnode.matching matching))
						     ct) 
				  (source-sub.matching matching))
		supmatchingset))))))


; =============================================================================
;
;
;                                          modified :  mrc 10/24/89
;     Modifications:
;        1. A global variable new-nodes is used. This variable contains
;           the nodes eventually created by the function unify, as a
;           result of path-based inference.
;        2. When a new-node was created by the function unify this new
;           node is returned instead of the node from where the path
;           started.
;
(defun match (tnode &optional initbind)
  "Returns a set of matchings
   where each matching consists of:
     1. a node that matches the given node
     2. a source binding
     3. a target binding."
  ;; written by: scs 5/19/88, modified 4/27/89
  (unless (find-if #'(lambda (n) (sneps:dominates.n tnode n))
		   initbind
		   :key #'cdr)
    (let ((new-nodes (sneps:new.ns))
	  (sub-tnode (applysubst tnode initbind))) ;Added 4/16/99 :- HI
      (declare (special new-nodes))
      (mapcan
       #'(lambda (psn)
	   (mapcan #'(lambda (bndg)
		       (unless (uvbr-conflict initbind (second bndg))
			 (let ((tb (append (second bndg)
					   (substituteInTerms initbind
							      (second bndg)))))
			   (unless (violates-uvbr tb)
			     (list (list (if (null new-nodes)
					     psn
					   (get-new-node (second bndg)))
					 (first bndg)
					 tb))))))
		   (unify psn sub-tnode)))
       (potential-source-nodes tnode)))))

(defun uvbr-conflict (sub1 sub2)
  "Returns T if pairs in the two substitutions have different
   variables, but eql terms;
   else NIL."
  (do* ((rsub sub1 (rest rsub))
	(v1 (caar rsub) (caar rsub))
	(t1 (cdar rsub) (cdar rsub))
	(v2 (car (rassoc t1 sub2)) (car (rassoc t1 sub2))))
      ((null rsub) nil)
    (when (and v2 (not (eql v1 v2))) (return t))))

(defun violates-uvbr (sub)
  "Returns T if two different terms of the substitution SUB are eql;
   else NIL."
  (do ((v (cdar sub) (cdar rsub))
       (rsub (rest sub) (rest rsub)))
      ((null rsub) nil)
    (when (rassoc v rsub) (return t))))

(defun get-new-node (tr)
  (declare (special new-nodes))
  (do ((n new-nodes (cdr n)))
      ((equal tr (cadar n)) (caar n))))


; =============================================================================
;
; forward-match-in-context
; ------------------------
;
;       arguments     : snode     -  <node>
;                       ct        -  <context>
;                       initbind  -  <bndg>
;
;       returns       : <supmatching set>
;
;       description   : This function is similar to forward-match but it considers
;                       the context "ct".
;                       Returns a set of supmatchins where each supmatching
;                       consists of:
;                        1. a node that matches the given node
;                        2. a source binding
;                        3. the set of source node supports that are
;                           subsets of <context> "ct".
;                        4. a target binding.
;
;
;                                        written : njm 10/23/88 
;
;
(defun forward-match-in-context (snode ct &optional initbind)
  "Returns a set of supmatchings
   where each supmatching consists of:
     1. a node that matches the given node
     2. a source binding
     3. the set of target node supports that are subsets of <context> "ct".
     4. a target binding."
  (let* ((crntct ct)
	 (matchingset (forward-match snode initbind))
	 (supmatchingset (new.supmatchingset)))
    (declare (special crntct))
    (do.matchingset (matching matchingset supmatchingset)
      (when (sneps:in-context.ns (makeone.ns (tnode.matching matching)) t)
	(setq supmatchingset
	      (putin.supmatchingset
		(make.supmatching (tnode.matching matching) 
				  (target-sub.matching matching)
				  (sneps:filter.ctcs (sneps:node-asupport (tnode.matching matching))
						     ct) 
				  (source-sub.matching matching))
		supmatchingset))))))

;
; =============================================================================
;


(defun forward-match (snode &optional initbind)
  "Returns a set of matchings,
   where each matching consists of:
     1. a node that matches the given node
     2. a source binding
     3. a target binding."
  ;; written by: scs 5/19/88, modified 4/28/89
  (let ((sub-node (applysubst snode initbind)))
    (mapcan #'(lambda (ptn)
		(mapcar #'(lambda (bndg)
			    (list ptn
				  (append (first bndg)
					  (substituteInTerms initbind
							     (first bndg)))
				  (second bndg)))
			(unify sub-node ptn)))
	    (potential-target-nodes snode))))

; =============================================================================


;;;(defun potential-source-nodes (tnode)
;;;  "Returns a list of nodes that might be matching source nodes of the given node."
;;;  ;; written by: scs 5/12/88
;;;  (case (node-type tnode)
;;;    (:base (insert.ns tnode (value.sv 'sneps::varnodes)))
;;;    (:var (value.sv 'sneps::nodes))
;;;    (otherwise (remove-if #'(lambda (n) (eq (node-type n) :base)) (value.sv 'sneps::nodes)))))

(defun potential-source-nodes (tnode)
  "Returns a list of nodes
        that might be matching source nodes of the given node."
  ;; written by: scs 5/12/88
  ;; modified by: scs 6/27/08
  (case (node-type tnode)
    (:base (insert.ns tnode (value.sv 'sneps::varnodes)))
    (:var (value.sv 'sneps::nodes))
    (otherwise
     (remove-if #'(lambda (n) (not-potential-source-node n tnode))
		(value.sv 'sneps::nodes)))))

(defun not-potential-source-node (n tnode)
  "Returns t if the node n is clearly
        not a potential source node for tnode."
  (and (not (eq (node-type n) :var))
       (or
	;; Don't need tnode itself
	(eq n tnode)
	;; n is a base node
	(eq (node-type n) :base)
	;; There is not some rel in tnode's caseframe
	;;   which goes to a non-empty nodeset in n.
	(not
	 ;; Return t as soon as
	 ;;    find an altrelation of
	 ;;    a relation emanating from tnode
	 ;;    also emanating from n;
	 ;; Return nil if never do find one.
	 (loop for rel in (sneps::down.fcs tnode) by #'cddr
	     when 
	       (and (not (sneps::isup.r rel))
		    (loop for altrel in (or (get rel :altrels) 
					    (list rel))
			when (nodeset.n n altrel)
			do (return t)
			finally (return nil)))
	     do (return t)
	     finally (return nil))))))

(defun potential-target-nodes (snode)
  "Returns a list of nodes that might be matching target nodes of the given node."
  ;; written by: scs 5/12/88
  (case (node-type snode)
    (:base (insert.ns snode (value.sv 'sneps::varnodes)))
    (otherwise (remove-if #'(lambda (n) (eq (node-type n) :base)) (value.sv 'sneps::nodes)))))

;;; FUNCTIONS FOR BINDINGS
;;; A binding is a list of pairs (... (var . term) ...)
;;; indication that the variable var is bound to the term term.
;;; Sometimes bindings are called substitutions,
;;; when they are thought of as sets {... term/var ...}

(defun initbind (pat)
  "Returns an binding (... (vari . vari) ...)
   containing all free variables, vari, in the pattern node pat."
  (mapcar #'(lambda (v) (cons v v))
	  (freevars.n pat)))

(defmacro bboundp (v b)
  "Returns T if the variable v is a variable of the binding b.
     (I.e. if v is bound in b.)
   Otherwise, returns NIL."
  ;; written by: scs 5/12/88
  `(assoc ,v ,b))

(defmacro bindingOf (v b)
  "Returns the term paired with the variable v in the binding b.
   In other words, assuming v is a variable, returns vb.
   In still other words, returns the binding of the variable v in b."
  ;; written by: scs 5/12/88
  `(cdr (assoc ,v ,b)))

(defun restrict-binding-to-pat (bndg pat)
  "Returns a binding containing only those pairs of bndg
   whose variables are free variables in the pattern node pat."
  ;; written by: scs 6/20/88
  (remove-if-not #'(lambda (pair) (ismemb.ns (car pair) (freevars.n pat))) bndg))

;;;
;;; FUNCTIONS FOR SUBSTITUTIONS
;;; 

(defun applysubst (pnode sbst)
  "Returns an node just like pnode,
   but with every dominated occurrence of a variable in sbst
   replaced by its term."
  ;; written by: scs 5/12/88
  (case (node-type pnode)
    ((:base :mol) pnode)			; pnode is a constant node
    (:var (or (bindingOf pnode sbst) pnode))
    (:pat (cond ((bindingOf pnode sbst))
		(t (clrhash *ssource*)
		   (clrhash *starget*)
		   (term-vere pnode (copy-alist sbst) nil *ssource* *starget*))))
    (otherwise (break "~&UNRECOGNIZED TYPE OF NODE ~A FROM INSTANTIATE~%" pnode))))

(defun substituteInTerms (b1 b2)
  "Returns a copy of the substitution b1
   with every term t replaced by tb2."
  ;; written by: scs 5/12/88
  (mapcar #'(lambda (pair)
	      (cons (car pair) (applysubst (cdr pair) b2)))
	  b1))



    
    




