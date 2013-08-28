;;; ----------------------------------------------------
;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: nodeset.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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


;;; =============================================================================
;;; Data Type:  <node set> : (<node> <node> ... <node>)  
;;; =============================================================================
;;;
;;; Implementation Note: A <node set> is implemented as an ordered list of <node>s
;;;                      using isless.n as the ordering function.
;;;                      A hash-table is used for union.ns,intersect.ns, etc.
;;; 
;;; =============================================================================
;;;
;;; new.ns 
;;; ------
;;;
;;;       returns       : <node set>
;;;
;;;       description   : creates a <newnodeset>
;;;
;;;                                        written:  rmo 07/28/83
;;;                                        translated: robert quinn 02/18/86
;;;                                        modified: scs 06/06/87
;;;
;;;
(defmacro new.ns ()
  "Returns a new, empty node set."
  nil)
;;;
;;;
;;; =============================================================================
;;;
;;; isnew.ns 
;;; --------
;;;
;;;       arguments     : ns - <node set>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : returns "true" if "ns" is a new <node set>,
;;;                       "false" otherwise.
;;;
;;;                                        written:  rmo 07/28/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;
;;;
(defmacro isnew.ns (ns)
  "Returns T if NS is a new, empty node set, NIL otherwise."
   `(null ,ns))
;;;
;;;
;;; =============================================================================
;;;
;;; is.ns
;;; -----
;;;
;;;       arguments     : u - <universal>
;;;
;;;       returns       : <boolean> 
;;;
;;;       description   : returns "true" if "u" is a <node set>, "false" otherwise
;;;
;;;       implementation: it does not check if all the elements of "u" are <node>s
;;;                       it checks just the first one.
;;;
;;;                                        written : rmo 07/28/83 
;;;                                        modified: ejm 08/30/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;
;;;
(defun is.ns (u)
  "Returns T if U is a node set, NIL otherwise.  Only checks first member."
  (and (listp u)
       (or (isnew.ns u)
	   (is.n (first u)))))
;;;
;;;
;;; =============================================================================
;;;
;;; isall.ns
;;; --------
;;;
;;;       arguments     : u - <universal>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : It returns "true" if "u" is a <node set>,
;;;                       "false" otherwise.
;;;
;;;       implementation: Unlike "is.ns", "isall.ns" tests whether all the 
;;;                       elements of "u" are <node>s.
;;;
;;;                                        written :  ejm 06/22/84
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 07/08/04
;;;
;;;
(defun isall.ns (u)
  "Returns T if U is a node set, NIL otherwise. Checks everything."
  (and (listp u) 
       (every #'is.n u)))
;;;
;;;
;;; =============================================================================
;;;
;;; makeone.ns
;;; ----------
;;;
;;;       arguments     : n - <node>
;;;
;;;       returns       : <node set>
;;;
;;;       description   : returns a <node set> composed of the <node>.
;;;
;;;       note          : This function should be used whenever one wants to
;;;                       create a <node set> with just one <node>.
;;;                       Don't use the alternative forms
;;;
;;;                                 (make.ns n)
;;;                       or
;;;
;;;                                 (insert.ns n (new.ns))
;;;
;;;                       in order to improve efficiency.
;;;
;;;                                        written :  ejm 10/20/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;
;;;
(defmacro makeone.ns (n)
  "Returns a node set containing just the node N."
  `(list ,n))
;;;
;;;
;;; =============================================================================
;;;
;;; insert.ns 
;;; ---------
;;;
;;;       arguments     : n - <node>
;;;                       ns - <node set>
;;;
;;;       returns       : <node set> 
;;;
;;;       description   : returns a <node set> identical to "ns" but with "n" as
;;;                       a new <node> if "n" was not yet in "ns". 
;;;                       If "n" was in "ns" it just returns "ns" unchanged.
;;;
;;;                                        written:  rmo 07/28/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                                  scs 12/23/88
;;;                                                  ssc 03/15/89
;;;                                                  ssc  5/10/89
;;;                                                  aec 10/05/95
;;;                                                  flj 06/20/02
;;;                                                  scs 07/08/04

(defun insert.ns (n ns)
  "Returns a node set like NS, but with N inserted."
  (let ((tl (member n ns
                    :test #'(lambda (x y)
                              (or (eq x y) (isless.n x y))))))
    (cond ((null tl) (append ns (list n)))
          ((eq n (first tl)) ns)
          (t (nconc (ldiff ns tl) (list n) tl)))))
;;;
;;;
;;; =============================================================================
;;;
;;; make.ns
;;; -------
;;;
;;;       arguments     : n ... n - <node>s
;;;
;;;       returns       : <node set>
;;;
;;;       description   : returns a <node set> composed of the <node>s passed as
;;;                       arguments.
;;;
;;;                                        written :  ejm 09/19/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 12/23/88
;;;
;;;
(defmacro make.ns (&rest n)
  "Returns a node set composed of the argument nodes."
  (cond ((null n) (new.ns))
	(t `(insert.ns ,(first n) (make.ns ,@(rest n))))))
;;;
;;;
;;; =============================================================================

(let ;;One hash table for all the functions that need it.
    ((ht (make-hash-table :test #'eq)))
;;;
;;; union.ns 
;;; --------
;;;
;;;       arguments     : ns1 - <node set>
;;;                       ns2 - <node set>
;;;
;;;       returns       : <node set>
;;;
;;;       description   : returns the union of "ns1" and "ns2"
;;;
;;;                                        written:  rmo 07/28/63
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 06/28/04
;;;
;;;

  ;; Often called with the second argument being an empty nodeset,
  ;;    or a singleton.
  ;; This version is faster in those cases.

  (defun union.ns (ns1 ns2)
    "Returns the union of the two node sets, NS1 and NS2."
    (cond
     ;; Assumes the list representation of nodesets
     ((null ns2) ns1)
     ((null ns1) ns2)
     ((null (rest ns2)) (insert.ns (first ns2) ns1))
     ((null (rest ns1)) (insert.ns (first ns1) ns2))
     (t (clrhash ht)
	(loop for n in ns1
	    do (setf (gethash n ht) t))
	(loop with newns = ns1
	    for n in ns2
	    unless (gethash n ht)
	    do (setf newns (insert.ns n newns))
	    finally (return newns)))))

;;;
;;;
;;; =============================================================================
;;;
;;; intersect.ns   
;;; ------------
;;;
;;;       arguments     : ns1 - <node set>
;;;                       ns2 - <node set>
;;;
;;;       returns       : <node set>
;;;
;;;       description   : returns the intersection of "ns1" and "ns2"
;;;
;;;
;;;                                        written:  rmo 08/15/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 06/28/04
;;;
;;;
  (defun intersect.ns (ns1 ns2)
    "Returns the intersection of the node sets NS1 and NS2."
    (cond
     ((isnew.ns ns1) ns1)
     ((isnew.ns ns2) ns2)
     (t (clrhash ht)
	(loop for n in ns1
	    do (setf (gethash n ht) t))
	(loop for n in ns2
	    when (gethash n ht)
	    collect n))))

;;;
;;;
;;; =============================================================================
;;;
;;; compl.ns 
;;; -------
;;;
;;;       arguments     : ns1 - <node set>
;;;                       ns2 - <node set> 
;;;
;;;       returns       : <node set>
;;;
;;;       description   : returns the set difference ns1 - ns2. 
;;;
;;;                                        written:  rmo 07/28/83
;;;                                        modified: ejm 08/19/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 06/28/04
;;;
;;;
  (defun compl.ns (ns1 ns2)
    "Returns the relative complement of node set NS1 and node set NS2."
    (cond
     ((or (isnew.ns ns1) (isnew.ns ns2)) ns1)
     (t (clrhash ht)
	(loop for n in ns2
	    do (setf (gethash n ht) t))
	(loop for n in ns1
	    unless (gethash n ht)
	    collect n))))

;;;
;;;
;;; =============================================================================
;;;
;;; issubset.ns
;;; -----------
;;;
;;;       arguments     : ns1 - <node set>
;;;                       ns2 - <node set>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : returns "true" if "ns1" is a subset of "ns2", 
;;;                       "false" otherwise.
;;;
;;;                                        written:  rmo 07/28/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 07/08/04
;;;
;;;
  ;; Often called with the first argument being a singleton.
  ;; This version is faster in those cases.
  (defun issubset.ns (ns1 ns2)
    "Returns T if node set NS1 is a subset of node set NS2; NIL otherwise."
    (cond
     ;; Assumes the list representation of nodesets
     ((null ns1))
     ((null ns2) nil)
     ((null (rest ns1))
      (member (first ns1) ns2 :test #'eq))
     (t (clrhash ht)
	(loop for n in ns2
	    do (setf (gethash n ht) t))
	(loop for n in ns1
	    always (gethash n ht)))))
  )


;;;
;;;
;;; =============================================================================
;;;
;;; iseq.ns 
;;; -------
;;;
;;;       arguments     : ns1 - <node set>
;;;                       ns2 - <node set>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : Compares "ns1" and "ns2" as sets : returns "true" if
;;;                       "ns1" and "ns2" have exactly the same elements,
;;;                       "false" otherwise.
;;;
;;;                                        written:  rmo 07/28/83 
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 06/28/04
;;;                                                  changed from macro to function
;;;                                        modified: scs 07/08/04
;;;
;;;
(defun iseq.ns (ns1 ns2
		;; &aux (ht (make-hash-table :test #'eq))
		)
  "Returns T if node sets NS1 and NS2 are equal; NIL otherwise."
  (equal ns1 ns2))
;;;
;;;
;;; =============================================================================
;;;
;;; describe.ns
;;; ------------
;;;
;;;       arguments     : ns - <node set>
;;;
;;;       returns       : <sequence>
;;; 
;;;       description   : It returns a <sequence> which is a description of 
;;;                       the <node set> "ns" to be printed.
;;;
;;;                                        written :  ejm 06/05/84
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;
(defmacro describe.ns (ns)
  "Returns a sequence which is a printable version of the node set NS."
  ns)
;;;
;;;
;;; =============================================================================
;;;
;;; is.nas
;;; ------
;;;
;;;       arguments     : u - <universal>
;;;
;;;       returns       : <boolean>
;;;
;;;       description   : It returns "true" if "u" is a set of <node-access>es,
;;;                       "false" otherwise.
;;;
;;;                                        written :  ejm 06/19/84
;;;                                        modified:
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 06/28/04
;;;
;;;
(defun is.nas (u)
  "Returns T if U is a node-access set."
  (every #'(lambda (obj) 
	     (or (is.n (node obj))
		 (and (numberp obj) (node (un-ize obj)))))
	 u))
;;;
;;;
;;; =============================================================================
;;;
;;; apply-subst.ns
;;; --------------
;;;
;;;       arguments     : subst - <substitution>
;;;                       nodeset - <node set>
;;;
;;;       returns       : <node set>
;;;
;;;       description   : applies the <substitution> "subst" to the <node set>
;;;                       "nodeset" and returns the result
;;;
;;;                                        written :  rgh  8/21/85
;;;                                        modified:  rgh  4/11/86
;;;                                        modified:  scs  6/16/88
;;;                                        modified:  scs  7/09/04
;;;
;;;
(defun apply-subst.ns (subst nodeset)
  (reduce #'(lambda (resultns n)
	      (insert.ns (match::applysubst n subst) resultns))
	  nodeset
	  :initial-value (new.ns)))
;;;
;;;
;;; =============================================================================
;;;
;;; cardinality.ns
;;; --------------
;;;
;;;       arguments     : ns - <node set>
;;;
;;;       returns       : <non-negative integer>
;;;
;;;       description   : returns the number of <node>s in "ns"
;;;
;;;                                        written :  rgh  4/06/86
;;;                                        modified:
;;;
;;;
(defmacro cardinality.ns (ns)
  `(length ,ns))
;;;
;;;
;;; =============================================================================

(defmacro remove-if-not.ns (test ns)
  "Returns a <node set> just like ns, but with all <nodes> failing the test removed."
  `(remove-if-not ,test ,ns))
;;;
;;;
;;; =============================================================================
;;;
;;; remove.ns 
;;; ---------
;;;
;;;       arguments     : n - <node>
;;;                       ns - <node set>
;;;
;;;       returns       : <node set> 
;;;
;;;       description   : returns a <node set> identical to "ns" but with "n"
;;;                       removed if it was there. If "n" was not in "ns"
;;;                       it just returns "ns" unchanged.
;;;
;;;                                        written:  ejm 08/15/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                        modified: scs 06/28/04
;;;
;;;
(defun remove.ns (n ns)
  "Returns a node set like NS, but with the node N absent."
  (remove n ns :test #'equal :count 1))
;;;
;;;
;;; =============================================================================
;;;
;;; choose.ns 
;;; ---------
;;;
;;;       arguments     : ns - <node set>
;;;
;;;       returns       : <node> 
;;;
;;;       description   : returns a <node> of "ns"
;;;
;;;                                        written:  rmo 07/28/63
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;
;;;
(defun choose.ns (ns)
  "Returns an arbitrary, but deterministic member of the node set NS."
  (first ns))
;;;
;;;
;;; =============================================================================
;;;
;;; others.ns 
;;; ---------
;;;
;;;       arguments     : ns - <node set>
;;;
;;;       returns       : <node set>
;;;
;;;       description   : returns a <node set> identical to "ns" but without
;;;                       the element that would be chosen by choose.ns
;;;
;;;                                        written:  rmo 07/28/63
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;
;;;
(defun others.ns (ns)
  "Returns a node set like NS, but with (CHOOSE.NS NS) deleted."
   (rest ns))
;;;
;;;
;;; =============================================================================

(defmacro do.ns ((var nsform &optional resultform) &body forms)
  `(dolist (,var ,nsform ,resultform) ,@forms))

;;; =============================================================================
;;;
;;; ismemb.ns    
;;; ---------
;;;
;;;       arguments     : n - <node>
;;;                       ns - <node set> 
;;;
;;;       returns       : <boolean> 
;;;
;;;       description   : returns "true" if "n"  is an element of "ns",
;;;                       "false" otherwise.
;;;
;;;                                        written:  rmo 07/28/83
;;;                                        translated: rmq 02/18/86
;;;                                        modified: scs 06/06/87
;;;                                                  aec 10/05/95
;;;                                                  scs 07/08/04

(defun ismemb.ns (n ns)
  "Returns T if node N is in the node set NS."
  (let ((tl (member n ns
                    :test #'(lambda (x y)
                              (or (eq x y) (isless.n x y))))))
    (and (consp tl)
	 (eq n (first tl)))))
;;;
;;;
;;; =============================================================================
