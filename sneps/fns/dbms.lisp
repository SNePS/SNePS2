;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1994--2013
;; Research Foundation of State University of New York

;; Version: $Id: dbms.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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


(defsnepscom dbproject ((nodesetexp &rest rels))
  "A virtual relation (a set of flat cable sets) is created and
returned.  The virtual relation is formed by taking the nodes
returned by the SNePSUL node set expression, NODESETEXP, and
projecting down the SNePSUL relations included in the sequence,
RELATIONS."
  (let ((nodeset (nseval nodesetexp)))
    (delete-duplicates
     (mapcar #'(lambda (nde)
		 (mapcan #'(lambda (rel)
			     (list rel
				   #!((find
				       (converse ~(checkpath rel))
				       ~nde))))
			 rels))
	     nodeset)
     :from-end t :test #'equal)))

(defsnepscom dbjoin ((jrel ns1 rels1 ns2 rels2))
  "A virtual relation (a set of flat cable sets) is created and
returned.  The virtual relation is formed by taking the nodes
returned by the SNePSUL node set expression, NODESETEXP1 and the
nodes returned by the SNePSUL node set expression, NODESETEXP2,
joining these two relations on the attribute RELATION, and then
projecting the result down the RELATIONS1 attributes from the first
nodeset and the RELATIONS2 attributes from the second nodeset.
Note that RELATIONS1 and RELATIONS2 is each a list of relations."
  (mapcar #'(lambda (nb)
	      (let ((nde1
		     (choose.ns
		      (nodeset.bs 'jvar1 (bindset.nb nb))))
		    (nde2
		     (choose.ns
		      (nodeset.bs 'jvar2 (bindset.nb nb)))))
		(nconc
		 (mapcan #'(lambda (rel)
			     (list rel #!((find (converse ~rel) ~nde1))))
			 rels1)
		 (mapcan #'(lambda (rel)
			     (list rel #!((find (converse ~rel) ~nde2))))
			 rels2))))
	  (find1
	   (fevalsnd
	    `((compose !) ,ns1 (compose !) (? jvar1)
			  (compose ,jrel (converse ,jrel))
			  (find (compose !) ,ns2
				(compose !) (? jvar2)))))))

(defsnepscom dbassertvirtual ((virtualdbexp &optional initialfields) assert)
  "Evaluates VIRTUALEXP, which must return a virtual relation (set of
flat cable sets), appends the list ([RELATION NODESET]*) to each
flat cable set, asserts each resulting flat cable set as a SNePS
molecular node, and returns the set of asserted nodes."
  (mapcan #'(lambda (spec)
	      #!((assert ~@initialfields ~@spec)))
	  #!(~virtualdbexp)))

(defsnepscom dbmin ((ns) (top ns bns fns))
  "Evaluates the SNePSUL nodeset expression, NS, which must evaluate
to a set of nodes all of whose identifiers look like numbers, and
returns the node whose identifier looks like the smallest of the
numbers."
  (makeone.ns (apply-function-to-ns #'cl:min ns)))

(defsnepscom dbmax ((ns) dbmin)
  "Evaluates the SNePSUL nodeset expression, NS, which must evaluate
to a set of nodes all of whose identifiers look like numbers, and
returns the node whose identifier looks like the biggest of the
numbers."
  (makeone.ns (apply-function-to-ns #'cl:max ns)))

(defsnepscom dbtot ((ns) dbmin)
  "Evaluates the SNePSUL nodeset expression, NS, which must evaluate
to a set of nodes all of whose identifiers look like numbers, and
returns a node whose identifier looks like the sum of the
numbers."
  (makeone.ns (apply-function-to-ns #'cl:+ ns)))

(defsnepscom dbcount ((ns) dbmin)
  "Evaluates the SNePSUL nodeset expression, NS, and returns a node
whose identifier looks like the number which is the number of nodes
in the resulting set."
  (lisp-list-to-ns (list (length (nseval ns)))))



    
    




