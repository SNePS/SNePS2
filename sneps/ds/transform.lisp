;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: transform.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


;; Altered for ACL 6 by FLJ

(in-package :sneps)


; =============================================================================
; Data Type Transformers
; =============================================================================
;
;
; =============================================================================
;
; nbs-to-ns 
; ---------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node set>
;
;       description   : Takes <node bind set> and returns <node set>. 
;
;       side effect   : it will put <node bind> of each <node> under
;                       the property :val of this <node>.    
;
;                                        written:  rmo 08/05/83
;                                        modified: ejm 10/11/83, rmr 07/27/84,
;                                                  ejm 08/02/84
;                                                  ssc  5/10/89
;
;
(defun nbs-to-ns (nbs)
  ;; Assumes that both nodesets and bound nodesets are represented by lists.
  (reset-vars nbs) 
  (mapc #'(lambda (nb)
	    (mapc #'(lambda (b)
		      (mapc #'(lambda (n) 
				(set.sv (var.b b)
					(insert.ns n 
						   (value.sv (var.b b)))))
			    (nodeset.b b)))
		  (bindset.nb nb)))
	nbs)
  (mapcar #'node.nb nbs))
;
;
; =============================================================================
;
; reset-vars
; ----------
;
;       arguments     : nbs - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : Takes <node bind set> and returns it with variables
;                       reset.  
;
;                                        written:  rmo 08/05/83
;                                        modified:                  
;
;
;
;(declare (localf reset-vars))
;(de reset-vars  (nbs)
;          (mapc
;            '(lambda (nb)
;                     (mapc '(lambda (b)
;                              (set.sv (var.b b) (new.ns)))
;                            (bindset.nb nb)) 
;                     (node.nb nb))
;             nbs))
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defun reset-vars  (nbs)
          (declare (special nbs))
          (mapc
            #'(lambda (nb)
                     (mapc #'(lambda (b)
                              (set.sv (var.b b) (new.ns)))
                            (bindset.nb nb)) 
                     (node.nb nb))
             nbs))
;
;
; =============================================================================
;
; ns-to-nbs 
; ---------
;
;       arguments     : ns - <node set>
;
;       returns       : <node bind set>
;
;       description   : takes a <node set> and returns a <node bind set> 
;                   
;
;                                        written:  rmo 08/10/83
;                                        modified:
;
;
(defun ns-to-nbs (ns)
  (mapcar #'(lambda (n)
	      (new.nb n (new.bs)))
	  ns))
;
;
; =============================================================================
;
; cs-to-fcs
; ----------
;
;        arguments     : cs - <cable set>
;
;        returns       : <flat cable set>
;
;        description   : It transforms the <cable set> "cs" into a 
;                        <flat cable set>. 
;
;                                        written:  ejm 07/26/83
;                                        modified: ejm 09/27/83
;                                        modified: scs 05/10/88
;
;
(defun cs-to-fcs (cs)
  (let ((fcs (new.fcs)))
    (do.cs (c cs fcs)
       (setq fcs (insert.fcs (relation.c c) (nodeset.c c) fcs)))))
;
;
; =============================================================================
;
; fcs-to-cs
; ---------
;
;        arguments     : fcs - <flat cable set>
;
;        returns       : <cable set>
;
;        description   : It transforms the <flat cable set> "fcs" which is the
;                        property list of the <node> "n" (see n-to-cs)
;                        into the corresponding <cable set>.
;
;                                        written : ejm 07/26/83 
;                                        modified: ejm 07/26/83 
;
;
(defun fcs-to-cs (fcs)
   (cond ((isnew.fcs fcs) (new.cs))
         (t (putin.cs (new.c (relation.fcs fcs) (nodeset.fcs fcs))
                      (fcs-to-cs (others.fcs fcs))))))
;
;
;
; =============================================================================
; cs-to-n
; -------
;
;        arguments     : n  - <node>
;                        cs - <cable set>
;
;        returns       : <node>
;
;        description   : It transforms the <cable set> "cs" into the <node>
;                        "n".
;
;        side-effects   : It side effects the <node> "n".
;
;                                        written:  ejm 07/26/83
;                                        modified: scs 02/20/87
;
;
(defun cs-to-n (n cs)
       (setf (node-fcableset n) (cs-to-fcs cs))
	n)
;
; =============================================================================
; fcs-to-n
; --------
;
;        arguments     : n  - <node>
;                        cs - <flat cable set>
;
;        returns       : <node>
;
;        description   : It transforms the <flat cable set> "fcs" into the <node>
;                        "n".
;
;        side-effects   : It side effects the <node> "n".
;
;                                        written:  scs 02/23/87
;                                        modified: 
;
;
(defun fcs-to-n (n fcs)
  "Returns the node n
     with fcableset fcs installed,
     and height set to
     1 more than the height
     of its highest immediately dominated node."
  (setf (node-fcableset n) fcs
	(node-height n) (calcheight n))
  n)

(defun calcheight (n)
  "Returns the height of node n,
       calculated from the heights of its immediately dominated nodes."
  (let ((h 0))
    (1+ (do.fcs (r ns (down.fcs n) h)
		(do.ns (n ns)
		       (setf h (max h (node-height n))))))))
;
; =============================================================================
;
; n-to-cs
; -------
;
;        arguments     : n - <node>
;
;        returns       : <cable set>
;
;        description   : It transforms the <node> "n" into the corresponding
;                        <cable set>.
;
;                                        written:  ejm 07/26/83
;                                        modified: scs 02/20/87
;                                        modified: scs 02/11/88
;
;
(defmacro n-to-cs (n)
  `(fcs-to-cs (node-fcableset ,n)))
;
;
; =============================================================================
;
; n-to-downcs
; -----------
;
;        arguments     : n - <node>
;
;        returns       : <down cable set>
;
;        description   : It transforms the <node> "n" into the corresponding
;                        <down cable set>. The <down cable set> is the subset
;                        of a <cable set> consisting of only those <cable>s
;                        with <descending relations>.
;
;                                        written:  vhs 07/27/84
;                                        modified: scs 02/11/88
;
;
(defun n-to-downcs (n)
    (down.cs (n-to-cs n)))
;
;
; =============================================================================
;
; nas-to-ns
; ---------
;
;       arguments     : <nodeaccessset>
;
;       returns       : <node set>
;
;       description   : transforms an <nodeaccessset> into a <node set>,
;                       i.e., it transforms a set of <node access>es into
;                       a set of corresponding <node>s.
;
;                                        written :  ejm 06/07/84
;                                        modified:  scs 06/08/87
;
;
(defun nas-to-ns (nas)
  "Returns a node set containing the nodes whose node accesses are in the nodeaddessset NAS."
  (cond ((null nas) (new.ns))
	((numberp (first nas))
	 (insert.ns (node (un-ize (first nas))) (nas-to-ns (rest nas))))
	(t (insert.ns (node (first nas)) (nas-to-ns (rest nas))))))
;
;
; =============================================================================
;
; string-to-symbol
; ----------------
;
;       arguments     : <string>
;
;       returns       : <symbol>
;
;       description   : transforms a <string> into a <symbol>.
;
;                                        written :  scs 06/08/87
;                                        modified:
;

(defun string-to-symbol (s)
  "Returns s symbol whose print name is the string S."
  (read-from-string (concatenate 'string "|" s "|")))

;
; =============================================================================
;
;
; =============================================================================
;
; seq-to-set 
; ----------
;
;       arguments     : seq - <sequence>
;
;       returns       : <set> 
;
;       description   : takes <sequence> and returns <set> 
;
;                                        written :  rmr 07/02/84 
;                                        modified:
;
;
;(declare (localf seq-to-set))
(defun seq-to-set (seq)
       (declare (special seq))
       seq)
;
;
; ============================================================================
;
; queue-to-seq 
; ------------
;
;       arguments     : q - <queue> 
;
;       returns       : <sequence> 
;
;       description   : takes <queue> and returns <sequence> 
;
;                                        written :  rmr 07/02/84 
;                                        modified:
;
;
;(declare (localf queue-to-seq))
(defun queue-to-seq (q)
         (declare (special q))
         (cdar q))
;
;
; ============================================================================
;
; queue-to-set 
; ------------
;
;       arguments     : q - <queue> 
;
;       returns       : <set> 
;
;       description   : takes <queue> and returns <set> 
;
;                                        written :  rmr 07/02/84 
;                                        modified:
;
;
;(declare (localf queue-to-set))
(defun queue-to-set (q)
         (declare (special q))
         (cdar q))
;
;
; ==============================================================================
;
; queue-to-bs 
; -----------
;
;       arguments     : q - <queue> 
;
;       returns       : <bindset> 
;
;       description   : tranforms a <queue> into a <bindset>
;
;                                        written :  rmr 07/19/84 
;                                        modified:
;
;
(defmacro queue-to-bs (q)
   `(cdar ,q))
;
;
; =============================================================================
;
; queue-to-nbs   
; ------------
;
;       arguments     : q - <queue> 
;
;       returns       : <nodebindset> 
;
;       description   : transforms a <queue> into a <nodebindset> 
;
;                                        written :  rmr 07/20/84 
;                                        modified:
;
;
(defmacro queue-to-nbs (q)
   `(cdar ,q))
;
;
; =============================================================================
;
; ctcs-to-cts    
; -----------
;
;       arguments     : ctcs - <context cable set> 
;
;       returns       : <context set> 
;
;       description   : transforms a <context cable set> into a <context set> 
;
;                                        written :  njm 10/11/88 
;                                        modified:
;
(defun ctcs-to-cts (ctcs)
  (cond ((isnew.ctcs ctcs) (new.cts))
	(t (append (contextset.ctcs ctcs)
                      (ctcs-to-cts (others.ctcs ctcs))))))

;
; ==========================================================================
;
;  seq-to-ns
;  ---------
;
;
;      arguments     :  seq - <node set>
;
;      returns       :  <node set>
;
;      description   :  It sorts the node set through the string< function.
;
;
;
;                                         written : hc/njm 05/10/89
;                                         modified: 
;                                                   
;
(defun seq-to-ns (seq)
  "Sorts a list of assertions using the id"
  (let ((ns (new.ns)))
    (dolist (one-node seq)
      (setq ns (insert.ns one-node ns)))
    ns))

;
;
; ==============================================================================
;
; ctcs-to-ots    
; -----------
;
;       arguments     : ctcs - <context cable set> 
;
;       returns       : <otag list> 
;
;       description   : transforms a <context cable set> into a <otag list> 
;
;                                        written :  mrc 12/13/88 
;                                        modified:
;
(defun ctcs-to-ots (ctcs)
  (cond ((isnew.ctcs ctcs) nil)
	(t (append (repeat-ot (ot.ctcs ctcs) (cardinality.cts (contextset.ctcs ctcs)))
                      (ctcs-to-ots (others.ctcs ctcs))))))


(defun repeat-ot (ot times)
  (let (otlist)
    (dotimes (i times otlist)
      (setq otlist (cons ot otlist)))
    otlist))


;; Various "official" node/lisp transformers (courtesy Stu):

(defun node-to-lisp-object (nde)
  "Returns a Lisp object corresponding to the SNePS node NDE."
  ;; The Lisp object is either a number or a symbol.
  ;; The first read-from-string will handle numbers and simple symbols.
  ;; The second read-from-string will handle weird node names like "#". 
  ;; The final check is for names like "5 pounds" or "Lake Erie".
  (check-type nde node)
  (let* ((nodeAccess (node-na nde))
	 (nodeName (symbol-name nodeAccess))
	 (version1
	 (or
	  (ignore-errors (read-from-string nodeName))
	  (read-from-string (format nil "~S" nodeAccess)))))
    (if (string= (format nil "~A" version1) nodeName)
	version1
      nodeAccess)))


(defun lisp-object-to-node (obj)
  "Returns a SNePS node whose identifier looks like OBJ."
  ;; If we had a strange OBJ it might be a node already,
  ;; but `node' won't find it. Hence, create a name symbol first...
  (let ((node-name
	 (cond ((symbolp obj) obj)
	       ((stringp obj) (intern obj))
	       (t  (intern (build-namestring obj))))))
    ;; ...now try `node'...
    (or (node node-name)
	;; ...and if that fails use `newpbase.n' which returns NIL for
	;; existing nodes, hence, we want to be sure that it's a new one:
	(newpbase.n node-name))))

(defun ns-to-lisp-list (ns)
  "Returns a list of Lisp objects corresponding to the SNePS nodes in
   the node set NS."
  (let (list)
    (do.ns (nde ns (nreverse list))
	   (push (node-to-lisp-object nde) list))))

(defun lisp-list-to-ns (list)
  "Returns a set of nodes whose identifiers look like the printed
   representations of the objects on LIST."
  (let ((ns (new.ns)))
    (dolist (obj (if (atom list) (list list) list) ns)
      (setf ns (insert.ns (lisp-object-to-node obj) ns)))))

(defun apply-function-to-ns (fn ns)
  "Converts the node set NS to a list of lisp objects,
   applies the function FN to that list,
   then converts the result to a node,
   and returns that."
  (lisp-object-to-node
   (apply fn (ns-to-lisp-list (nseval ns)))))



    
    




