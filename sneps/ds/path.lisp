;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: path.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
; Data Type:   <followed paths> ::= ( <followed path> .... <followed path> )
;                    
;              <followed path> ::= ((<node> <flag>) ... (<node> <flag>))
;
;              <flag> ::= t | nil
; =============================================================================
; =============================================================================
; A <followed path> represents the nodes gone through by a path followed to 
; reach a certain node. The <flag> indicates whether the associated node was
; required to be asserted (t) or not (nil). The nodes associated to a flag that
; is t will contribute to the support of the inferred node. For example, the
; <followed path> ((m4 nil) (m3 t) (m2 nil) (m1 t)), means that node m4 was
; reached by following a path that started in m1, that the path went through
; nodes m2 and m3, and that the nodes that should be taken into account when
; computing the support of an eventually inferred node are m1 and m3. We will
; say that a followed path ends in the node of its first element, and starts 
; in the node of its last element. 
;             
;
; =======================================================================================================
;
;                
; CONSTRUCTORS   add-nodes         : ((<node> status) (<node> status) (<node> status) ...) 
;                                     x <followed paths> --> <followed paths>
;
;                intersect-paths   : <followed paths> x <followed paths>
;                                      --> <followed paths>
;
;                put-in-front      : <followed paths> x <followed path>
;                                      --> <followed paths>
;
;                assert-and-remove : ((<node> status) (<node> status) (<node> status) ...)
;                                     x <followed paths> x
;                                     ((<node> status) (<node> status) (<node> status) ...)
;                                     --> <followed paths>
;
; SELECTORS      select-paths : <followed paths> x <node> --> <followed paths>
;
; UTILITY        retrieve-supporting-nodes : <followed paths> x
;                                            ((<node> status) (<node> status) (<node> status) ...)
;                                            -->
;                                            ((<node> (<node> ... <node>)) ... 
;                                             (<node> (<node> ... <node>)))
; ======================================================================================================



; ==============================================================================
;
; select-paths 
; ------------
;
;       arguments     : fpaths - <followed paths>
;                       n - <node>
;
;       returns       : <followed paths>
;
;       description   : Finds all the followed paths of fpaths that end in node
;
;                                          written:  mrc 10/24/89
;                                          modified: hc   4/27/94

(defun select-paths (fpaths node)
  (let (result)
    (dolist (fpath fpaths (nreverse result))
      (if (iseq.n (caar fpath) node)
	  (push fpath result)))))

; ==============================================================================
;
; add-nodes 
; ---------
;
;       arguments     : nodes - ((<node> status) (<node> status) (<node> status) ...)
;                       fpaths - <followed paths>
;
;       returns       : <followed paths>
;
;       description   : Adds to the end of each path of fpaths a <followed path>, (<node> nil),
;                       for each <node> in nodes.
;
;       example       : (add-nodes '((m5 t) (m6 nil)) 
;                                  '(((m3 nil) (m2 t) (m1 t)) ((m3 nil) (m4 t) (m1 t))))
;                       will return
;                       (((M6 NIL) (M3 NIL) (M4 T) (M1 T)) 
;                        ((M6 NIL) (M3 NIL) (M2 T) (M1 T)) 
;                        ((M5 NIL) (M3 NIL) (M4 T) (M1 T))
;                        ((M5 NIL) (M3 NIL) (M2 T) (M1 T)))
;
;                                          written:  mrc 10/24/89


(defun add-nodes (nodes fpaths)
  (let (result)
    (dolist (node nodes result)
      (dolist (path fpaths)
	(setq result (cons (cons (list (car node) nil) path) result))))))

; ==========================================================================
;
; intersect-paths
; ---------------
;
;       arguments     : fpaths1 - <followed paths>
;                       fpaths2 - <followed paths>
;
;       returns       : <followed paths>
;
;       description   : Computes the followed paths in a path P = (and P1 P2)
;                       fpaths1 are the followed paths for path P1
;                       fpaths2 are the followed paths for path P2
;
;       example       : (path-intersect '(((m3 nil) (m2 t) (m1 t)) ((m3 nil) (m4 t) (m1 t)))
;                                       '(((m3 nil) (m8 t) (m1 t)) ((m3 nil) (m5 t) (m1 t))))
;                       will return
;                       (((M3 NIL) (M5 T) (M4 T) (M1 T)) 
;                        ((M3 NIL) (M8 T) (M4 T) (M1 T)) 
;                        ((M3 NIL) (M5 T) (M2 T) (M1 T))
;                        ((M3 NIL) (M8 T) (M2 T) (M1 T)))
;
;                                          written:  mrc 10/24/89


(defun path-intersect (fpaths1 fpaths2)
  (let (result)
    (dolist (path1 fpaths1 result)
      (dolist (path2 fpaths2)
	(setq result (cons (path-intersect-1 path1 path2) result))))))

(defun path-intersect-1 (p1 p2)
  (let ((result (list (car p1))))
    (dolist (el2 p2)
      (unless (member el2 p1 :test #'equal)
	(setq result (append result (list el2)))))
    (dolist (el1 (cdr p1) result)
      (setq result (append result (list el1))))))

; ==========================================================================
;
; assert-and-remove
; -----------------
;
;       arguments     : asserted-nodes - ((<node> status) (<node> status) (<node> status) ...)
;                       fpaths         - <followed paths>
;                       node           - ((<node> status) (<node> status) (<node> status) ...)
;
;       returns       : <followed paths>
;
;       description   : Computes the followed paths when a ! was found in a path.
;                       asserted-nodes is the value returned by the function check-!
;                       (the nodes that were found to be asserted), and node
;                       is the first argument of function path-infer-1 (the nodes
;                       that were required to be asserted).
;       example       : (assert-and-remove '((m3 t)) 
;                                          '(((m3 nil) (m2 nil) (m1 t)) 
;                                            ((m3 nil) (m4 nil) (m1 t)) 
;                                            ((m5 nil) (m4 t) (m1 t)))
;                                          '((m3 t) (m5 t)))
;                       will return
;                        (((M3 T) (M2 NIL) (M1 T)) 
;                         ((M3 T) (M4 NIL) (M1 T)))
;
;
;                                          written:  mrc 10/24/89
;                                          modified: hc   4/27/94

(defun assert-and-remove (asserted-nodes fpaths node)
  (let (result first-node)
    (dolist (fpath fpaths (nreverse result))
      (setq first-node (caar fpath))
      (cond ((member first-node asserted-nodes :key #'car)
	     (push (cons (list first-node t) (rest fpath))
		   result))
	    ((not (member first-node node :key #'car))
	     (push fpath result))))))

; ==========================================================================
;
; retrieve-supporting-nodes
; -------------------------
;
;       arguments     : fpaths - <followed paths>
;                       nodes  - ((<node> status) (<node> status) (<node> status) ...)
;
;       returns       : ((<node> (<node> ... <node>)) ... (<node> (<node> ... <node>)))
;
;       description   : For each node in nodes, for each path in fpaths that ends
;                       in that node this function retrieves the nodes that are
;                       part of a followed path whose second element is t.
;
;       example       : (retrieve-supporting-nodes 
;                           '(((a nil) (c t) (d t)) ((a nil) (s nil) (d t)) ((b nil) (x t) (d t)))
;                           '((a t) (b t)))
;                       will return 
;                         ((B (X D)) (A (D)) (A (C D)))
;
;
;                                          written:  mrc 10/24/89
;                                          modified: hc   4/26/94

(defun retrieve-supporting-nodes (fpaths nodes)
  (let (support node-supports all-supports)
    (dolist (node nodes all-supports)
      (setq node (car node))
      (setq node-supports nil)
      ;; do iteration more efficiently, so we won't get
      ;; exorbitant running times when there are many fpaths:
      (do ((paths (member node fpaths :key #'caar)
		  (member node (cdr paths) :key #'caar)))
	  ((null paths))
	(setq support (new.ns))
	(dolist (node (car paths))
	  (if (second node)
	      (setq support (insert.ns (first node) support))))
	(setq node-supports (adjoin support node-supports :test #'equal)))
      (dolist (support node-supports)
	(push (list node support) all-supports)))))

; ==========================================================================
;
; put-in-front
; ------------
;
;       arguments     : fpaths - <followed paths>
;                       fpath - <followed path>
;                       
;       returns       : <followed paths>
;
;       description   : Puts fpath at the end of each followed path of fpaths.
;
;
;                                          written:  mrc 10/24/89


(defun put-in-front (fpaths fpath)
  (mapcar #'(lambda (l) (cons fpath l)) fpaths))





    
    




