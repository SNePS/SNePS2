;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: path.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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


(defvar flag 0 "Used in check-!")


(defmacro path-star.p (l)
  "Returns T if every member of the list L
   is either a relation, the symbol !, or a legal path;
   NIL otherwise."
  `(do* ((list1 ,l (rest list1))
	 (curr (first list1) (first list1)))
    ((null list1) t)
    (unless (if (atom curr)
		(or (is.r curr) (eql curr '!))
		(legal-def.p curr))
      (sneps-error
       (format nil "Incorrect path specification: ~A" curr)
       "Path Syntax Checker"
       "define-path"))))

(defun legal-def.p (p)
  (or (is.r p)
      (eq p '!)
      (and (consp p)
	   (case (car p)
	     ((converse kstar kplus not irreflexive-restrict)
	      ;; Acceptable Arg(s): path
	      (and (cl:= 2 (length p))
		   (legal-def.p (second p))))
	     ((compose or and)
	      ;; Acceptable Arg(s): path*
	      (path-star.p (rest p)))
	     ((relative-complement exception)
	      ;; Acceptable Arg(s): path path 
	      (and (cl:= 3 (length p))
		   (legal-def.p (second p))
		   (legal-def.p (third p))))
	     ((domain-restrict)
	      ;; Acceptable Arg(s): (path node) path
	      (and (cl:= 3 (length p))
		   (consp (second p))
		   (cl:= 2 (length (second p)))
		   (legal-def.p (first (second p)))
		   (or (symbolp (second (second p)))
		       (stringp (second (second p)))
		       (numberp (second (second p))))
		   (legal-def.p (third p))))
	     ((range-restrict)
	      ;; Acceptable Arg(s): path (path node)
	      (and (cl:= 3 (length p))
		   (consp (third p))
		   (cl:= 2 (length (third p)))
		   (legal-def.p (second p))
		   (legal-def.p (first (third p)))
		   (or (symbolp (second (third p)))
		       (stringp (second (third p)))
		       (numberp (second (third p))))))
	     (otherwise
	      (sneps-error
	       (format nil
		       "Illegal path operation: ~A~%In the specification:  ~A"
		       (car p) p)
	       "Path Syntax Checker"
	       "define-path"))))))

;
;
;
(defsnepscom undefine-path ((&rest rfs))
  (dolist (rel rfs rfs)
    (cond ((is.r rel)
	   (unlink-path rel (get rel ':pathdef))
	   (setf (get rel :pathdef) nil
		 (get rel :altrels) nil)
	   (unlink-path (convt rel)(get (convt rel) ':pathdef))
	   (setf (get (convt rel) :pathdef) nil
		 (get (convt rel) :altrels) nil))
	  (t
	   (beep)
	   (format t "~%WARNING: ~A  is not a valid relation. ~%" rel)))))

(defun unlink-path (rel path-def)
  (cond ((null path-def) t)
	((atom path-def)
	 (if (is.r path-def)
	     (setf (get path-def ':fwd-paths)
		   (remove-if #'(lambda (lst) (eq rel (car lst)))
			      (get path-def ':fwd-paths)))
	     t))
	(t
	 (unlink-path rel (car path-def))
	 (unlink-path rel (cdr path-def)))))
;
;
;define-path
;-----------
;
;arguments:
;   REL - a relation
;   PATH-DEF - the alternate path definition of REL
;
;returns  : t    
;
;description: Given the relation REL and its alternate path
;   definition PATH-DEF, it first determines the pair of forward     
;   path-based inference paths (Ptail, Phead) for each single
;   relation r in PATH-DEF and inserts the triple
;   (REL Ptail Phead) into the list which is the value of the 
;   :FWD-PATHS property of r.  If a single relation r appears more    
;   than once in PATH-DEF, say appears k times, and 
;     (Ptail1, Phead1), (Ptail2, Phead2), ... , (Ptailk, Pheadk)
;   are its respective pairs of forward path-based inference
;   paths; then the list
;     (REL Ptail1 Phead1 Ptail2 Phead2 ... Ptailk Pheadk)
;   is inserted into the :FWD-PATHS property of r.  Then the
;   function does the same thing with the converse of REL, i.e.
;   REL- , and its alternate path, i.e. the converse of PATH-DEF.
;
;side-effects: It side effects the :FWD-PATHS property of all the 
;   single relations in PATH-DEF and its converse.
;
;                                       modified by scs 2/24/88 & 2/25/88
;                                                ssc  5/10/89
;                                                hc   7/18/93
;                                                scs  7/11/94

(defsnepscom define-path ((&rest rels&path-defs))
  (do* ((rs&pds rels&path-defs (cddr rs&pds))
	(rel (first rs&pds) (first rs&pds))
	(path-def (second rs&pds) (second rs&pds)))
      ((null rs&pds) (values))
    (when (legal-def.p path-def)
      (when (atom path-def) (setf path-def (list 'compose path-def)))
      (unless (is.r rel)
	(let* ((name (symbol-name rel))
	       (x (1- (length name))))
	  (if (char= (char name x) #\-)
	      (new.r (intern (subseq name 0 x) (find-package 'sneps)))
	    (new.r rel))))
      (cond ((get rel :pathdef)
	     (beep)
	     (format t "~&~A already has the path definition: ~%~% ~S"
		     rel (get rel :pathdef))
	     (cond ((yes-or-no-p "~%Do you really want to redefine it?")
		    (eval `(undefine-path ,rel))
		    (pathlink1 rel path-def)
		    (pathlink1 (convt rel) (convt-path path-def)))
		   (t t)))
	    (t (pathlink1 rel path-def)
	       (pathlink1 (convt rel) (convt-path path-def)))))))

;
;
;pathlink1
;---------
;
;arguments:
;   REL - a relation
;   PATH-DEF - the alternate path definition of REL
;
;returns  : nil
;
;description: Given the relation REL and its alternate path 
;   definition PATH-DEF, it determines the pair of forward
;   path-based inference paths (Ptail, Phead) for each single 
;   relation r in PATH-DEF and inserts the triple
;   (REL Ptail Phead) into the list which is the value of the 
;   :FWD-PATHS property of r.  If a single relation r appears more    
;   than once in PATH-DEF, say appears k times, and
;     (Ptail1, Phead1), (Ptail2, Phead2), ... , (Ptailk, Pheadk)
;   are its respective pairs of forward path-based inference
;   paths; then the list
;     (REL Ptail1 Phead1 Ptail2 Phead2 ... Ptailk Pheadk)
;   is inserted into the :FWD-PATHS property of r.  
;
;side-effects: It side effects the :FWD-PATHS property of all the 
;   single relations in PATH-DEF. 
;
;

(defun pathlink1 (rel path-def)
  (setf (get rel :pathdef) path-def
	(get rel :altrels)
	;; a list of the relations that could start a path
	;;    that would imply rel by path-based inference.
	(pathfirstrels path-def))
  (let ((paths (pathconnect path-def)) temp1 old-path)
    (dolist (path-triple paths)
      (setf (get (car path-triple) ':fwd-paths)
	    (remove rel (get (car path-triple) ':fwd-paths)
		    :key #'car)))
    (dolist (path-triple paths)
      (cond ((setq old-path (cl::find (car path-triple) temp1 
					:key #'car))
	     (do ((path-pairs (cdr old-path)))
		 ((equal (cdr path-triple)
			 (list (car path-pairs) (cadr path-pairs)))) 
		 (setq path-pairs (cddr path-pairs))
		 (if (null path-pairs)
		   (return
		    (setq temp1
			  (substitute
			   (append old-path (cdr path-triple))
			   old-path temp1))))))
	    (t (setq temp1 (cons path-triple temp1)))))
    (dolist (path-pairs temp1)
      (setf (get (car path-pairs) ':fwd-paths)
	    (cons (cons rel (cdr path-pairs)) 
		  (get (car path-pairs) ':fwd-paths)))))
  (format t "~&~A implied by the path ~A~%" rel path-def))

(defun pathfirstrels (path)
  "Returns a list of the relations that could start the path."
  (cond ((null path) nil)
	((atom path) (list path))
	(t (case (first path)
	     (converse (pathfirstrels (convt-path (second path))))
	     (compose (pathfirstrels
		       (if (eq (second path) 'snepsul:!)
			   (third path)
			 (second path))))
	     ((sneps:kstar sneps:kplus sneps:relative-complement
	       sneps:irreflexive-restrict sneps:exception sneps:range-restrict)
	      (pathfirstrels (second path)))
	     (or (mapcan #'pathfirstrels (rest path)))
	     (and (shortest
		   (mapcar #'(lambda (p)
			       (delete-duplicates (pathfirstrels p)))
			   (rest path))))
	     (sneps:domain-restrict (pathfirstrels (third path)))
	     (t (pathfirstrels
		 (if (eq (first path) 'snepsul:!)
		     (second path)
		   (first path))))))))

(defun shortest (lists)
  "Returns the shortest list in the given list of lists."
  (loop with shortlist = (first lists)
      with leastlength = (length (first lists))
      for testlist in (rest lists)
      if (< (length testlist) leastlength)
      do (setf shortlist testlist
	       leastlength (length testlist))
      finally (return shortlist)))
;
;
;
;pathconnect
;-----------
;
;argument :
;   PATH - a path conforming to the syntax given in the SNePS user    
;          manual.
;
;returns  : the list containing all the triples (REL Ptail Phead)
;           found for all the single relations REL in PATH.
;
;description: It finds the triple (REL Ptail Phead) for each
;   single relation REL in PATH, and puts them all in a list and
;   returns it.
;
;

(defun pathconnect (path)
   (cond
      ((eq path '!) nil)
      ((atom path) (list (list path nil path)))
      ((eq (car path) 'compose)
       (do  ((left nil (cons (convt-path component) left))
             (right (cddr path) (cdr right))
             (component (cadr path) (car right))
             (result))
            (nil)
          (setq result  
            (append result
              (compose-combine (pathconnect component) left path)))    
          (if (null right) (return result))))
      ((eq (car path) 'converse) (pathconnect (convt-path
                                                   (cadr path))))
      ((or (member (car path) '(kstar kplus)))
       (k-combine (pathconnect (cadr path)) path))
      ((member (car path) '(or and irreflexive-restrict))
       (common-combine (cdr path) path))
      ((eq (car path) 'not) nil)
      ((member (car path) '(exception relative-complement range-restrict))
       (common-combine (list (cadr path)) path))
      ((eq (car path) 'domain-restrict)
       (common-combine (cddr path) path))))    
;
;
;
;compose-combine
;---------------
;
;arguments :
;   PATHS - a list of triples (rel Ptail Phead) where rel is a
;      single relation and (Ptail Phead) is its pair of forward 
;      path-based inference paths.
;   LEFT - a list of paths conforming to the syntax given in the     
;      SNePS user manual.
;   COMPOSE-PATH - a path in the form (compose r1 r2 ... rn)
;
;returns   : a list of triples (rel Ptail Phead)
;
;description: It returns PATHS after modifying each of the
;   elements in PATHS from the form (rel Ptail Phead) to the form    
;   (rel Ptail' COMPOSE-PATH) where Ptail' is a path formed by
;   composing Ptail with LEFT.
;
;

(defun compose-combine (paths left compose-path)
   (mapcar
     #'(lambda (x)
         (list (car x)
               (cond ((and (null (cadr x)) (null left)) nil)
                     ((null (cadr x)) (if (cdr left)
                                          (cons 'compose left)
                                          (car left)))
                     ((null left) (cadr x))
                     (t (cons 'compose (cons (cadr x) left))))
               compose-path))
     paths))
;
;
;
;k-combine
;---------
;
;arguments:
;   PATHS - a list of triples (rel Ptail Phead) where rel is a
;      single relation and (Ptail Phead) is its pair of forward    
;      path-based inference paths.
;   K-PATH - a path in the form (kstar P) or (kplus P)
;
;returns  : a list of the triples (rel Ptail Phead)
;
;description: It returns PATHS after modifying each of the triples    
;   in PATHS from the form (rel Ptail Phead) to the form
;   (rel Ptail' K-PATH) where Ptail' is the path formed by
;   composing Ptail with (kstar P).
;
;

(defun k-combine (paths k-path)
   (mapcar #'(lambda (x)
                (list (car x)
                      (if (cadr x)
                          (list 'compose (cadr x) 
                            (convt-path (cons 'kstar
                                              (cdr k-path))))
                          (convt-path (cons 'kstar (cdr k-path))))    
                      k-path))
           paths))
;
;
;
;common-combine
;--------------
;
;arguments:
;   PATH-LIST - a list of paths conforming to the syntax given in    
;      the SNePS user manual.
;   PATH - a path conforming to the syntax given in the SNePS user    
;      manual.
;
;returns  : a list of triples in the form (rel Ptail Phead)
;
;description: It finds the pair of forward path-based inference 
;   paths (rel Ptail Phead) for every single relation appearing in
;   PATH-LIST with respect to PATH.
;
;

(defun common-combine (pathlist path)
   (mapcan #'(lambda (x)
               (mapcar #'(lambda (y) (list (car y) (cadr y) path)) 
                       (pathconnect x)))
           pathlist))
;
;
;
;convt
;-----
;
;argument: PATH-NAME - a single relation
;
;returns : the converse of PATH-NAME
;
;description: It finds the converse of PATH-NAME and returns it.
;
;

(defun convt (path-name)
  (if (eq path-name '!) '!
      (converse.r path-name)))
;
;
;
;fwd-infer
;---------
;
;arguments:
;   TAIL - the node at the tail of the asserted arc
;   ARC - the asserted arc
;   HEAD - the node at the head of the asserted arc
;
;returns  : a list in the form
;   
;   ((REL1 (N11 A11 B11 C11 ...) (N12 A12 B12 C12 ...) ...)
;    (REL2 (N21 A21 B21 C21 ...) (N22 A22 B22 C22 ...) ...) ...)
;
;   where Nij, Aij, Bij, Cij, etc., are atoms representing the
;   nodes in SNePS.  The interpretation of this list is that Nij    
;   has the relation RELi with Aij, Bij, Cij, and so on, after ARC    
;   is asserted.
;
;description: fwd-infer is the function which actually makes the 
;   forward path-based inference in SNePS.  After we asserted ARC
;   between TAIL and HEAD in the network, we can call the function
;   fwd-infer to find the nodes, if any, which have a path of a 
;   relation established between each other after the assertion.  
;   The function fwd-infer will infer a node i to have a relation 
;   rel with a node j, only if there did not exist a path of rel
;   between i and j before the assertion.
;
;

(defmacro fwd-infer (tail arc head)
  "Perform forward path-based inference with args - <tail> <arc> <head>."
  `(fwd-infer1 ,tail ,arc ,head))

(defun fwd-infer1 (tail arc head)
   (declare (special tail arc head))
   (let (infer-nodes infered-list)
      (dolist (paths (get arc ':fwd-paths) infered-list)
         (cond
            ((setq infer-nodes
                (extract
                   (do  ((inferences) (path-list (cdr paths)))
                        (nil)
                      (setq inferences
                         (infer-combine inferences
                               (infer-links (car path-list)
                                            (cadr path-list))))
                      (setq path-list (cddr path-list))
                      (if (null path-list) (return inferences)))))    
             (push (cons (car paths) infer-nodes) 
			infered-list))))))

;
;
;infer-links
;-----------
;
;arguments: 
;   LEFT - a path conforming to the syntax given in the SNePS user    
;      manual.
;   RIGHT - a path conforming to the syntax given in the SNePS
;      user manual.
;
;global variable used: TAIL
;
;returns  : a list in the form
;
;   ((N1 (S11 status) (S12 status) (S13 status) ...)
;    (N2 (S21 status) (S22 status) (S23 status) ...) ...)
;
;   where Ni and Sij are atoms representing the nodes in SNePS,
;   and status is either T or NIL.  The interpretation of this
;   list is that Sij can be reached from Ni by traversing a path
;   of RIGHT starting at Ni.  If the status associated with the 
;   node Sij is T, then Sij is reachable from Ni only by a path of    
;   RIGHT passing through the asserted arc.  If the status
;   associated with Sij is NIL, then Sij is reachable from Ni by a
;   path of RIGHT without passing through the asserted arc.  
;
;description: The arguments LEFT and RIGHT represent the Ptail and
;   Phead of a single relation respectively.  The function
;   infer-links first finds all the nodes Ni that can be reached
;   by the global variable TAIL by following LEFT starting at
;   TAIL.  Then, for each of the Ni's, the function finds all the 
;   nodes Sij that can be reached from that Ni by traversing the 
;   path RIGHT, and sets their associated status accordingly.  
;   Finally the function returns a list of all the Ni's and Sij's.    
;
;

(defun infer-links (left right)
   (declare (special tail arc head))
   (let ((flag 1))
        (declare (special flag))
      (mapcan #'(lambda (x)
                   (let ((l1 (path-infer x right)))
                      (if l1 (list (cons x l1)))))
              (prog1
                 (mapcar #'(lambda (y) (car y))
                   (path-infer tail left))
                 (setq flag 0)))))
;
;
;
;extract
;-------
;
;argument: 
;   INFERENCES - a list in the form
;
;      ((N1 (S11 status) (S12 status) (S13 status) ...)
;       (N2 (S21 status) (S22 status) (S23 status) ...) ...)
;
;      where Ni and Sij ard nodes, and status is either T or NIL.    
;
;returns : a list in the form
;
;   ((N1 A1 B1 C1 ...) (N2 A2 B2 C2 ...) (N3 A3 B3 C3 ...) ...)
;
;   where Ni, Ai, Bi, Ci, etc., are nodes.
;
;description: For each of the elements in INFERENCES, it creates a    
;   list of nodes containing the nodes Ni and those Sij with a T
;   status in the element.  It then puts all these newly created 
;   lists into a list and returns it.
;
;

(defun extract (inferences)
   (mapcan
     #'(lambda (x)
         (let ((l1 (mapcan #'(lambda (y)
                                (if (cadr y) (list (car y))))
                           (cdr x))))
            (if l1 (list (cons (car x) l1)))))
     inferences))
;
;
;
;infer-combine
;-------------
;
;arguments:
;   L1 and L2 are lists in the form
; 
;   ((N1 (S11 status) (S12 status) (S13 status) ...)
;    (N2 (S21 status) (S22 status) (S23 status) ...) ...)
;
;   where Ni and Sij are nodes, and status is either T or NIL.
;
;returns  : a list having the same form as the arguments above.
;
;description: It returns the union of the elements in L1 and L2 
;   using the Ni's as the key.  If the node Ni of the element
;      (Ni (Si1 status) (Si2 status) (Si3 status) ...)
;   in L1, and the node Nj of the element
;      (Nj (Sj1 status) (Sj2 status) (Sj3 status) ...)
;   in L2 are the same, then the element
;      (Nk (Sk1 status) (Sk2 status) (Sk3 status) ...),
;   where Nk=Ni=Nj and the (Skt status)'s are the union of 
;   (Si1 status), (Si2 status),..., (Sj1 status), (Sj2 status),...;
;   will be included in the union of L1 and L2.  The union of
;   (Siv status) and (sjt status), where Siv=Sjt and the two 
;   statuses are different, is taken to be (Siv NIL).
;
;

(defun infer-combine (l1 l2)
   (let (found)
      (dolist (node-list l2 l1)
         (cond ((setq found (cl::find (car node-list) l1 :key #'car))    
                (setq l1
                   (substitute (cons (car node-list)
                                     (pair-combine (cdr found)
                                               (cdr node-list)))
                               found l1)))
               (t (setq l1 (append l1 (list node-list))))))))
;
;
;
;pair-combine
;------------
;
;argument:
;   L1 and L2 are lists in the form
;
;      ((S1 status) (S2 status) (S3 status) ...)
;
;   where Si are nodes and status is either T or NIL.
;
;returns : a list having the same form as the arguments above
;
;description: It returns the union of the elements in L1 and L2
;   using the Si's as the key.  The union of (Si status) and 
;   (Sj status), where Si=Sj and the two statuses are different,
;   is taken to be (Si NIL).
;
;

(defun pair-combine (l1 l2)
   (let (found)
      (dolist (pair l2 l1)
         (cond ((setq found (cl::find (car pair) l1 :key #'car))
                (if (null (cadr pair)) (setq l1
                                          (substitute
                                               pair found l1)))) 
               (t (setq l1 (append l1 (list pair))))))))
;
;
;
;path-infer
;----------
;
;arguments:
;   NODE - a list in the form
;
;      ((N1 status) (N2 status) (N3 status) ...)
;
;      where Ni are nodes and status is either T or NIL.
;   PATH - a path conforming to the syntax given in the SNePS user    
;      manual.
;
;returns  : a list having the same form as the argument NODE above
;
;description: It finds all the nodes Ni that can be reached from 
;   some node in NODE by traversing PATH.  Every node in the Ni's
;   is associated with a status.  For each node in the Ni's, if 
;   that node is reachable from the node in the original call of
;   the function path-infer by the function infer-links, only by a    
;   path of PATH passing through the asserted arc, then that node
;   is associated with a status of T.  If that node is reachable
;   from the original node by a path of PATH without passing
;   through the asserted arc, then that node is associated with a
;   NIL status.  The function path-infer puts all these
;   (Ni status) pairs into a list and returns it.
;
;                                      modified: ssc  5/10/89
;                                                mrc  10/24/89
;
;   Modifications: This function returns exactly the same as before,
;   but now it calls path-infer-1, which is the same as old
;   path-infer except for the fact that it also returns the
;   nodes gone through by path.
;
;
(defun path-infer (node path)
  (car (path-infer-1 node path (mapcar #'(lambda (l) (list (list (car l) t)))
				       node))))



;;; Adds error checking, because some errors had been causing signal number 10 (Bus error)
(defun path-infer-1 (node path followed-paths)
  (cond
   ((null node)
    (list nil nil))
   ((null path)
    (list node followed-paths))
   ((symbolp node)
    (check-type node node
      "a SNePS node. May be trying to follow a path from a non-node")
    (path-infer-1 (list (list  (node node) nil)) path followed-paths))
   ((typep node 'node)
    (path-infer-1 (list (list node nil)) path  followed-paths))
   ((eq path '!)
    (let ((asserted-nodes (check-! node)))
      (list asserted-nodes (assert-and-remove asserted-nodes  followed-paths node))))
   ((atom path)
    (let ((found-nodes nil)
	  (result (list nil nil))
	  (new-paths nil))
      (dolist (one-node node result)
	(check-type
	    (first one-node) node
	  "a SNePS node. May be trying to follow a path from a non-node")
	(setq found-nodes (check-link (nodeset.n (car one-node) path)  one-node path))
	(setq new-paths nil)
	(unless (null found-nodes)
	  (setq new-paths (add-nodes found-nodes
				     (select-paths followed-paths (car one-node)))))
	(setq result (list (pair-combine found-nodes (car result))
			   (append new-paths (cadr result)))))))
   ((null (rest path))
    (path-infer-1 node (first path) followed-paths ))
   (t
    (case (car path)
      ((compose)
       (let ((up-to-now-path (path-infer-1 node (cadr path) followed-paths))) 
	 (if (cddr path)
	     (path-infer-1 (car up-to-now-path)
			   (cons 'compose (cddr path))
			   (cadr up-to-now-path))
	   up-to-now-path )))
      ((converse)
       (path-infer-1 node (convt-path (cadr path)) followed-paths))    
      ((or)
       (do ((path-list (cdr path) (cdr path-list))
	    (u-list (list nil nil) (let ((res (path-infer-1 node
							    (car path-list)
							    followed-paths)))
				     (list (pair-combine (car u-list) (car res))
					   (append (cadr u-list) (cadr res))))))
	   ((null path-list) u-list)))
      ((and)
       (let ((first-result (path-infer-1 node (cadr path) followed-paths)))
	 (do  ((path-list (cddr path) (cdr path-list))
	       (int-list first-result
			 (let* ((current-result (path-infer-1 node (car path-list) followed-paths))
				(common-nodes (pair-intersctn (car current-result)
							      (car int-list)))
				(common-nodes-paths (apply 'append
							   (mapcar #'(lambda (l)
								       (path-intersect
									(select-paths (cadr int-list) (car l))
									(select-paths (cadr current-result) (car l))))
								   common-nodes))))
			   (list common-nodes common-nodes-paths)))) 
	     ((or (null path-list) (null int-list)) int-list))))
      ((not)
       (let* ((first-result (path-infer-1 node (cadr path) followed-paths))
	      (l1 (complemt (get 'nodes ':val)
			    (mapcar #'(lambda (x) (car x))
				    (car first-result)))))
	 (cons
	  (if (cl:find nil node :key #'cadr)
	      (mapcar #'(lambda (x) (list x nil)) l1)
	    (mapcar #'(lambda (x) (list x t)) l1))
	  ;; Not sure whether this is ok as is (hc, 07/17/94):
	  (cdr first-result))))
      ((relative-complement)
       ;; Rephrase `(relative-complement P Q)' as `(and P (not Q))' (hc):
       (path-infer-1
	node `(and ,(cadr path) (not ,(caddr path))) followed-paths))
      ((irreflexive-restrict)
       (let* ((l1 (path-infer-1 node (cadr path) followed-paths))
	      (l2 (complemt (mapcar #'(lambda (x) (car x)) (car l1))
			    (mapcar #'(lambda (x) (car x)) node))))
	 (list (mapcan #'(lambda (x) (if (member (car x) l2) (list x)))
		       (car l1))
	       (if (null l2) followed-paths (apply 'append (mapcar #'(lambda (x) (select-paths (cadr l1) x))
								   l2))))))
      ((kstar)
       (do  ((found node) (path (cadr path)) (temp nil nil) (old-pair) (found-paths followed-paths))
	   ((null node) (list found found-paths))
	 (let ((first-time (path-infer-1 node path found-paths)))
	   (dolist (pair (car first-time))
	     (cond ((setq old-pair (cl::find (car pair) found :key #'car))
		    (cond ((and (null(cadr pair))
				(eq (cadr old-pair) t))
			   (setq found (substitute pair old-pair found))    
			   (setq temp (cons pair temp)))))
		   (t (setq found (cons pair found))
		      (setq found-paths (append found-paths (select-paths (cadr first-time) (car pair))))
		      (setq temp (cons pair temp))))))
	 (setq node temp)))
      ((kplus)
       (let ((first-result (path-infer-1 node (cadr path) followed-paths))) 
	 (path-infer-1 (car first-result)
		       (cons 'kstar (cdr path))
		       (cadr first-result))))
      ((exception)
       (do ((temp1 (length-path-infer
		    (mapcar #'(lambda (x)
				(list (car x) (cadr x) 0))
			    node)
		    (cadr path)))
	    (temp2 (length-path-infer 
		    (mapcar #'(lambda (x) (list (car x) (cadr x) 0))
			    node)
		    (caddr path)))
	    (result) (n1) (t1) (n2) (t2) (pair1) (pair2) (triple) (l1))
	   ((null temp1) result)
	 (setq triple (car temp1)
	       temp1 (cdr temp1)
	       l1 (find-quadruple triple (car triple) temp1 temp2)
	       temp1 (remove (car triple) temp1 :key #'car)
	       n1 (cadr l1) t1 (caddr l1) n2 (cadddr l1) t2 (fifth l1)    
	       pair1 (list (car triple) nil)
	       pair2 (list (car triple) t))
	 (case (car l1)
	   (1       (push pair2 result))
	   ((2 3)   (push pair1 result))
	   (4       (if (cl:> n2 n1) (push pair1 result)))
	   ((5 10)  (if (cl:> t2 n1) (push pair1 result)))
	   (6       (if (cl:> n2 t1) (push pair2 result)))
	   ((7 11)  (if (cl:> t2 t1) (push pair2 result)))
	   (8       (cond ((<= n2 t1))
			  ((cl:< n1 n2) (push pair1 result))
			  (t (push pair2 result))))
	   (9       (if (cl:> t2 t1) (push pair1 result)))
	   (12      (cond ((<= t2 t1))
			  ((cl:< n1 n2) (push pair1 result))
			  (t (push pair2 result)))))))
      ((domain-restrict)
       (let ((found-nodes nil)
	     (found-paths nil)
	     (one-result nil)
	     (result nil))
	 (setq result
	   (dolist (n node (list found-nodes found-paths))
	     (setq one-result (path-infer-1 (list n) (caadr path) followed-paths))
	     (setq found-nodes (append (dr-restrict n (cadr (cadr path)) (car one-result))
				       found-nodes))
	     (setq found-paths (append (put-in-front
					(select-paths (cadr one-result) (node (cadr (cadr path))))
					(list (car n) nil))
				       found-paths))))
	 (path-infer-1 (car result) (caddr path)  (cadr result))))
      ((range-restrict)
       (let ((found-nodes nil)
	     (found-paths nil)
	     (one-result nil)
	     (result (path-infer-1 node (cadr path) followed-paths))) 
	 (dolist (n (car result) (list found-nodes found-paths))
	   (setq one-result (path-infer-1 (list n) (caaddr path) (cadr result)))
	   (setq found-nodes (append (dr-restrict n (cadr (caddr path)) (car one-result))
				     found-nodes))
	   (setq found-paths (append (put-in-front (select-paths (cadr one-result)
								 (node (cadr (caddr path))))
						   (list (car n) nil))
				     found-paths)))))
      (otherwise
       (path-infer-1 node (cons 'compose path) followed-paths))))))

;
; ==========================================================================
;
; pathto
; ------
;
;       arguments     : path - <path>
;                       nbs - <node bind set>
;
;       returns       : <node bind set>
;
;       description   : Find all the nodes which have the path to the any 
;                       node in the given nodeset.
;
;                                          written:  SCS 02/24/88
;                                          modified: ssc  5/10/89
;
(defun pathto (path nbs)
  "Returns a <node bind set> whose nodes are the ones which have the given path
   to the nodes in the given <node bind set>"
  (let ((path (convt-path path))
	(outnbs (new.nbs)))
    (do.nbs (nb nbs outnbs)
      (let ((nde (node.nb nb))
	    (bs (bindset.nb nb))
	    (newnbs (new.nbs)))
	(dolist (node-status (path-infer (list (list nde t)) path)
			     (setq outnbs (union.nbs newnbs outnbs)))
	  (push (new.nb (first node-status) bs) newnbs))))))


; ==========================================================================
;
; pathfrom
; --------
;
;       arguments     : path - <path>
;                       n - <node>
;
;       returns       : <node>
;
;       description   : Find all the nodes at the end of the given path
;                       from the given node.
;
;                                          written:  SCS 02/24/88
;                                          modified: scs 12/23/88
;                                                    ssc  5/10/89
;                                                    mrc 10/24/89
;
;       modifications : global variables:
;                          match:supporting-nodes - nodes to be taken
;                            into account by the function match:unify
;                            when computing the support of a node
;                            inferred by path-based inference.
;                          match:top-node - see function match:unify
;
(defun pathfrom (path n)
  "Returns a set of nodes which are at the end of the given path
   from the given node."
  (declare (special match:supporting-nodes match:top-node))
  (if (or (cl:< 1 (length path))
	  (get (car path) ':fwd-paths))
      (let ((result
	     (path-infer-1 (list (list n t)) path
			   (if (boundp 'match:top-node)
			       (if (iseq.ns match:top-node n)
				   (list (list (list n t)))
				   (list (list (list n nil)
					       (list match:top-node t))))
			       (list (list (list n t)))))))
	(if (boundp 'match:supporting-nodes)
	    (setq match:supporting-nodes
		  (append (list (retrieve-supporting-nodes
				 (cadr result)
				 (car result)))
			  match:supporting-nodes)))
	;; make a nodeset:
	(let ((nset (new.ns)))
	  (dolist (n (mapcar #'first (car result)) nset)
	    (setq nset (insert.ns n nset))))
	)
      (getf (node-fcableset n) (car path))))

;
; ==========================================================================
;
;length-path-infer
;-----------------
;
;arguments:
;   NODE - a list whose elements are 3-tuples in the form
;          (NODE STATUS LENGTH).
;   PATH - a path conforming to the syntax given in the SNePS user    
;          manual.
;
;returns  : a list whose elements are 3-tuples in the form
;   (NODE STATUS LENGTH).
;
;description: The function length-path-infer is used to implement
;   the exception feature in forward path-based inference.  
;   length-path-infer returns a list of nodes that can be reached 
;   by following PATH starting from some node in NODE.  Each 
;   element in the returned list is a 3-tuple of the form 
;   (NODE STATUS LENGTH).  LENGTH is the shortest path-length from    
;   the node in the original function call to length-path-infer by
;   the function path-infer, to the node NODE in this 3-tuple with
;   this particular status.  For each distinct node N that 
;   length-path-infer returns, length-path-infer will return
;   
;   (a) the shortest path-length with the NIL status, if any; and
;   (b) the shortest path-length with the T status, if any, which
;       is also smaller than the shortest path-length with the NIL    
;       status mentioned in (a).
;
;

(defun length-path-infer (node path)
 (cond
  ((null node) nil)
  ((eq path '!) (check-! node))
  ((atom path)
   (triple-combine
     (cond
       ((var (caar node))
        (triple-check-link (nodeset.n (caar node) path)
                           (car node)
                           path))
       ((isup.r path)
        (triple-check-link (nodeset.n (caar node) path) (car node) path))
       (t (triple-check-link (nodeset.n (caar node) path)
                             (car node) 
                             path)))
     (length-path-infer (cdr node) path)))
  ((eq (car path) 'compose)
   (if (cddr path)
       (length-path-infer (length-path-infer node (cadr path)) 
                          (cons 'compose (cddr path)))
       (length-path-infer node (cadr path))))
  ((eq (car path) 'converse)
   (length-path-infer node (convt-path (cadr path))))
  ((eq (car path) 'or)
   (do ((path-list (cdr path) (cdr path-list))
        (u-list nil (triple-combine
                       (length-path-infer node (car path-list))
                       u-list)))
       ((null path-list) u-list)))
  ((eq (car path) 'and) (length-path-and node path))
  ((eq (car path) 'not)
   (let ((temp (complemt
                  (get 'nodes ':val)
                  (mapcar #'(lambda (x) (car x))
                          (length-path-infer node (cadr path))))))    
      (cond ((cl::find nil node :key #'cadr)
             (mapcar #'(lambda (x) (list x nil 0)) temp))
            (t (mapcar #'(lambda (x) (list x t 0)) temp)))))
  ((eq (car path) 'relative-complement)
   (let ((temp (mapcar #'(lambda (x) (car x))
                       (length-path-infer node (caddr path)))))
      (mapcan
         #'(lambda (x) (if (not (member (car x) temp)) (list x)))    
         (length-path-infer node (cadr path)))))
  ((eq (car path) 'irreflexive-restrict)
   (let ((temp (mapcar #'(lambda (x) (car x)) node)))
      (mapcan
         #'(lambda (x) (if (not (member (car x) temp)) (list x)))
         (length-path-infer node (cadr path)))))
  ((eq (car path) 'kstar) (length-path-kstar node path))
  ((eq (car path) 'kplus)
   (length-path-infer (length-path-infer node (cadr path))
                      (cons 'kstar (cdr path))))
  ((eq (car path) 'exception)
   (length-path-exception node path))
  ((eq (car path) 'domain-restrict)
   (length-path-domain node path))
  ((eq (car path) 'range-restrict)
   (length-path-range node path))))
;
;
;
;pair-intersctn
;--------------
;
;arguments: 
;   L1 - a list of (NODE STATUS) pairs
;   L2 - a list of (NODE STATUS) pairs
;
;returns  : a list of (NODE STATUS) pairs
;
;description: It returns the intersection of L1 and L2.  If a node    
;   N appears in both L1 and L2 with different statuses, then
;   (N T) is included in the returned list.
;
;

(defun pair-intersctn (l1 l2)
  (mapcan #'(lambda (x)
	      (mapcan #'(lambda (y)
			  (if (equal (car x) (car y))
			    (if (eq (cadr x) t)
			      (list x)
			      (list y))))    
		      l2))
	  l1))
;
;
;
;complemt
;--------
;
;arguments: L1 - a list
;           L2 - a list
;
;returns  : a list
;
;description: It returns the set difference L1 - L2.
;
;

(defun complemt (l1 l2)
   (mapcan #'(lambda (x) (if (not (member x l2)) (list x))) l1))
;
;
;
;dr-restrict
;-----------
;
;arguments : NODE - a (NODE STATUS) pair
;            Z - a node
;            NODE-LIST - a list of (NODE STATUS) pairs
;
;returns   : a list of (NODE STATUS) pairs
;
;description: Let the argument NODE be (N STATUS).  If Z appears
;   in NODE-LIST as (Z T), then the list ((N T)) is returned.  If
;   Z appears as (Z NIL), then the list ((N STATUS)) is returned.  
;   If Z does not appear in NODE-LIST, then NIL is returned.
;
;                                    modified:  ssc  5/10/89
;
;
;
(defun dr-restrict (node z node-list)
      (when (node z)
	(cond ((setq z (cl::find (node z) node-list :key #'car))
	       (if (eq (cadr z) t) (list (list (car node) t))
		   (list node))))))

;
;
;
;check-link
;----------
;
;arguments: NODE-LIST - a list of nodes
;           PAIR - a (NODE STATUS) pair
;           PATH - a single relation
;
;global variables used: TAIL, ARC, HEAD
;
;returns  : a list of (NODE STATUS) pairs
;
;description: It sets a status for each node N in NODE-LIST.  If
;   PAIR has a T status, then every node in NODE-LIST is
;   associated with a T status.  Otherwise if N is a terminal node    
;   of the asserted arc, then N is associated with a T status. 
;   All other nodes in NODE-LIST are associated with a NIL status.
;
;

(defun check-link (node-list pair path)
   (declare (special tail arc head))
   (cond
      ((null node-list) nil)
      ((cadr pair) (mapcar #'(lambda (x) (list x t)) node-list))
      ((and (equal (node tail) (car pair)) (equal arc path))
       (mapcar #'(lambda (x) (if (equal x (node head)) (list x t)
                                                (list x nil)))
               node-list))
      ((and (equal (node head) (car pair)) (equal arc (convt path)))
       (mapcar #'(lambda (x) (if (equal x (node tail)) (list x t)
                                                (list x nil)))
               node-list))
      (t (mapcar #'(lambda (x) (list x nil)) node-list))))
;
;
;
;triple-check-link
;-----------------
;
;arguments : NODE-LIST - a list of nodes
;            TRIPLE - a (NODE STATUS LENGTH) triple
;            PATH - a single relation
;
;global variables used: TAIL, ARC, HEAD
;
;returns   : a list of (NODE STATUS LENGTH) triples
;
;description: It sets a status and a path-length for each node N 
;   in NODE-LIST.  If TRIPLE has a T status, then every node in 
;   NODE-LIST is associated with a T status.  Otherwise if N is a    
;   terminal node of the asserted arc, then N is associated with a    
;   T status.  All other nodes in NODE-LIST is associated with a 
;   NIL status.  Moreover every node in NODE-LIST is associated
;   with a path-length of one plus the LENGTH in TRIPLE.
;
;

(defun triple-check-link (node-list triple path)
 (declare (special tail arc head))
 (cond
  ((null node-list) nil)
  ((eq (cadr triple) t) 
   (mapcar #'(lambda (x) (list x t (1+ (caddr triple))))
           node-list))
  ((and (equal (node tail) (car triple)) (equal arc path))
   (mapcar #'(lambda (x) (cond ((equal x (node head))
                                (list x t (1+ (caddr triple))))
                               (t (list x nil
                                        (1+ (caddr triple))))))
           node-list))
  ((and (equal (node head) (car triple)) (equal arc (convt path)))
   (mapcar #'(lambda (x) (cond ((equal x (node tail))
                                (list x t (1+ (caddr triple))))
                               (t (list x nil
                                        (1+ (caddr triple))))))
           node-list))
  (t (mapcar #'(lambda (x) (list x nil (1+ (caddr triple))))
             node-list))))
;
;
;
;triple-combine
;--------------
;
;arguments: L1 - a list of (NODE STATUS LENGTH) triples
;           L2 - a list of (NODE STATUS LENGTH) triples
;
;returns  : a list of (NODE STATUS LENGTH) triples
;
;description: For each distinct node N that appears in the union 
;   of L1 and L2, the function will include in the returned list
;
;   (a) the triple (N NIL LENGTH1) where LENGTH1 is the shortest 
;       path-length with a NIL status, if any; and
;   (b) the triple (N T LENGTH2) where LENGTH2 is the shortest
;       path-length with a T status, if any, which is also smaller    
;       than LENGTH1 mentioned in (a), if any.
;
;

(defun triple-combine (l1 l2)
 (let (t2 n2)
  (dolist (triple l1 l2)
   (cond ((eq (cadr triple) t)
          (cond ((setq t2
                    (find-if #'(lambda (x)
                                 (and (equal (car x) (car triple))    
                                      (eq (cadr x) t)))
                             l2))
                 (if (cl:< (caddr triple) (caddr t2))
                     (setq l2 (substitute triple t2 l2))))
                ((setq n2
                    (find-if #'(lambda (x)
                                 (and (equal (car x) (car triple))    
                                      (null (cadr x))))
                             l2))
                 (if (cl:< (caddr triple) (caddr n2))
                     (setq l2 (cons triple l2))))
                (t (setq l2 (cons triple l2)))))
         (t (cond
             ((setq n2
                (find-if #'(lambda (x)
                             (and (equal (car x) (car triple))
                                  (null (cadr x))))
                         l2))
              (cond
               ((cl:< (caddr triple) (caddr n2))
                (setq l2 (substitute triple n2 l2))
                (and (setq t2
                       (find-if
                         #'(lambda (x)
                              (and (equal (car x) (car triple))
                                   (eq (cadr x) t)))
                         l2))
                     (<= (caddr triple) (caddr t2))
                     (setq l2 (remove t2 l2 ))))))
            ((setq t2
               (find-if 
                  #'(lambda (x) (and (equal (car x) (car triple))    
                                     (eq (cadr x) t)))
                  l2))
             (if (<= (caddr triple) (caddr t2))
                 (setq l2 (substitute triple t2 l2))
                 (setq l2 (cons triple l2))))
            (t (setq l2 (cons triple l2)))))))))
;
;
;
;check-!
;-------
;
;argument: NODE-LIST - either a list of (NODE STATUS) pairs
;                      or a list of (NODE STATUS LENGTH) triples.
;   
;global variable used: FLAG
;
;returns : either a list of (NODE STATUS) pairs or a list of 
;          (NODE STATUS LENGTH) triples.
;     
;description: It implements the "!" feature in forward path-based    
;   inference.  If the global variable FLAG is 1, then NODE-LIST
;   is returned.  Otherwise a list containing all the elements in
;   NODE-LIST whose car is a top-level node is returned.
;
;

(defun check-! (node-list)
   (declare (special flag))
   (if (cl:= flag 1)
       node-list
       (mapcan #'(lambda (x) (if (isassert.n  (car x)) (list x)))
               node-list)))
;
;
;
;length-path-and
;---------------
;
;arguments: NODE - a list of triples in the form
;                  (NODE STATUS LENGTH).
;           PATH - a path in the form (and P1 P2 ... Pn)    
;
;returns  : a list of triples in the form (NODE STATUS LENGTH)    
; 
;description: It returns a list of nodes that can be reached by    
;   following PATH starting from some node in NODE.  For each 
;   distinct node N that the function length-path-and returns,
;   it will return 
;   (a) the triple (N NIL LENGTH1) where LENGTH1 is the shortest    
;       path-length with a NIL status, if any, and
;   (b) the triple (N T LENGTH2) where LENGTH2 is the shortest
;       path-length with a T status, if any, which is also 
;       smaller than LENGTH1 mentioned in (a), if any.
;
;

(defun length-path-and (node path)
   (do  ((path-list (cddr path) (cdr path-list))
         (int-list (length-path-infer node (cadr path))))
        ((or (null path-list) (null int-list)) int-list)
      (setq int-list
         (do  ((temp1 int-list (remove (caar temp1) (cdr temp1)
                                       :key #'car))
               (temp2 (length-path-infer node (car path-list)))
               (result) (n1) (t1) (n2) (t2) (l1))
              ((or (null temp1) (null temp2)) result)
           (setq l1 (find-quadruple (car temp1) (caar temp1)
                                    (cdr temp1) temp2))
           (setq n1 (cadr l1) t1 (caddr l1) n2 (cadddr l1)
                 t2 (fifth l1))
           (case (car l1)
              ((1 2 3))
              (4        (push (list (caar temp1) nil (min n1 n2))
                              result))
              (5        (push (list (caar temp1) t   (min n1 t2))
                              result))
              (6        (push (list (caar temp1) t   (min t1 n2))
                              result))
              ((7 9 11) (push (list (caar temp1) t   (min t1 t2))
                              result))
              (8        (push (list (caar temp1) nil (min n1 n2))
                              result)
                        (if (cl:< t1 n2)
                            (push (list (caar temp1) t t1)
                                  result)))
              (10       (push (list (caar temp1) nil (min n1 n2))
                              result)
                        (if (cl:< t2 n1)
                            (push (list (caar temp1) t t2)
                                  result)))
              (12       (setq result
                         (cons (list (caar temp1) nil (min n1 n2))   
                           (cons (list (caar temp1) t (min t1 t2))
                                 result)))))))))
;
;
;
;find-quadruple
;--------------
;
;arguments: TRIPLE - a (NODE STATUS LENGTH) triple
;           Z - a node
;           L1 - a list of (NODE STATUS LENGTH) triples
;           L2 - a list of (NODE STATUS LENGTH) triples
;
;returns  : a list in the form (NUMBER N1 T1 N2 T2) where NUMBER,
;   N1, T1, N2, T2 are non-negative integers.
;
;description: Let the argument TRIPLE be (NODE STATUS LENGTH).  If    
;   STATUS is NIL, then N1 in the returned list is LENGTH, and T1
;   is the path-length associated with the triple in L1 which has 
;   NODE as the first element and a status of T, if such a triple
;   exists.  If no such triple exists, the T1 is set to NIL.  If 
;   STATUS is T, then T1 is set to LENGTH, and N1 is set to the 
;   path-length associated with the triple in L1 which has NODE as    
;   the first element and a status of NIL, if such a triple
;   exists.  If no such triple exists, then N1 is set to NIL.  
;      N2 is the path-length associated with the triple in L2
;   which has Z as the first element and a status of NIL.  If no
;   such triple exists in L2, then N2 is set to NIL.
;      T2 is the path-length associated with the triple in L2 
;   which has Z as the first element and a status of T. If no such    
;   triple exists in L2, T2 is set to NIL.  
;      The element NUMBER in the returned list is:
;
;      1  if N1, N2, T2 = NIL and T1 <> NIL;
;      2  if T1, N2, T2 = NIL and N1 <> NIL;
;      3  if N2, T2 = NIL and N1, T1 <> NIL;
;      4  if T1, T2 = NIL and N1, N2 <> NIL;
;      5  if T1, N2 = NIL and N1, T2 <> NIL;
;      6  if N1, T2 = NIL and T1, N2 <> NIL;
;      7  if N1, N2 = NIL and T1, T2 <> NIL;
;      8  if T2 = NIL and N1, T1, N2 <> NIL;
;      9  if N2 = NIL and N1, T1, T2 <> NIL;
;      10 if T1 = NIL and N1, N2, T2 <> NIL;
;      11 if N1 = NIL and T1, N2, T2 <> NIL;
;      12 if N1, T1, N2, T2 <> NIL.
;
;   Finally the function find-quadruple puts NUMBER, N1, T1, N2,
;   T2 into a list and returns it.
;
;

(defun find-quadruple (triple z l1 l2)
 (let (number n1 t1 n2 t2)
  (cond
     ((cadr triple)
      (setq t1 (caddr triple))
      (if (setq n1 (cl::find (car triple) l1 :key #'car))
          (setq n1 (caddr n1))))
     (t (setq n1 (caddr triple))
        (if (setq t1 (cl::find (car triple) l1 :key #'car))
            (setq t1 (caddr t1)))))
  (if (setq n2 (find-if #'(lambda (x) (and (eq (car x) z)
                                           (null (cadr x))))
                        l2))
      (setq n2 (caddr n2)))
  (if (setq t2 (find-if #'(lambda (x) (and (eq (car x) z)
                                           (eq (cadr x) t)))
                        l2))
      (setq t2 (caddr t2)))
  (setq number
     (cond ((and (null n1)       t1  (null n2) (null t2))  1)
           ((and       n1  (null t1) (null n2) (null t2))  2)
           ((and       n1        t1  (null n2) (null t2))  3)
           ((and       n1  (null t1)       n2  (null t2))  4)
           ((and       n1  (null t1) (null n2)       t2)   5)
           ((and (null n1)       t1        n2  (null t2))  6)
           ((and (null n1)       t1  (null n2)       t2)   7)
           ((and       n1        t1        n2  (null t2))  8)
           ((and       n1        t1  (null n2)       t2)   9)
           ((and       n1  (null t1)       n2        t2)  10)
           ((and (null n1)       t1        n2        t2)  11)
           (t                                             12)))
  (list number n1 t1 n2 t2)))
;
;
;
;length-path-kstar
;-----------------
;
;arguments: NODE - a list of (NODE STATUS LENGTH) triples
;           PATH - a path in the form (kstar P)
;
;returns  : a list of (NODE STATUS LENGTH) triples
;
;description: It returns a list of nodes that can be reached by 
;   following PATH starting from some node in NODE.  For each
;   distinct node N that the function length-path-kstar returns,    
;   it will return
;   
;   (a) the triple (N NIL LENGTH1) where LENGTH1 is the shortest 
;       path-length with a NIL status, if any; and
;   (b) the triple (N T LENGTH2) where LENGTH2 is the shortest
;       path-length with a T status, if any, which is also smaller  
;       than LENGTH1 mentioned in (a), if any.
;
;

(defun length-path-kstar (node path)
 (do  ((found node) (path (cadr path)))
      ((null node) found)
   (do* ((l2 (length-path-infer node path))
	 (temp1 (mapcar #'(lambda (x) (car x)) found))
	 (new (mapcan #'(lambda (x)
				(if (not (member (car x) temp1))
				    (list x)))
		      l2))
	 (result) (temp2) (n1) (t1) (n2) (t2) (fn1) (ft1) 
	 (fn2) (ft2) (triple) (temp3))
        ((null found) (setq found (append result new))
                      (setq node (append temp2 new)))
      (setq triple (car found)
            found (cdr found)
            temp3 (find-quadruple triple (car triple) found l2)
            found (remove (car triple) found :key #'car)
            n1 (cadr temp3) t1 (caddr temp3) n2 (cadddr temp3)
            t2 (fifth temp3)
            fn1 (and n1 (list (car triple) nil n1))
            ft1 (and t1 (list (car triple) t t1))
            fn2 (and n2 (list (car triple) nil n2))
            ft2 (and t2 (list (car triple) t t2)))
      (case (car temp3)
        (1  (push ft1 result))
        (2  (push fn1 result))
        (3  (setq result (cons fn1 (cons ft1 result))))
        (4  (cond ((cl:< n2 n1) (push fn2 result) (push fn2 temp2))
                  (t (push fn1 result))))
        (5  (cond ((cl:< t2 n1) (push ft2 result) (push ft2 temp2))
                  (t (push fn1 result))))
        (6  (cond ((cl:< t1 n2) (push ft1 result))
                  (t (push fn2 result) (push fn2 temp2))))
        (7  (cond ((cl:< t2 t1) (push ft2 result) (push ft2 temp2))
                  (t (push ft1 result))))
        (8  (cond ((<= n2 t1) (push fn2 result) (push fn2 temp2))
                  (t (push ft1 result)
                     (cond ((cl:< n2 n1)
                            (push fn2 result)
                            (push fn2 temp2))
                           (t (push fn1 result))))))
        (9  (cond ((cl:< t2 t1) (push ft2 result) (push ft2 temp2))
                  (t (push ft1 result)))
            (push fn1 result))
        (10 (cond ((cl:< t2 n1)
                   (push ft2 result)
                   (push ft2 temp2)
                   (cond ((cl:< n2 n1)
                          (push fn2 result)
                          (push fn2 temp2))
                         (t (push fn1 result))))
                  (t (push fn1 result))))
        (11 (cond ((cl:< t2 t1) (push ft2 result) (push ft2 temp2))
                  (t (push ft1 result)))
            (push fn2 result)
            (push fn2 temp2))
        (12 (cond ((<= n1 t2)
                   (setq result (cons fn1 (cons ft1 result))))
                  (t (cond ((cl:< t2 t1)
                            (push ft2 result) 
                            (push ft2 temp2))
                           (t (push ft1 result)))
                     (cond ((cl:< n2 n1)
                            (push fn2 result)
                            (push fn2 temp2))
                           (t (push fn1 result ))))))))))
;
;
;
;length-path-exception
;---------------------
;
;arguments: NODE - a list of (NODE STATUS LENGTH) triples
;           PATH - a path of the form (exception P Q)
;
;returns  : a list of (NODE STATUS LENGTH) triples
;
;description: It returns a list of nodes that can be reached by 
;   following PATH starting from some node in NODE.  For each 
;   distinct node N that the function length-path-exception
;   returns, it will return
;
;   (a) the triple (N NIL LENGTH1) where LENGTH1 is the shortest
;       path-length with a NIL status, if any; and
;   (b) the triple (N T LENGTH2) where LENGTH2 is the shortest
;       path-length with a T status, if any, which is also smaller    
;       than LENGTH1 mentioned in (a), if any.
;
;

(defun length-path-exception (node path)
 (do* ((temp1
         (mapcan
           #'(lambda (x)
                (cond ((cadr x) (list (list (car x) (cadr x) 0)))
                      ((find-if #'(lambda (y) 
                                      (and (equal (car y) (car x)) 
                                           (eq (cadr y) t)))
                                node)
                       nil)
                      (t (list (list (car x) (cadr x) 0)))))
           node))
       (l1 (length-path-infer temp1 (cadr path)))
       (l2 (length-path-infer temp1 (caddr path)))
       (result) (n1) (t1) (n2) (t2) (fn1) (ft1) (triple) (temp2))
      ((null l1) result)
   (setq triple (car l1)
         l1 (cdr l1)
         temp2 (find-quadruple triple (car triple) l1 l2)
         l1 (remove (car triple) l1 :key #'car)
         n1 (cadr temp2) t1 (caddr temp2) n2 (cadddr temp2)
         t2 (fifth temp2)
         fn1 (and n1 (list (car triple) nil n1))
         ft1 (and t1 (list (car triple) t t1)))
   (case (car temp2)
     (1      (push ft1 result))
     (2      (push fn1 result))
     (3      (setq result (cons fn1 (cons ft1 result))))
     (4      (if (cl:> n2 n1) (push fn1 result)))
     ((5 10) (if (cl:> t2 n1) (push fn1 result)))
     (6      (if (cl:> n2 t1) (push ft1 result)))
     ((7 11) (if (cl:> t2 t1) (push ft1 result)))
     (8      (when (cl:> n2 t1) (push ft1 result)
                             (if (cl:< n1 n2) (push fn1 result))))
     (9      (if (cl:> t2 t1)
                 (setq result (cons fn1 (cons ft1 result)))))
     (12     (when (cl:> t2 t1) (push ft1 result)
                             (if (cl:< n1 n2) (push fn1 result)))))))    
;
;
;
;length-path-domain
;------------------
;
;arguments: NODE - a list of (NODE STATUS LENGTH) triples
;           PATH - a path in the form (domain-restrict (Q z) P)
;
;returns  : a list of (NODE STATUS LENGTH) triples
;
;description: It returns a list of nodes that can be reached by
;   following PATH starting from some node in NODE.  For each 
;   distinct node N that the function length-path-domain returns,    
;   it will return
;
;   (a) the triple (N NIL LENGTH1) where LENGTH1 is the shortest
;       path-length with a NIL status, if any; and
;   (b) the triple (N T LENGTH2) where LENGTH2 is the shortest
;       path-length with a T status, if any, which is also smaller    
;       than LENGTH1 mentioned in (a), if any.
;
;

(defun length-path-domain (node path)
 (do  ((result) (triple) (temp1))
      ((null node) (length-path-infer result (caddr path)))
   (setq triple (car node) node (cdr node))
   (if (setq temp1 (length-path-infer
                      (list (list (car triple) nil 0))
                      (caadr path)))
       (setq result
         (append result (length-dr-restrict (car triple)
                           (find-quadruple triple (node (cadadr path))
                                           node temp1)))))
   (setq node (remove (car triple) node :key #'car))))
;
;
;
;length-dr-restrict
;------------------
;
;arguments: 
;   NODE - a node
;   L1 - a list in the form (NUMBER N1 T1 N2 T2) where NUMBER, N1,
;        T1, N2, T2 are non-negative integers.
;
;returns  : a list of (NODE STATUS LENGTH) triples
;
;description: This function is called by length-path-domain and 
;   length-path-range.  It checks if NODE satisfies the
;   restriction imposed by the domain-restrict or range-restrict
;   feature in the syntax of path formation.  The elements N1 and     
;   T1 in L1 are the shortest path-length of NODE with a NIL and T    
;   status respectively.  This function returns a list of
;   (NODE STATUS LENGTH) if NODE satisfies the restriction.
;
;

(defun length-dr-restrict (node l1)
   (let ((fn1 (list node nil (cadr l1)))
         (ft1 (list node t (caddr l1))))
      (case (car l1)
         ((1 2 3))
         ((6 7 9 11) (list ft1))
         ((8 12)     (list fn1 ft1))
         (5          (list (list node t (cadr l1))))
         ((4 10)     (list fn1)))))
;
;
;
;length-path-range
;-----------------
;
;arguments: NODE - a list of (NODE STATUS LENGTH) triples
;           PATH - a path in the form (range-restrict P (Q z))
;
;returns  : a list of (NODE STATUS LENGTH) triples
;
;description: It returns a list of nodes that can be reached by
;   following PATH starting form some node in NODE.  For each
;   distinct node N that the function length-path-range returns,     
;   it will return
;  
;   (a) the triple (N NIL LENGTH1) where LENGTH1 is the shortest
;       path-length with a NIL status, if any; and
;   (b) the triple (N T LENGTH2) where LENGTH2 is the shortest
;       path-length with a T status, if any, which is also smaller
;       than LENGTH1 mentioned in (a), if any.  
;
;

(defun length-path-range (node path)
 (do ((result) (triple) (temp1) 
      (node (length-path-infer node (cadr path))
            (remove (car triple) node :key #'car)))
     ((null node) result)
   (setq triple (car node) node (cdr node))
   (if (setq temp1 (length-path-infer
                      (list (list (car triple) nil 0))
                      (caaddr path)))
       (setq result (append result
                      (length-dr-restrict (car triple) 
                        (find-quadruple triple (node (cadr (caddr path)))
                                        node temp1)))))))
;
;                                                
;
;     
;
;
;var
;---
;
;argument: X - an s-expression
;
;returns : X if X is a variable node and nil otherwise.
;
;description : It checks if X is a variable node.
;
;

(defun var (x)
   (cond ((numberp x) nil)
         ((atom x) (cond ((typep x 'node) (cond ((isvar.n x) x)))
                          (t (cond ((isvar.n (node x)) x)))))
         ((eq (car x) '?) x)))
;
;
;convt-path
;----------
;
;argument:
;   PATH - a path conforming to the syntax given in the SNePS user    
;          manual.
;
;returns : the converse of PATH
;
;description: It takes a path as argument and returns the converse    
;   of that path.  The converse of a null path is assumed to be
;   nil.
;
;

(defun convt-path (path)
  (cond
   ((null path) nil)
   ((atom path) (convt path))
   ((null (rest path)) (convt (first path)))
   ((eq (car path) 'compose)
    (cons 'compose (mapcar #'convt-path (reverse (cdr path)))))
   ((eq (car path) 'converse)
    (cons 'converse (list (convt-path (cadr path)))))
   ((member (car path) '(kstar kplus))
    (cons (car path) (mapcar #'convt-path (reverse (cdr path)))))
   ((eq (car path) 'or)
    (cons 'or (mapcar #'(lambda (x) (convt-path x))
		      (cdr path))))
   ((eq (car path) 'and) (cons 'and
			       (mapcar #'(lambda (x) (convt-path x))    
				       (cdr path))))
   ((eq (car path) 'not) (list 'not (convt-path (cadr path))))
   ((eq (car path) 'relative-complement)
    (list 'relative-complement (convt-path (cadr path))
	  (convt-path (caddr path))))
   ((eq (car path) 'irreflexive-restrict)
    (list 'irreflexive-restrict (convt-path (cadr path))))
   ((eq (car path) 'exception)
    (list 'exception (convt-path (cadr path)) (convt-path
					       (caddr path))))
   ((eq (car path) 'domain-restrict)
    (list 'range-restrict (convt-path (caddr path)) 
	  (list (caadr path) (cadr (cadr path)))))
   ((eq (car path) 'range-restrict)
    (list 'domain-restrict 
	  (list (caaddr path) (cadr (caddr path)))
	  (convt-path (cadr path))))
   (t (cons 'compose (mapcar #'convt-path (reverse path))))))



    
    




