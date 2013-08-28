;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1994--2013
;; Research Foundation of State University of New York

;; Version: $Id: ptree.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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




(in-package :snip)


(defun ptree-for-and-ent (ants)
  "Build a P-tree for an and-entailment rule from
   its antecedent patterns."
  (build-ptree ants))

(defun ptree-for-num-quant (ants cqs)
  "Build a P-tree for a numerical quantifier rule.
   Unlike an and-entailment rule, both antecedents and consequents
   should be included in P-tree construction.
   A sub-P-tree is built for antecedents patterns, 
   another sub-P-tree is built for consequent patterns, and
   then those two sub-P-trees are combined to make a P-tree."
  (list (build-ptree ants)
	(build-ptree cqs)))

    
(defun build-ptree (pat-list)
  "Build a P-tree from an antecedent pattern list.
   Basically, it calls four sub-functions: get-patvar-list,
   patvar-to-varpat, varpat-to-patseq, and patseq-to-ptree."
  (let* ((patvar-list (get-patvar-list pat-list))
	 (varpat-list (patvar-to-varpat patvar-list))
	 (pat-seq (varpat-to-patseq pat-list patvar-list varpat-list))
	 (ptree (patseq-to-ptree pat-seq patvar-list)))
    ptree))
	
(defun get-patvar-list (pat-list)
  "Associate a pattern with its variables.
   For example, for antecedents P(x,y), Q(y,z), and R(x,z),
   the input is (P Q R), and the output is ((P x y) (Q y z) (R x z))."
  (let ((patvar-list nil))
    (dolist (pat pat-list)
      (setq patvar-list 
	    (insert-last patvar-list (cons pat (sneps::all-vars.n pat)))))
    patvar-list))

(defun patvar-to-varpat (patvar-list)
  "Convert a list, in which each pattern is associated with variables
   that it contains, to a list, in which each variable is associated
   with patterns that it is contained.
   For example, ((P x y) (Q y z) (R x z)) is converted to
   ((x P R) (y P Q) (z Q R))."
  (let ((varpat-list nil))
    (dolist (patvar patvar-list)
      (let ((pat (first patvar))
	    (var-list (rest patvar)))
	(dolist (var var-list)
	   ; if 'var-list' already has an entry for 'var',
	   ; current 'pat' is added to that entry,
	   ; otherwise, build a new (var pat) entry to 'var-list'
	  (if (null (assoc var varpat-list))
	      (setq varpat-list (insert-last varpat-list (list var pat)))
	      (rplacd (assoc var varpat-list)
		      (union (rest (assoc var varpat-list)) (list pat)))))))
    varpat-list))



(defun varpat-to-patseq (pat-list patvar-list varpat-list)
  "Get a sequence of patterns from a variable-pattern list
   so that those patterns that have shared variables are adjacent
   in the sequence.
   For example, a 'varpat-list' ((x P R) (y P Q) (z Q R))
   produces a 'patseq' (P R Q)."
  (let ((pat-seq nil)
	(var-union nil) ; variables that the patterns in 'pat-seq' have
	(var-temp nil) ; intersection of 'unprocessed-vars' and 'var-union'
	(unprocessed-vars (mapcar #'first varpat-list)))
    (do 
     ((current-var (car unprocessed-vars) (car unprocessed-vars)))
     ((or (= (length pat-list) (length pat-seq)) (null unprocessed-vars)))
     ; add patterns associated with the current variable in 'varpat-list' 
     ; to 'pat-seq' only when each pattern is not already in 'pat-seq'.
     ; e.g., if pat-seq = (P Q S), and the current variable has a list
     ; of patterns (Q S R T), then the resulting 'pat-seq' = (P Q S R T).
      (dolist (pat (rest (assoc current-var varpat-list)))
	(when (null (member pat pat-seq))
	  (setq var-union (union var-union (rest (assoc pat patvar-list))))
	  (setq pat-seq (insert-last pat-seq pat))))
       ; get the next unprocessed variable.
       ; if 'unprocessed-vars' does not have any variables in 'var-union',
       ; the first variable in 'unprocessed-vars' will be processed.
       ; if 'unprocessed-vars' do have variables in 'var-union', choose
       ; any one of them as the next unprocessed variable.
       ; e.g., if 'unprocessed-vars' = (y w), and 'var-union' = (x z w v),
       ;       then 'w' will be the next variable to be processed by
       ;       changing 'unprocessed-vars' to (w y).
      (setq var-temp (intersection (cdr unprocessed-vars) var-union))
      (setq unprocessed-vars
	    (if (null var-temp)
		(cdr unprocessed-vars)
		(cons (first var-temp)
		      (remove (first var-temp) (cdr unprocessed-vars))))))
     ; those elements in pat-list that are not in pat-seq
     ; are appended at the end of pat-seq
    (if (= (length pat-list) (length pat-seq))
	pat-seq
	(append pat-seq
		(set-difference pat-list pat-seq)))))


(defun patseq-to-ptree (pat-seq patvar-list)
  "Build a P-tree from a sequence of patterns.
   For example, from the antecedents P(x,y), Q(y,z), and R(x,z),
         'patvar-list' = ((P x y) (Q y z) (R x z))
         'pat-seq' = (P R Q)
         'ptree' = ((P R) Q)."
  (do ((ptree pat-seq)
       (parent-node nil)
       (ptree-patvar-list patvar-list))
      ((or (null ptree) (= (length ptree) 1)) (first ptree))
    (do* ((cur-ptree ptree)
	  (temp-ptree nil)
	  (first-node (first cur-ptree) (first cur-ptree))
	  (sec-node (second cur-ptree) (second cur-ptree)))
	 ((null cur-ptree) (setq ptree temp-ptree))
	  (cond
	  ; if the next two subtrees in the intermediate ptree have
	  ; shared variables, make them adjacent nodes in the ptree
	   ((intersection (rest (assoc first-node
				       ptree-patvar-list
				       :test 'equal))
			  (rest (assoc sec-node
				       ptree-patvar-list
				       :test 'equal)))
	    (setq parent-node (list first-node sec-node))
	    (setq temp-ptree (insert-last temp-ptree parent-node))
	    (setq ptree-patvar-list
		  (acons parent-node
			 (union
			  (rest (assoc first-node
				       ptree-patvar-list
				       :test 'equal))
			  (rest (assoc sec-node
				       ptree-patvar-list
				       :test 'equal)))
			 ptree-patvar-list))
	    (setq cur-ptree (rest (rest cur-ptree))))
	  ; if the next two subtrees in the intermediate ptree have
	  ; no shared variables, adjoin the first subtree with the last
	  ; subtree in the ptree built so far.
	   (t
	    (let ((prev-last-node (first (last temp-ptree))))
	      (cond
		((null prev-last-node)
		 (setq temp-ptree (list first-node)))
		(t (setq parent-node (list prev-last-node first-node))
		   (setq temp-ptree  (append (butlast temp-ptree)
					     (list parent-node)))
		   (setq ptree-patvar-list
			 (acons parent-node
				(union
				 (rest (assoc first-node
					      ptree-patvar-list
					      :test 'equal))
				 (rest (assoc prev-last-node
					      ptree-patvar-list
					      :test 'equal)))
				ptree-patvar-list)))))
	    (setq cur-ptree (rest cur-ptree)))))))


(defun ptree-to-adj-list (ptree)
  "If ptree contains only 1 pattern P, return ((P nil)),
   otherwise call get-adj-node-list."
  (if (<= (length ptree) 1)
      (list (list ptree nil))
      (get-adj-node-list ptree)))

(defun get-adj-node-list (ptree)
  "Get a list of adjacent node pairs from a P-tree which
   is a binary tree.
   For example,   ptree = (((a b) (c d)) ((e f) g))
                  adj-node-list = ((a b)
                                   (c d)
                                   (e f)
                                   ((a b) (c d))
                                   ((e f) g)
                                   (((a b) (c d)) ((e f) g)))."
  (let ((first-node (first ptree))
	(sec-node (second ptree)))
    (cond ((and (atom first-node) (atom sec-node))
	   (list (list first-node sec-node)))
	  ((atom first-node)
	   (insert-last (get-adj-node-list sec-node)
			(list first-node sec-node)))
	  ((atom sec-node)
	   (insert-last (get-adj-node-list first-node)
			(list first-node sec-node)))
	  (t
	   (insert-last
	    (append (get-adj-node-list first-node)
		    (get-adj-node-list sec-node))
	    (list first-node sec-node))))))


(defun get-rule-use-info-ptree (report cqch)
  "Modified 'get-rule-use-info' for the P-tree method.
   Returns the set of rule-use-infos which are compatible with
   the reported instance.
   Data structure of cqch for P-tree looks like:
    <cqch> = (<channel> <antecedents> <ptree-ruiset>)
    <ptree-ruiset> = (<ptree> <adj-node-list> <pnode-ruiset>)
    <adj-node-list> = ((<pnode> <pnode>) (<pnode> <pnode>) ....)
        adj-node-list contains adjacent ptree node pairs
    <pnode-ruiset> = ((<pnode> <ruis>) (<pnode> <ruis>) ....)
        in node-ruiset, each ptree node is associated with its
        corresponding ruiset."
  (declare (special *RULE-USE-CHANNELS*))
  (let* ((ch (channel.cqch cqch))
	 (ants (ants.cqch cqch))
	 (ptree-ruiset (ruiset.cqch cqch))
	 (ptree (first ptree-ruiset))
	 (adj-node-list (second ptree-ruiset))
	 (pnode-ruiset (third ptree-ruiset))
	 (sign (sign.rep report))
	 (ant (signature.rep report))
	 (updated-ruis
	    (if (or (member ant ants) (null ants))
		(makeone.ruis
		  (make.rui (subst.rep report)
		            (if (eq sign 'POS) 1 0)
			    (if (eq sign 'NEG) 1 0)
			    (update.fns (nodeset-to-fnodeset (list ant))
					ant 
					(support.rep report)
					(if (eq sign 'POS) 'true 'false))
			    nil))
	      nil))
	 (result updated-ruis)
	 (cur-node ant))
    (do*
      ; parent-node of cur-node is a pair in adj-node-list
      ; where the first node or the second node is equal to cur-node.
      ; e.g., adj-node-list = ((a b) (c d) ((a b) (c d))),
      ;       cur-node = c ->  parent-node = (c d)
      ;       cur-node = b ->  parent-node = (a b)
     ((parent-node (or (assoc cur-node adj-node-list :test 'equal)
		       (rassoc (list cur-node) adj-node-list :test 'equal))
		   (or (assoc cur-node adj-node-list :test 'equal)
		       (rassoc (list cur-node) adj-node-list :test 'equal)))
      
      ; adjacent node can be obtained by removing cur-node
      ; from parent node.
      ; e.g., parent-node = (a b),
      ;       cur-node = a  ->  adj-node = b
      ;       cur-node = b  ->  adj-node = a
      (adj-node (if (equal cur-node (first parent-node))
		    (second parent-node)
		    (first parent-node))
		(if (equal cur-node (first parent-node))
		    (second parent-node)
		    (first parent-node))))
     ((null updated-ruis))
      ; pnode-ruiset is an association list of (<node> <ruiset>) pairs.
      ; current node's ruiset is updated with new ruiset in 'updated-ruis'
      (if (null (assoc cur-node pnode-ruiset :test 'equal))
	  (setq pnode-ruiset (acons cur-node
				   updated-ruis
				   pnode-ruiset))
	  (rplacd (assoc cur-node pnode-ruiset :test 'equal)
		  (update.ruis-set updated-ruis
				   (rest (assoc cur-node
						pnode-ruiset
						:test 'equal)))))
      ; get compatible ruis between cur-node and its adjacent node
      (setq updated-ruis
	    (get-compatible-ruis updated-ruis
				 (rest (assoc adj-node
					      pnode-ruiset
					      :test 'equal))))
      ; among compatible ruis, only those that are not already in
      ; parent node's ruiset are added
      (setq updated-ruis 
	    (get-new-ruis updated-ruis
			  (rest (assoc parent-node
				       pnode-ruiset
				       :test 'equal))))
      (setq result (append result updated-ruis))
      (setq cur-node parent-node))
    ; update the *rule-use-channels* register with new pnode-ruiset
    (setq *RULE-USE-CHANNELS*
	  (update.cqchset
	   (make.cqch ch ants (list ptree adj-node-list pnode-ruiset))
	   *RULE-USE-CHANNELS*))
    result))


(defun update.ruis-set (ruis1 ruis2)
  "Do 'update.ruis' for each RUI in a RUI set
   with respect to another RUI set."
  (let ((updated-ruis ruis2))
    (do.set (rui ruis1)
       (setq updated-ruis (update.ruis rui updated-ruis)))
    updated-ruis))


(defun get-compatible-ruis (ruis1 ruis2)
  "Get compatible RUIs between two RUI sets."
  (let ((compatible-ruis nil))
    ; only consider the case when both ruis1 and ruis2 are not empty
    (when (and ruis1 ruis2)
      (do.set (rui1 ruis1)
        (do.set (rui2 ruis2)
	  (when (match::is-compatible.sbst (subst.rui rui1)
					   (subst.rui rui2))
		(setq compatible-ruis
		   (update.ruis
		      (make.rui
			   (union.sbst (subst.rui rui1) (subst.rui rui2))
			   (+ (poscount.rui rui1) (poscount.rui rui2))
			   (+ (negcount.rui rui1) (negcount.rui rui2))
			   (append (fns.rui rui1) (fns.rui rui2))
			   (remarkedp.rui rui2))
		      compatible-ruis))))))
    compatible-ruis))


(defun get-new-ruis (ruis1 ruis2)
  "Get RUIs in the first RUI set that is not in the second RUI set."
  (let ((new-ruis (new.ruis)))
    (do.set (rui ruis1)
	(unless (can-merge rui ruis2)
	  (setq new-ruis (putin.ruis rui new-ruis))))
    new-ruis))


(defun can-merge (rui ruis)
  "An RUI can be merged to a RUI set if there is an RUI in the RUI
   set that has the same substitution and the flag in 'fns' is
   not contradictory."
  (let ((can-merge-flag nil))
    (do.set (rui2 ruis)
       (if (and (iseq.sbst (subst.rui rui)
			   (subst.rui rui2))
		(compatible.rui rui rui2))
	   (setq can-merge-flag t)))
    can-merge-flag))
	 

(defun insert-last (list element)
  "Insert an element at the end of a list."
  (append list (list element)))




    
    




