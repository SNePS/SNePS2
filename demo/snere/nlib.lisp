;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package:SNEPSUL; Base:10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: nlib.lisp,v 1.1 2011/05/24 17:59:36 mwk3 Exp $

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

(in-package :snepsul)


;; Function s associated with variables and reference.

(defvar *variable-class-hashtable* nil
  "Contains all the variables, and their class.")

(defvar *current-variables-list* nil
  "Just a list of all the currently mentioned variables.")

(defvar *temporary-variable-nodes* nil
  "A list of transient (i. e., just during sentence processing) nodes.")

(defvar *variable-ref-nodes* nil
  "A list of already generated variable nodes, used to decide on 'a' or 'the'.")

(defun get-variables ()
   (return-all-mentioned-variables))

(defun clear-variables ()
  (setq *current-variables-list* nil)
  (reset-references)
  (if *temporary-variable-nodes*
      (eval `(sneps::silent-erase ,*temporary-variable-nodes*)))
  (setq *temporary-variable-nodes* nil)
)

(defun erase-node (n)
  (declare (special sneps::outunit))
  (cond ((not (sneps::is.n n)) 
	 (princ (format nil "~A ,node  not found or already erased. ~%" n)))
	((sneps::is-isolated n)
	 (dismantle-node n)
	 (if (sneps::isassert.n n)
	     (sneps::set.sv 'sneps::assertions
			    (sneps::remove.ns n (sneps::value.sv 'sneps::assertions)))))
	(t (princ (format nil "~A not isolated. Can't be erased. ~%" n)))))

(defun dismantle-node (n)
  (declare (special sneps::erased-ns))  
  (sneps::set.sv 'sneps::nodes (sneps::remove.ns n (sneps::value.sv 'sneps::nodes)))
  (if (sneps::isvar.n n)
    (sneps::set.sv 'sneps::varnodes (sneps::remove.ns n (sneps::value.sv 'sneps::varnodes))))
  (setf sneps::erased-ns (nconc sneps::erased-ns (list n)))
  (remprop (sneps::nodeaccess n) 'sneps::=snode)
  (sneps::do.fcs (sneps::r sneps::ns (sneps::node-fcableset n) nil) (sneps::erase-cable n sneps::r sneps::ns)))

;(defmacro erase (&rest nsf)
 ; `(let (sneps::erased-ns)
;     (declare (special sneps::erased-ns))
;     (mapc #'erase-node (sneps::nseval ',nsf))
;     (sneps::set.sv 'sneps::variables
;	     (sneps::clean-svars (sneps::value.sv 'sneps::variables) sneps::erased-ns))
;     (values)))

(defun reset-all ()
  (setq
    *variable-class-hashtable* nil
    *current-variables-list* nil
    *temporary-variable-nodes* nil))

(reset-all)

(defun new-or-reuse-varnode (class)
  "Returns a varnode. Can reuse varnodes from previous sentences, or just create new varnodes, optionally."
  (if (and (boundp '*use-new-variables*)
	   *use-new-variables*)
      (flatten ($ (gensym class)))
      (let
	((free-vars
	   (set-difference (gethash class *variable-class-hashtable*)
			   *current-variables-list*
			   :test #'equal)))
	(if free-vars
	    (car free-vars)
	    (flatten ($ (gensym class)))))))

(defun new-variable (class)
  "Creates or reuses a variable node, adding default properties as necessary."
  (let 
    ((varlist (gethash class
		       (if (null *variable-class-hashtable*)
			   (setq *variable-class-hashtable* (make-hash-table :test #'equal))
			    *variable-class-hashtable*)))
     (newvarnode (new-or-reuse-varnode class)))
    (if (not (member newvarnode varlist :test #'eq))
	(setq varlist (append varlist (list newvarnode))))
;    (setq varlist (adjoin newvarnode varlist :test #'eq))
    (if (not (member newvarnode *current-variables-list* :test #'eq))
	(setq *current-variables-list* (cons newvarnode *current-variables-list*)))
    (setf (gethash class *variable-class-hashtable*) varlist)
    (add-default-properties class newvarnode)
; *** Change (commented out)
;    (add-class newvarnode class)
    newvarnode
    ))

(defun add-default-properties (class varnode)
  "Adds some default properties to a variable node."
#|
  (collect-temporary-node
    (eval
      `(build object ,varnode
	      property ,(nth (1- (length (return-all-mentioned-variables)))
				    '("first" "second" "third" "fourth" "fifth" "sixth")))))
|#
  (collect-temporary-node
    (eval
      `(build object ,varnode
	      property ,(format nil "~:R" (length (return-all-mentioned-variables))))))
; *** New stuff
  (if class
      (collect-temporary-node
	(eval
	  `(build member ,varnode
		  class ,(string class)))))
; *** End of new stuff.
  (if (equal (length (return-all-mentioned-variables)) 1)
      (collect-temporary-node
	(eval
	  `(build object ,varnode
		  property  "former"))))
  (if (cl:> (length (return-all-mentioned-variables)) 2)
      (erase-temporary-nodes
	(eval `(find object ,(return-all-mentioned-variables)
		     property ("latter" "former"))))
      (let ((oldlatter (eval `(find object ,(return-all-mentioned-variables)
				    property "latter"))))
	(if oldlatter
	    (erase-temporary-node oldlatter))
	(collect-temporary-node
	  (eval
	    `(build object ,varnode
		    property "latter")))))
)

(defun collect-temporary-node (vnode)
  "Collects a list of temporary nodes for later erasure."
  (if (boundp '*temporary-variable-nodes*)
      (setq  *temporary-variable-nodes* (adjoin (flatten vnode) *temporary-variable-nodes* :test #'eq))
      (progn
	(defvar *temporary-variable-nodes* nil "List of temporary properties of variables.")
	(setq  *temporary-variable-nodes* (adjoin (flatten vnode) *temporary-variable-nodes* :test #'eq)))))

(defun get-temporary-nodes ()
  *temporary-variable-nodes*)

(defun erase-temporary-node (vnode)
  "Erases a temporary node."
  (if (member (flatten vnode) *temporary-variable-nodes* :test 'equal)
      (eval `(sneps::silent-erase ,vnode))))

(defun erase-temporary-nodes (nodes)
  (mapcar #'erase-temporary-node nodes))

(defun add-properties (varnode properties)
  "Adds presumably permenant properties to variable nodes."
  (dolist (prop properties)
    (eval
      `(build object ,varnode
	      property (build lex ,prop)))))

(defun add-class (varnode class)
  "Adds the member-class for the varnode."
  (eval
    `(build member ,varnode

	    class ,class)))

(defun get-variable-properties (varnode)
  "Returns a nodeset of all properties attached to the varnode."
  (eval
    `(find property- (find object ,varnode))))

(defun find-variables (class properties)
  "Returns a nodeset of the variables with specified member-class & object-properties."
  (if (null properties)
      (eval
	`(find member- (find member ,(return-all-mentioned-variables)
				    class ,class)))
      (if (equal (length (setq properties (flistify properties))) 1)
	  (eval
	    `(find object-
		   (find object
			 (find member-
			       (find class ,class))
			 property ,(car properties))))
	  (eval
	    `(& 
	       (find object-
		     (find object
			   (find member-
				 (find class ,class))
			   property ,(car properties)))
	       ,(find-variables class (cdr properties))))
	  )
      )
)

(defun return-all-mentioned-variables ()
  "Returns a list of all variables created while processing the current sentence."
  (reverse *current-variables-list*))

(defun add-types-to-context (nodes)
  "Adds variable typing to temporary context, these are extracted from the &ant entailment, and deletes
them from the &ant list (so they are not generated)."
  (remove-if #'add-types nodes))

(defun add-types (node)
  "Extracts the type (MEMBER-CLASS) information, and adds it to the temporary context."
  (if (has-path 'member node)
      (collect-temporary-node
	(eval
	  `(build member ,(has-path 'member node)
		  class ,(has-path '(class lex) node))))
      nil))

(defun add-temporary-properties (node)
  "Adds temporary OBJECT-PROPERTY nodes for stuff like 'former' and 'latter' for generated nodes."
  (if (not (member node *current-variables-list* :test #'eq))
	(setq *current-variables-list* (cons node *current-variables-list*)))
  (add-default-properties nil node))

;;;
;;; Functions associated with generation of an complex NP category, in context.
;;; 

(defun reset-references ()
  (setq *variable-ref-nodes* nil))

(defun choose-prenoun (node)
  "Choose the pre-noun component of the noun phrase."
  (if (member node *variable-ref-nodes*)
      (list (choose-det node) (choose-properties node))
      (choose-det node)))

(defun choose-det (node)
  (if (member node *variable-ref-nodes*)
      "the"
      (if (member
	    node
	    *variable-ref-nodes*
	    :test #'(lambda (x y)
		      (eq (has-path 'class
				    (has-path 'member- x *temporary-variable-nodes*))
			  (has-path 'class
				    (has-path 'member- y *temporary-variable-nodes*)))))
	  (progn
	    (setq *variable-ref-nodes* (cons node *variable-ref-nodes*))
	    "another")
	  (progn
	    (setq *variable-ref-nodes* (cons node *variable-ref-nodes*))
	    "a")
	  )))

(defun choose-class (node)
  "Choose a suitable class type to identify a node. Currently 
done arbitrarily."
  (let (;; These have lex arcs
	(network-candidates (eval `(find member ,node)))
	;; these don't
	(temp-candidates (get-temporary-nodes)))
    ;; Useful trace for debugging!!!
    ;;(eval `(full-describe ,network-candidates))
    ;;(eval `(full-describe ,temp-candidates))
    (get-possible-lex
     (first-atom
      (eval `(find class- (& ,network-candidates ,temp-candidates)))))))

(defun get-possible-lex (node)
  (if (has-path '(lex) node)
      (has-path '(lex) node)
      node))

(defun order-properties (propnodes)
  "Supposedly, orders properties in order of preference. Doesn't do anything right now."
  (if (member (SNEPS::node "latter") propnodes)
      (setq propnodes (cons (SNEPS::node "latter") propnodes)))
  (if (member (SNEPS::node "former") propnodes)
      (setq propnodes (cons (SNEPS::node "former") propnodes)))
  propnodes)

(defun choose-properties (node)
  "Chooses a minimally specifying (in the sentence context) subset of the PROPERTYs associated with the node."
  (let
    ((props
       (order-properties
	 (eval
	   `(find property-
		  (& ,(get-temporary-nodes)
			    (find object ,node))))))

     (other-nodes
       (eval
	`(& ,(return-all-mentioned-variables)
		    (sneps::- (find (member- class) ,(choose-class node))
			      (,node)))))
     (temp nil)
    )
    (if other-nodes
	(progn
	  (setq temp (dolist (ps props)
		       (if (null (eval
				   `(find object ,other-nodes
					  property ,ps)))
			   (return ps))))
	  (if temp
	      temp
	      props))
;	      (get-lex temp)
;	      (get-lex props)))
	temp)
    ))

(defun get-lex (nodes)
  (if (listp nodes)
      (mapcar #'get-lex nodes)
      (eval `(find lex- ,nodes))))

;;;
;;; Remaining functions are used by the parser & grammar.
;;; 

(defun new-sneps-var ()
  ($ 'x))

(defun add-indef (phrase)
  "Adds a 'a' or 'an' to a NP depending on if it starts with a vowel. Used in generation."
  (cons 
    (if (member (if (listp phrase)
		    (char (car phrase) 0)
		    (char phrase 0))
		'(#\a #\e #\i #\o #\u))
	"an"
	"a")
    (if (atom phrase)
	(list phrase)
	phrase)))

(defun mylength (s)
  "Returns 0 if s is NIL, 1 if s is a non-NIL atom, and the length of s otherwise."
  (cond
    ((null s) 0)
    ((atom s) 1)
    (t (length s))))


(defun pretty-print-response (response &optional (line-length 70) &key (output t))
  "Cleans up the response from the generation component and prints it."
  (let ((output-line (if output
			 "~%~%"
			 ""))
	(ll 0)
	)
    (do* ((resp (mapcar #'princ-to-string response) (rest resp))
	  (wrd (first resp) (first resp)))
	 ((null resp) output-line)
      (if (member (second resp) '("," ";" "." "!") :test #'string-equal)
	  (setq
	    output-line (concatenate 'string output-line " " wrd (second resp))
	    ll (cl::+ ll (length wrd) 1)
	    resp (rest resp))
	  (setq
	    output-line (concatenate 'string output-line " " wrd)
	    ll (cl::+ ll (length wrd) 1)))
      (if (cl::> ll line-length)
	  (setq output-line (concatenate 'string output-line "~%")
		ll 0)))
    (setq output-line (concatenate 'string output-line ".~%"))
    (if output
	(format t output-line)
	output-line)))

(defun has-path (path node &optional (restriction-nodes nil))
  "Returns t iff there is a path in node."
  (if restriction-nodes
      (first-atom (intersection (SNEPS::pathfrom (flistify path) node) restriction-nodes))
      (first-atom (SNEPS::pathfrom (flistify path) node))))

(defun build-type-list (nodes)
  "Builds a nodeset of MEMBER-CLASS nodes for objects in the current context."
  (mapcar
    #'(lambda (nde)
	(eval `(build member ,nde class (build lex ,(choose-class nde)))))
    (flistify (flatten nodes))))

(defun build-and-ent-rule (ants cqs)
  (let* ((allvars (return-all-mentioned-variables))
	 (allants (append
		    (build-type-list allvars) ants)))
    (eval
      `(build
	 forall ,allvars
	 ,(select-ant allants) ,allants
	 cq ,cqs))))

(defun select-ant (ants)
  "Returns ant or &ant depending on number of ants."
  (if (cl::> (mylength ants) 1)
      '&ant
      'ant))

(defun build-rule-structure (act effect &optional (ants nil))
  "Builds the act-effect caseframe, allows optional antecedents."
  (let* ((allvars (return-all-mentioned-variables))
	 (allants (append
		    (build-type-list allvars) ants)))
    (eval
      `(build
	 forall ,allvars
	 ,(select-ant allants) ,allants
	 cq (build
		     act ,act
		     effect ,effect))))
)

(defun build-act-precondition-structure (act precondition &optional (ants nil))
  "Builds the act-precondition caseframe."
  (let* ((allvars (return-all-mentioned-variables))
	 (allants (build-type-list (return-all-mentioned-variables))))
    (eval
      `(build
	 forall ,allvars
	 ,(select-ant allants) ,allants
	 cq (build
		     act ,act
		     precondition ,precondition))))
)

(defun build-action-structure (action objects)
  "Builds the action-objects caseframe."
  (let ((objs (flistify objects)))
    (if (equal (length objs) 1)
	(eval `(build act 
		      (build
			action ,action
			object1 ,(car objs))))
	(if (equal (length objs) 2)
	    (eval `(build act 
			  (build
			    action ,action
			    object1 ,(car objs)
			    object2 ,(cadr objs))))
	    (if (equal (length objs) 3)
		(eval `(build act 
			  (build
			    action ,action
			    object1 ,(car objs)
			    object2 ,(cadr objs)
			    object3 ,(caddr objs))))
	    )
	 )
    )
  )
)

(defun build-plan-structure (ants goal acts actwrd)
  "Builds the plan-goal caseframes."
  (let* ((p (plan-goal-structure goal (flistify acts) actwrd))
	 (allvars (return-all-mentioned-variables))
	 (allants (append (flistify ants) (build-type-list allvars))))

    (if allants
	(eval
	  `(build
	     forall ,allvars
	     ,(select-ant allants) ,allants
	     cq ,p))
	(let ((g (if actwrd
		     (eval `(first-atom (find act- ,p)))
		     (eval `(first-atom (find goal- ,p)))))
	      (pl (eval `(first-atom (find plan- ,p)))))
	  (if actwrd
	      (eval
		`(build
		   forall ,allvars
		   act ,g
		   plan ,pl))
	      (eval
		`(build
		   forall ,allvars
		   goal ,g
		   plan ,pl))))
	))
)

(defun plan-goal-structure (goal acts actwrd)
  (let ((planlist (plan-structure (reverse acts))))
    (if actwrd
	(eval
	  `(build
	     act ,goal
	     plan ,planlist))
	(eval
	  `(build
	     goal ,goal
	     plan ,planlist)))))

(defun plan-structure (acts)
  (cond
    ((equal (length acts) 0) nil)
    ((equal (length acts) 1) (first acts))
    ((equal (length acts) 2) (eval
			       `(build
				  action (build lex "snsequence")
				  object1 ,(second acts)
				  object2 ,(first acts))))
    (t (eval
	 `(build
	    action (build lex "snsequence")
	    object1 ,(plan-structure (rest acts))
	    object2 ,(first acts))))
    )
)



    
    




