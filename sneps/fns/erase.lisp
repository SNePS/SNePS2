;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: erase.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
(export 'gc-nodes)

(defun garbage-node (nde)
  "Returns T iff nde is a useless node."
  (and (is-isolated nde)
       (not (pointed-to nde))
       (not (or (node-gi-node nde) (node-activation nde)
		(node-asupport nde) (node-jsupport nde)
		(node-contexts nde)))))

(defun pointed-to (node)
  "Returns T iff the node NODE is pointed to by some SNePSUL variable
   other than NODES or PATTERNS."
  (do.ns (var (value.sv 'variables))
	 (unless (member var '(nodes patterns))
	   (let ((varval (value.sv var)))
	     (cond ((is.n varval)
		    (when (eql node varval) (return-from pointed-to T)))
		   ((is.ns varval)
		    (do.ns (nde (value.sv var))
			   (when (eql node nde) (return-from pointed-to T)
				 ))))))))

(defun gc-nodes ()
  (let ((erased-ns nil))
    (declare (special erased-ns))
    (do.ns (nde (value.v 'nodes))
	   (when (garbage-node nde)
	     (erase-node nde :silent t)))
    (unless (isnew.ns erased-ns)
      (set.sv 'variables (clean-svars (value.sv 'variables) erased-ns))
      #|(format t "~&gc-nodes: ~A nodes erased.~%" (length erased-ns))|#)
    (values)))



; ==========================================================================
;
; erase
; -----
;
;      arguments     : nsf <ns>.
;
;      returns       : no value.
;
;      description   : Erases each node in the nodeset nsf.
;                      Erase of a node n means that 
;                        i) the node n is dismantled;
;                       ii) all wires coming into  n from other nodes 
;                           should also be removed; and
;                      iii) all the nodes isolated due to the erase of n
;                           should also be erased.  If the non-local switch
;                           interact-mode is set to non-nil, the system asks
;                           the user whether he wants the thus-isolated node
;                           to be erased.
;                      For the precise def. of "being isolated",
;                      see the comments to fn is-isolated.
; 
;      side-effects  : The effect of erase and some message printed.
;                      System variables (nodes, varnodes, nsvars) are
;                      appropriately updated when nodes are erased.
;
;      notes         : The prog var "erased-ns" works as a record of all
;                      erased nodes modified by fn dismantle-node.
;
;                                         written:  HYY 08/14/83
;                                         modified: HYY 10/11/83
;                                         modified: scs 02/27/87
;                                                   hc  07/18/93
;
(defsnepscom erase ((&rest nsf))
  (let (erased-ns)
    (declare (special erased-ns))
    (mapc #'erase-node (nseval nsf))
    (unless (isnew.ns erased-ns)
      (set.sv 'variables (clean-svars (value.sv 'variables) erased-ns)))
    (values)))

;
; ==========================================================================
;
; silent-erase 
; ------------
;
;                                         written:  scs  5/10/89
;                                         modified: hc   7/18/93
;
(defsnepscom silent-erase ((&rest nsf))
  (let (erased-ns)
    (declare (special erased-ns))
    (mapc #'(lambda (n) (erase-node n :silent t)) (nseval nsf))
    (set.sv 'variables
	    (clean-svars (value.sv 'variables) erased-ns))
    (values)))

;
; ==========================================================================
;
; erase-node
; ----------
;
;      arguments     : n <node>.  
;
;      returns       : nil.
;
;      description   : Erases node n if n is isolated.
;
;      side-effects  : The erase of node n and some possible resulting
;                      erases of some other isolated nodes, and appropriate
;                      message printed.
;
;      implementation: Erase-node is invoked from top-level erase only.
;                      Erase-node-1 is similar to this, but is called
;                      while the erasing action is propagated to the node 
;                      n's vicinities which become newly isolated.
;
;                                         written:  HYY 08/14/83
;                                         modified: HYY 10/11/83
;                                                   njm 05/01/89
;                                                   ssc  5/10/89
;
;
(defun erase-node (n &key silent)
  (declare (special silent))
  (let ((crntct (buildcontext (new.ns))))
    (declare (special outunit crntct))
    (cond ((not (is.n n)) 
	   (unless silent
	     (format outunit "~&Node ~A not found or already erased. ~%" n)))
	  ((is-isolated n) 
	   (when (ismemb.ns n (value.sv 'assertions))
	     (erase-contexts n)
	     (set.sv 'assertions (remove.ns n (value.sv 'assertions))))
	   (unless (or silent
		       (isbase.n n)
		       (isvar.n n))
	     (format outunit "~&~%")
	     (Print-description.n n outunit))
	   (dismantle-node n))
	  ((not silent)
	   (format outunit "~&~%Node ~A not isolated. Can't be erased. ~%" n))
	  (t t))))
;
;
;
; ==========================================================================
;
; erase-cable
; -----------
;
;      arguments     : o-n <node>.
;                      rel <relation>.
;                      ns  <nodeset>.
;
;      returns       : nil.
;
;      description   : As a part of the erase of node o-n, the cable (rel ns)
;                      from the originating node o-n is erased.  Erase
;                      of a cable is achieved by  cutting all the wires
;                      coming into o-n.
;
;      side-effects  : the erase of the cable.
;
;      implementation: Notice that garbage nodes only can be erased. Thus,
;                      all the cables erased must be descending relations
;                      that come out from o-n.
;
;                                         written:  HYY 08/14/83
;                                         modified: scs 02/27/87
;                                         modified: scs 04/16/96
;
;
(defun erase-cable (o-n rel ns)
  (let ((crel (converse.r rel)))
    (mapc #'(lambda (g-n)
	      (cut-wire g-n crel o-n)
	      (if (garbage-node g-n) (erase-node-1 g-n)))
	  ns)))


;
;
;
; ==========================================================================
;
;cut-wire (n0 rel n1)
;--------
;
;      arguments     : n0, n1 <nodeindent>.
;                      rel : <relation>.
;
;      returns       : nodeset which is the new cable for rel from n0.
;
;      description   : As a part of erase action, cut the wire from node n0
;                      to node n1 through relation rel, thus, in the cable
;                      of rel from n0, you don't have the wire going to nn1
;                      any longer.
;
;      side-effects  : the cut of the wire.
;
;      implementation: The node n1 is the node from which the erase action 
;                      is propagated.  The node n0 is the affected node.  As
;                      n1 doen not exit any longer, the wire going to n1 
;                      from n0 is removed.  Because of this nature, the 
;                      relation rel is always an ascending relation.
;
;                                         written:  HYY 08/14/83
;                                         modified:
;
;
(defun cut-wire (n0 rel n1)
  (setnodeset.n n0 rel (remove.ns n1 (nodeset.n n0 rel))))

;
;
;
; ==========================================================================
;
; is-isolated (nn)
;-----------
;
;      arguments     : nn <nodeident>.
;
;      returns       : <boolean>
;
;      description   : Checks and answers whether or not a node named as nn
;                      is isolated.  A node is called "isolated" iff the 
;                      node has no descending relations into it.
;
;                                         written:  HYY 08/14/83
;                                         modified:
;
;
(defun is-isolated (nn)
  (do.fcs (r ns (node-fcableset nn) t)
    (if (isup.r r) (return nil))))

;
; ==========================================================================
;
; erase-node-1
; ------------
;
;       arguments     : n <node>.
;
;       returns       : nil.
;
;       description   : As an effect of the propagation of an erase action,
;                       this function erases node n that has become newly
;                       isolated and trims all cables coming into it, if it
;                       is not a standing alone assertion.
;
;       side-effects  : The erase of node n and its cables.
;
;       implementation: If the global switch interact-mode is set to 
;                       non-nil, and if n is not a base node or a var 
;                       node, the system asks user whether he wants it 
;                       to be erased.
;
;       Changes       : Now all nodes not suppported are dismantled, since
;                       they can not be find by 'FIND' command. njm 05/02/89
;
;
;                                          written:  HYY 08/16/83
;                                          modified: SCS 03/01/88
;                                                    njm 05/02/89
;                                                    ssc  5/10/89
;
;
(defun erase-node-1 (n)
  (declare (special outunit silent))
  (cond ((not (isnew.ctcs (node-asupport n)))
	 (unless silent
	   (format outunit
		   "~&~%Node ~A, a supported node, is left intact. ~%" n)))
	(t (unless (or silent
		       (isbase.n n)
		       (isvar.n n))
	     (format outunit "~&~%")
	     (Print-description.n n outunit))
	   (dismantle-node n))))

;
;
;
; ==========================================================================
;
; resetnet
; --------
;
;       arguments     : reset-relations? &optional
; 
;       returns       : no value.
;
;       description   : Resets the network to the initial status. The
;                       relations defined are left effective unless the user
;                       explicitly ask to reset them by supplying T for
;                       'reset-relations?'. In this case the relations are
;                       reset to the relations defined in *initial-relations*
;                       (see 'sneps-setup').
;
;       side-effects  : the reset action.
;
;                                          written : HYY 10/11/83
;                                          modified: scs 02/27/87
;                                                    njm 05/02/89
;                                                    njm 05/02/89
;                                                    ssc 05/10/89
;                                                    hc  05/18/89
;                                                    hc  06/30/93
;                                                    hc  07/18/93
;                                                    hi  03/28/99
;


(defsnepscom resetnet ((&optional (reset-relations? nil)) (top) t)
  (declare (special outunit crntct *Node-Counter*
		    snip::*attached-functions*
		    snip::*primitive-action-functions*))

  ;; Do this to make processes garbage-collectible:
  (clear-infer-all)
  
  ;; Remove node access information:
  (do.ns (n (value.sv 'nodes))
	 (remprop (nodeaccess n) '=snode))
    
  ;; Initialize the list of inconsistent hypotheses sets
  (setf *nogoods* nil)
  (setf *oknogoods* nil)

  ;;Initialize node counter. 
  (reset-node-counter)

  ;; Initialize node-name counters:
  (dolist (prefix '(b m v p tm tv tp))
    (setf (get 'gennewnode prefix) 0))
  (setf (get 'gennewcontext 'c) 0)

  ;; Clear hashtable of attached functions and attached primitive actions
  (setf snip::*attached-functions* (make-hash-table :test #'eql)
	snip::*primitive-action-functions* (make-hash-table :test #'eql))
  ;; Reset SNePSUL variables:
  (do.svs (v (value.sv 'variables))
	  (unless (member v '(relations contexts))
	    (remprop v :val)))
  (when reset-relations?
    ;; Reset relations AND paths!!
    (do.rs (r (value.sv 'relations))
	   (when (get r :pathdef)
	     (eval `(undefine-path ,r)))
	   (undefine.r r))
    (remprop 'relations :val)
    (mapcar #'(lambda (ident) (new.r ident))
	    *initial-relations*))

  ;; Initialize SNePSUL variables:
  (set.sv 'nodes nil)
  (set.sv 'varnodes nil)
  (set.sv 'variables '(variables relations nodes command 
		       lastcommand errorcommand contexts lastvalue 
		       assertions patterns defaultct))
  (if (hash-table-p (value.sv 'contexts))
      (clrhash (value.sv 'contexts))
    (set.sv 'contexts (make-hash-table :test #'equal)))
  (name.ct (buildcontext (new.ns)) 'default-defaultct)
  (set.sv 'defaultct 'default-defaultct)
  (setf crntct 'default-defaultct)
  (format outunit
	  "~&~%Net reset~:[ - Relations and paths are still defined~;~]"
	  reset-relations?)
  (values))

;
; ==========================================================================
;
;  dismantle-node (n)
;  --------------
;
;       arguments     : n <node>.
;
;       returns       : nil.
;
;       nonlocal-vars : erased-ns from erase.
;
;       description   : Physically dismantles the nodeinformation of the
;                       node n, updates appropriate sysvars (nodes or
;                       varnodes), and records the node dismantled in the
;                       non-local variable erased-ns.
;
;       side-effects  : The action of dismantling with some message printed;
;                       update of some system variables and the non-local
;                       varialbe erased-ns.
;
;       implementation: The non-local var erased-ns is updated to be used
;                       for final update of the system variable nsvars at
;                       the end of all erase action taken at fn erase.
;                       Dismantling of a node n is completed by invoking
;                       fn erase-cable that removes all the cables coming
;                       into it.
;
;                                          written : HYY 10/11/83
;                                          modified: scs 02/27/87
;                                                    ssc  5/10/89
;
;
(defun dismantle-node (n)
  (declare (special erased-ns silent outunit))
  (unless (or silent (isbase.n n) (isvar.n n)) 
    (format outunit "node dismantled. ~%"))
  (set.sv 'nodes (remove.ns n (value.sv 'nodes)))
  (cond ((isvar.n n)
	 (set.sv 'varnodes (remove.ns n (value.sv 'varnodes))))
	((ispat.n n)
	 (set.sv 'patterns (remove.ns n (value.sv 'patterns)))))
  (setf erased-ns (nconc erased-ns (list n)))
  (remprop (nodeaccess n) '=snode)
  (do.fcs (r ns (node-fcableset n) nil) (erase-cable n r ns)))
;
;
;
; ==========================================================================
;
;  issureerase (n)
;  -----------
;
;       arguments     : n <node>.
;
;       returns       : <boolean>
;
;
;       description   : Describes the node n, and prints a message asking
;                       the user if he wants to erase the node n.  
;                       The user's response decides the return value of 
;                       this function.
;
;       side-effects  : Some i/o activities.
;
;                                          written : HYY 10/11/83
;                                          modified: ejm 05/23/84
;                                          modified: scs 03/01/88
;                                                    ssc  5/10/89
;
;
#|
(defun issureerase (n)
  (declare (special outunit interact-mode))
  (cond (silent t)
	(t (Print-description.n n outunit)
	   (cond (interact-mode (yes-or-no-p "Do you want to erase ~A?" n))
		 (t t)))))
|#

;(defvar interact-mode t)

;
;
;
; ==========================================================================
;
; clean-svars
; -----------
;
;       arguments     : svar-set  -- <svs>
;                       erased-ns -- <ns>.
;
;       returns       : A nodeset <svs> which is the cleaned version of
;                       nsvars* through the filter nodeset erased-ns.
;                       
;       description   : Svar-set is a set of variables where each variable
;                       points to a set of nodes.  This function filters 
;                       out the nodes referred to in erased-ns from the 
;                       value of each variable in the variable node set.  
;                       The effect is it gets an s-expr which should be 
;                       the new content of the updated sysvar nsvars 
;                       ultimately.
;
;       implementation: Clean-svars runs recursively reducing the list
;                       svars* by one element.
;
;       changes:        If the variable is not a system svar and its value
;                       is an empty node set it is removed from the sneps
;                       variable list. 
;
;                                          written : HYY 10/18/83
;                                          modified: njm 05/02/89
;
;
;
(defun clean-svars (svar-set erased-ns)
  (cond ((isnew.svs svar-set) svar-set)
	((is.ns (value.sv (choose.svs svar-set)))
	 (cond ((and (isnew.ns (set.sv (choose.svs svar-set)
				       (compl.ns (value.sv (choose.svs svar-set)) erased-ns)))
		     (not (issys.sv (choose.svs svar-set))))
		(remprop (choose.svs svar-set) :val)
		(clean-svars (others.ns svar-set) erased-ns))
	       (t (insert.svs (choose.svs svar-set)
			      (clean-svars (others.ns svar-set) erased-ns)))))
	(t (insert.svs (choose.ns svar-set)
		       (clean-svars (others.ns svar-set) erased-ns)))))

;
;
; ==========================================================================
;
; erase-contexts 
; --------------
;
;       arguments     : nd -- <node>
;
;       returns       : nil
;                       
;       description   : Updates context information due to the erase of an
;                       hypothesis:
;                           - removes the contexts where the hypothesis is
;                             present from the support of all nodes.
;                             (if a node loose all its support then it is
;                             erased if it is an isolated node).
;                           - removes the above contexts from the context
;                             data-base (in the present implementation a
;                             hash table)
;                           - removes as sneps variables the context names
;                             (if any) of removed contexts
;                           - If one of removed contexts is named DEFAULTCT
;                             then the DEFAULTCT is the removed context without
;                             the hypothesis (that is being removed - ND)
;
;                                          Written : njm 05/02/89
;                                          modified: 
;
;
;
(defun erase-contexts (nd)
  (declare (special erased-ns outunit))
  (let ((nodes-without-support (new.ns))
	(ct-set (node-contexts nd)))
    (declare (special nodes-without-support))
    ;;
    ;; removes context from the support of all nodes
    ;; (if a node loose all its support then it is appended
    ;; to 'nodes-without-support')
    ;; 
    (remove-contexts-from-supports (value.sv 'nodes) ct-set)
    ;;
    ;; removes contexts
    ;;
    (do.cts (one-ct ct-set)
      ;;
      ;; It changes variables which values are context names. 
      ;; The new value is the context without the hypothesis
      ;; that is being removed.
      ;;
      (let ((new-ct (buildcontext (remove.ns nd (context-hyps one-ct)))))
	(do.svs (name (context-names one-ct))
	  (name.ct new-ct name)))
      ;;
      ;; removes the context entry from the hash table
      ;;
      (remhash (context-hyps one-ct) (value.sv 'contexts)))
    ;;
    ;; removes nodes without support due to the previous context removal
    ;; (only if node is isolated)
    ;;
    (do.ns (one-node (remove.ns nd nodes-without-support))
      (when (is-isolated one-node)
	(unless (or (isbase.n one-node) (isvar.n one-node))
	  (format outunit "~&~%")
	  (Print-description.n one-node outunit))
	(dismantle-node one-node)
	(setf erased-ns (nconc erased-ns (list one-node)))))))


;
;
; ==========================================================================
;
; remove-contexts-from-supports  
; -----------------------------
;
;       arguments     : allnodes -- <node set>
;                       ct-set   -- <context set>
;
;       returns       : nil
;                       
;       description   : removes the contexts present in CT-SET 
;                       from the support of nodes present in ALLNODES.
;
;                                          written : njm 05/02/89
;                                          modified: 
;
;
;
(defun remove-contexts-from-supports (allnodes ct-set)
  (declare (special nodes-without-support))
  (do.ns (one-node allnodes)
    (let ((supp (node-asupport one-node)))
      (unless (isnew.ctcs supp)
	(let ((new-supp (filter-support supp ct-set)))
	  (setf (node-asupport one-node) new-supp)
	  (when (isnew.ctcs new-supp)
	    (setq nodes-without-support
		  (insert.ns one-node nodes-without-support))))))))


;
;
; ==========================================================================
;
; filter-support 
; --------------
;
;       arguments     : supp   -- <context cable set>
;                       ct-set -- <context set>
;
;       returns       : <context cable set>
;                       
;       description   : removes the contexts present in CT-SET 
;                       from the support SUPP.
;
;
;                                          written : njm 05/02/89
;                                          modified: 
;
;
;
(defun filter-support (supp ct-set)
  (let ((new-supp (new.ctcs))
	(new-hyp (compl.cts (getcontextset.ctcs 'HYP supp) ct-set))
	(new-der (compl.cts (getcontextset.ctcs 'DER supp) ct-set))
	(new-ext (compl.cts (getcontextset.ctcs 'EXT supp) ct-set)))
    (do.cts (c new-hyp) (setq new-supp (insert.ctcs 'HYP c new-supp)))
    (do.cts (c new-der) (setq new-supp (insert.ctcs 'DER c new-supp)))
    (do.cts (c new-ext) (setq new-supp (insert.ctcs 'EXT c new-supp)))
    new-supp))



    
    




