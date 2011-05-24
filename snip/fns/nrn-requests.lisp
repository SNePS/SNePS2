;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: nrn-requests.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


(in-package :snip)


; =============================================================================
;
; process-requests.non-rule
; -------------------------
;
;       nonlocal-vars : various registers of the current *NODE*
;                         *NODE*, *REQUESTS*
;
;       description   : for each incoming request, the requested channel is
;                       inserted into the *OUTGOING-CHANNELS* register, and
;                       then:
;                          - if the current node is asserted, a report is
;                             sent through the channel
;                          - all known instances are sent through the channel
;                          - if this request is not already being worked on,
;                             requests are passed back to potential sources
;                             of answers
;
;       side-effects  : registers affected:  *REQUESTS*, *OUTGOING-CHANNELS*
;
;                                        written :  rgh 10/06/85
;                                        modified:  rgh 11/18/85
;                                                   rgh  2/02/86
;                                                   rgh  2/09/86 
;                                                   rgh  4/26/86
;                                                   scs  4/20/88
;                                                   njm 10/23/88
;                                                   cpf/njm 10/24/88
;                                                   ssc  5/10/89
;
; 
(defun process-requests.non-rule ()
  (let ((requests *REQUESTS*)
	(remark-sent nil)
	 pathfrom-result)
    (declare (special remark-sent pathfrom-result))
    (setq *REQUESTS* (new.chset))
    (do.set (ch requests t)
      (process-one-request.non-rule ch))))

;
; 
; =============================================================================
;
; process-one-request.non-rule
; ----------------------------
;
;       arguments     : ch - <channel>
;
;       nonlocal-vars : *NODE*
;
;       description   : processes the single request "ch"
;
;                                        written :  rgh  2/09/86
;                                        modified:  rgh  3/09/86
;                                                   rgh  3/27/86
;                                                   rgh  4/26/86
;                                                   scs  6/23/88
;                                                   njm/cpf 10/21/88
;

;;; Modify so that it uses procedural attachment.
(defun process-one-request.non-rule (ch)
  (install-channel ch)
  (cond ((isassert.n *NODE* (context.ch ch))
	 (process-request-of-asserted-node ch))
	(t (when (procedural-attachment-p *NODE*)
	     (process-request-of-attached-node ch))
	   (let ((restr nil)
		 (any-instances-sent nil)
		 (crntct (context.ch ch)))
	     (declare (special any-instances-sent crntct))
	     (send-known-instances ch)
	     (when (or (not any-instances-sent) (is-wh-question (filter.ch ch)))
	       (setq restr (ch-to-restr ch))
	       (when (not-working-on restr crntct ch)
		 (send-rule-use-requests restr crntct (destination.ch ch))
		 (when (and (or (is-rule-to-ant.req ch)
				(is-rule-to-cq.req ch)
				(is-from-user.req ch))
			    (enough-resources))
		   (decrease-resources)
		   (remark '"~%I wonder if"
			   (makeone.ns *NODE*)
			   restr
			   (context.ch ch))
		   (send-node-to-node-requests restr crntct)))
	       (when (and (is-case-of-introduction ch)
			  (not-working-on-introdution restr crntct))
		 (initiate-introduction.rule ch)))))))



(defun process-request-of-attached-node (ch)
  (loop for (sign sub) in
	(execute-attached-function *NODE* (filter.ch ch))
	do (when sign
	  (let* ((crntct (context.ch ch))
		 (newsub (union.sbst (nodize-sub-terms sub) (filter.ch ch)))
		 (newnode (match::applysubst *NODE* newsub)))
	    (declare (special crntct))
	    (cond ((eq sign 'POS)
		   (when (ispat.n newnode)
		     (error "Improper attempt by an attached function to assert the pattern node ~A" 
			    (surfaceToString newnode)))
		   (remark '"~%I figure" (makeone.ns newnode) (new.restr))
		   (assert.n newnode crntct))
		  ((eq sign 'NEG)
		   (when (ispat.n newnode)
		     (error
		      "Improper attempt by an attached function to assert the negation of the pattern node ~A" 
		      (surfaceToString newnode)))
		   (remark '"~%I figure it is not the case that"
			   (makeone.ns newnode) (new.restr))
		   (setf newnode (choose.ns
				  #!((assert min 0 max 0 arg ~newnode))))))
	    (send-reports (makeone.repset (make.rep
					   newsub
					   (filter.sup
					    (sneps:node-asupport newnode)
					    crntct)
					   sign
					   *NODE*
					   nil
					   crntct))
			  ch)))))

;
;
; =============================================================================
;
; is-wh-question
; --------------
;
;       arguments     : substitution - <substitution>
;
;       returns       : <boolean>
;
;       description   : returns "true" if any of the variables of
;                       "substitution" are bound to variables, thus indicating
;                       that the request is for as many answers as can be
;                       found, and "false" if each variable is bound to a
;                       constant, indicating a yes/no request.  Also checks
;                       all free variables dominated by the current node
;                       to see whether or not they are all bound to constants.
;
;                                        written :  rgh  3/27/86
;                                        modified:  rgh  4/18/86
;
;
(defun is-wh-question (substitution)
  (do* ((ns (freevars.n *NODE*)
	    (others.ns ns))
	(bndg (term.sbst (choose.ns ns) substitution)
	      (term.sbst (choose.ns ns) substitution)))
       ((or (isnew.ns ns)
	    (null bndg)
	    (not (is.n bndg))
	    (isvar.n bndg)
	    (ispat.n bndg))
	(not (isnew.ns ns)))))
;
;
; =============================================================================
;
; send-request
; ------------
;
;       arguments     : req - <request>
;                       n - <node>
;                       restr - <restriction>
;
;       nonlocal-vars : the *REQUESTS* register of the activation process of
;                       the node "n", and the *INCOMING-CHANNELS* register
;                       of the current node process
;
;       description   : actually sends the (request) message "req" to the
;                       node "n".  Also installs the incoming channel
;                       information.
;
;       side-effects  : inserts "req" into the *REQUESTS* register of "n"
;                       and updates *INCOMING-CHANNELS*
;
;                                        written :  rgh 10/07/85
;                                        modified:  rgh 11/24/85
;                                                   rgh  4/13/86
;                                                   njm/cpf 10/21/88
;                                                   choi 9/19/90
;
(defparameter *depthCutoffBack* 10
  "Maximum height of SNIP subgoal.")

(defun send-request (req n restr)
  (if (and *depthCutoffBack*
	   (or (> (sneps::node-height n) *depthCutoffBack*)
	       (> (max-height-sub (filter.ch req)) *depthCutoffBack*)))
      (remark
       (format nil "~&SNIP depth cutoff beyond *depthcutoffback* = ~A~%" *depthCutoffBack*)
       nil nil)
		 
    (let (pr)
      (activate.n n)
      (setq pr (activation.n n))
      (when (eq (regfetch pr '*NAME*) 'NUM-QUANT.RULE)
	(let ((new-sbst 
	       (compl.Set (subst.restr restr)
			  (restrict.sbst (subst.restr restr)
					 (quantified-vars.n (regfetch pr '*NODE*))))))
	
	  (setq restr (make.restr new-sbst))
	  (setq req (make.ch new-sbst
			     (switch.ch req)
			     (context.ch req)
			     (destination.ch req)
			     (valve.ch req)))))
      (regstore pr
		'*REQUESTS*
		(insert.chset req (regfetch pr '*REQUESTS*)))
      (regstore pr '*PRIORITY* 'LOW)
      (setq *INCOMING-CHANNELS*
	(insert.feedset (make.feeder restr
				     (context.ch req)
				     n
				     (valve.ch req))
			*INCOMING-CHANNELS*))
      (initiate pr))))

(defun max-height-sub (sub)
  (loop with max = 0
      for (var . term) in sub
      do (setf max (max max (sneps::node-height term)))
      finally (return max)))
;
;
; =============================================================================
;
; send-known-instances
; --------------------
;
;       arguments     : ch - <channel>
;
;       returns       : <boolean>
;
;       nonlocal-vars : the *KNOWN-INSTANCES* and *NODE* registers of the
;                       current node, and "any-instances-sent"
;
;       description   : sends a report of each of the known instances of the
;                       current node through the channel "ch"
;
;
;                                        written :  rgh 10/07/85
;                                        modified:  rgh  3/09/86
;                                                   rgh  3/27/86
;                                                   scs  3/24/88
;                                                   scs  4/20/88
;                                                   njm/cpf 10/21/88
;                                                   njm 10/11/88
;
(defun send-known-instances (ch)
  (declare (special any-instances-sent))
  (do.set (inst *KNOWN-INSTANCES* any-instances-sent)
    (when (try-to-send-report (make.rep (subst.inst inst)
					(support.inst inst)
					(sign.inst inst)
					*NODE*
					nil
					(context.ch ch))
			      ch)
      (setq any-instances-sent t))))
;
;
; =============================================================================
;
; not-working-on
; --------------
;
;       arguments     : restr - <restriction>
;                       ct    - <context>
;                       ch    - <channel>
;
;       returns       : <boolean>
;
;       nonlocal-vars : the *INCOMING-CHANNELS* register of the current node
;
;       description   : returns "true" if there is NOT an incoming channel to the
;                       current node which will supply instances satisfying
;                       the restriction "restr" in the context "ct"; "false"
;                       otherwise
;
;                                        written :  rgh 10/07/85
;                                        modified:  scs 03/24/88
;                                                   njm 10/23/88
;                                                   cpf/njm 07/12/89
;
;
;
(defun not-working-on (restr ct ch)
  (do.set (feeder *INCOMING-CHANNELS* t)
    (when (and (equivalent.restr restr (restriction.feeder feeder))
	       (issubset.ct ct (context.feeder feeder))
	       ;;
	       ;; If the request does not came by Match-channel, it verifies
	       ;; first if it performed a match in the past before saying
	       ;; it is working on it.
	       ;;
	       (if (or (is-rule-to-ant.req ch)
		       (is-rule-to-cq.req ch)
		       (is-from-user.req ch))
		   (let ((source (source.feeder feeder)))
		     (not (or (ismemb.ns source (nodeset.n *NODE* 'sneps:cq-))
			      (ismemb.ns source (nodeset.n *NODE* 'sneps:arg-)))))
		   t))
      (return nil))))
;
;
; =============================================================================
;
; send-rule-use-requests
; ----------------------
;
;       arguments     : restriction - <restriction>
;                       context - <context>
;                       prev-requester - <node> or 'USER
;
;       returns       : <boolean>
;
;       nonlocal-vars : the *NODE* register of the current node
;
;       description   : sends requests for the instance(s) requested by the
;                       <restriction> given as the argument to the function
;                       to all rule nodes for which the current node is in
;                       consequent position except for prev-requester. The
;                       requests context is the one specified by "context".
;
;                                        written :  rgh 10/07/85
;                                        modified:  rgh 11/18/85
;                                                   rgh  4/13/86
;                                                   scs  4/22/88
;                                                   scs  6/23/88
;                                                   njm/cpf 10/21/88
;                                                   njm 10/23/88
;                                                   ssc  5/10/89
;                                                    dk  6/2/93
; (added the IF- arcs for the DOIF transformer rules -: dk)
;
;
;
(defun send-rule-use-requests (restriction context prev-requester)
  (let ((outgoing-request (make.ch (subst.restr restriction)
				   (new.sbst)
				   context
				   *NODE*
				   'OPEN)))
    (declare (special pathfrom-result))
    (broadcast-request
      outgoing-request
      (if (eq prev-requester 'USER)
	  (or pathfrom-result
	      (setq pathfrom-result
		    (sneps::pathfrom '(sneps::or cq- arg- dcq- if-)
				     *NODE*)))
	  (remove.ns prev-requester
		     (or pathfrom-result
			 (setq pathfrom-result
			       (sneps::pathfrom '(sneps::or cq- arg- dcq- if-)
						*NODE*)))))
      restriction)))
;
;
; =============================================================================
;
; broadcast-request
; -----------------
;
;       arguments     : req - <request>
;                       ns - <node set>
;                       restr - <restriction>
;
;       returns       : <boolean>
;
;       description   : sends "req" to each of the nodes in "ns"
;                       ("restr" is the desired restriction)
;
;                                        written :  rgh 10/07/85
;                                        modified:  rgh  4/13/86
;                                        modified:  scs  3/24/88
;
;
(defun broadcast-request (req ns restr)
  (do.ns (nde ns t)
    (send-request req nde restr)))
;
;
; =============================================================================
;
; send-node-to-node-requests
; --------------------------
;
;       arguments     : restriction - <restriction>
;                       context     - <context>
;
;       returns       : <boolean>
;
;       nonlocal-vars : the *NODE* register of the current node
;
;       description   : calls match and sends requests for the instance(s)
;                       requested by "restriction" to all nodes which match
;                       the current node
;
;                                        written :  rgh 10/07/85
;                                        modified:  rgh 11/18/85
;                                                   rgh 11/24/85
;                                                   rgh  4/13/86
;                                                   scs  3/24/88
;                                                   njm/cpf 10/21/88
;
(defun send-node-to-node-requests (restriction context)
  ;;"Modified to facilitate knowledge shadowing by using the most general 
  ;; common instances (mgci) of two patterns.  It now sends requests only 
  ;; to those nodes that are determined not to be redundant by checking  
  ;; the mgci of the current node (source node) and the matched node 
  ;; (target node).       modified by choi: 2/25/92"
  (let* ((rsbst (subst.restr restriction))
	 (rnode (match::applysubst *NODE* rsbst)))
    ;; if the requesting node 'rnode' (*NODE* restricted by 'rsbst')
    ;; is already asserted in the context,
    ;; send request directly to 'rnode'  without doing match.
    (if (isassert.n rnode context)
	(send-request (make.ch nil rsbst context *NODE* 'OPEN) 
		      rnode restriction)
      ;; otherwise, call match and check if the mgci of the current node
      ;; (restricted by 'rsbst') and the target node is asserted.
      ;; send request to such a node that mgci is not asserted.
      (do.supmatchingset (sup (match-in-context *NODE* context rsbst) t)
        (let ((tnode (tnode.supmatching sup))
	      (tsbst (target-sub.supmatching sup))
	      (ssbst (source-sub.supmatching sup)))
	  (unless (or (eq tnode *NODE*)
		      ;; check if the mgci of the current and target node
		      ;; is asserted so that it can block the request
		      (and (ispat.n tnode)
			   (is-mgci-asserted tnode tsbst)))
	    (send-request (make.ch tsbst ssbst context *NODE* 'OPEN)
			  tnode restriction)))))))


;;; Change to call sneps:negate.n instead of snebr::negator-of
(defun is-mgci-asserted (tnode tsbst)
  ;;"The most general common instance (mgci) of two patterns S and T 
  ;; is Ss or Tt, where s is a source binding and t is a target binding.
  ;; Obtaining mgci in SNIP is implemented by applying the target binding 
  ;; to the target node.
  ;; This procedure will return T if the mgci or the negation of the mgci
  ;; is asserted in the current context."
  (let ((mgci (match::applysubst tnode tsbst)))
    (and (not (ispat.n mgci))
	 (or (isassert.n mgci)
	     (isassert.n (sneps:negate.n mgci))))))

;
;
; =============================================================================


#|
   Instructions For Using the Procedural Attachment Facility

Normally, if the system backchains into a proposition-valued function
node, it will use inference to determine what instances of the
(negation of the) node may be asserted.  If, however, the node has an
arc labelled attachedfunction to a node that has been attached to a
function, the system will determine what instances of the
(negation of the) node may be asserted by evaluating the attached
function.  For instance, backchaining into the node
     (p1 (attachedfunction Sum) (add1 3) (add2 4) (sum v1))
could result in a computation to determine that the node
     (m1 (attachedfunction Sum) (add1 3) (add2 4) (sum 7))
should be asserted.

To use the procedural attachment facility, the user must do three things:
1.  if using SNePSLOG, define the frames for the predicates to which
    functions will be attached,
    if using SNePSUL, define the relations to be used as function arguments;
2.  define the attached function;
3.  attach the function to a SNePS predicate node.

These steps are more fully explained below.

1.  For an example of (1) using the above example, the SNePSLOG user would do:
     define-frame Sum(attachedfunction add1 add2 sum)
and the SNePSUL user would do:
     (define add1 add2 sum).
The relation, attachedfunction is defined by the SNePS system.

2.  Attached functions must be defined by
     (define-attachedfunction fun ( <lambda variables> ) &body)
where:
a. fun will be the name of the attached function.
b. each lambda variable must either be a relation used for the
   arguments of the predicate node, or such a relation enclosed in a
   pair of parentheses. If the lambda variable is an atomic relation
   name, it will be bound to the (modified) set of nodes that that
   relation points to.  If it is a relation enclosed in parentheses,
   the relation symbol will be bound to one element of the (modified)
   set of nodes that that relation points to (presumably, there will
   only be one).  The way that the nodes will be modified is
    i. if the node is a variable, it will be left alone;
   ii. if the node's name looks like a Lisp number,
       the number will be provided;
  iii. otherwise, a Lisp symbol whose name is the same string as the
       node's name will be provided.
  In the body of the attached function, the three Lisp predicates
  numberp, symbolp, and sneps:isvar.n may be used to distinguish the
  three types of argument.
c. the attached function must return a list,
   each of whose members is a list of two members:
    i. Either:
       'snip:pos to indicate that the instance of the proposition is
                 to be asserted;
       'snip:neg to indicate that the negation of the instance of the
                 proposition is to be asserted;
    ii. A substitution of the form
          ( ... (var term) ...)
       to indicate the appropriate instance of the proposition,
       where each var is a variable-node argument,
             and term is a Lisp object to be converted into a node
             giving the instance.
    If the attached function returns nil,
    that indicates that neither any instance of the proposition
    nor any instance of its negation is to be asserted.
A simple attached-function definition for the above example is:
(define-attachedfunction sumfn ((add1) (add2) (sum))
  (cond
   ;; If all three are numbers, check that it is correct.
   ((and (numberp add1) (numberp add2) (numberp sum))
    (if (cl:= (cl:+ add1 add2) sum)
	`((snip:pos nil))
      `((snip:neg nil))))
   ;; If add1 and add2 are numbers, and sum is a variable, compute the sum.
   ((and (numberp add1) (numberp add2) (sneps:isvar.n sum))
    `((snip:pos ( (,sum . ,(cl:+ add1 add2))) )))
   ;; Else, don't give an answer
   (t nil)))

3. The user must attach functions to SNePS predicates using
     (attach-function node fun node fun ...)
For example
     (attach-function Sum sumfn)
After which, the SNePSLOG user can ask questions such as
     Sum(3,4,7)?
and  Sum(4,6,?x)?
and the SNePSUL user can ask questions such as
     (deduce attachedfunction Sum add1 3 add2 4 sum 7)
and  (deduce attachedfunction Sum add1 3 add2 4 sum $x)
|#



(defun process-request-of-asserted-node (ch)
  (let ((crntct (context.ch ch)))
    (declare (special crntct remark-sent))
    (unless remark-sent     
      (remark '"~%I know" (makeone.ns *NODE*) (new.restr))
      (setq remark-sent t))
    (send-reports (makeone.repset (make.rep
				   (new.sbst)
				   (filter.sup (sneps:node-asupport *NODE*)
					       crntct)
				   'POS
				   *NODE*
				   nil
				   crntct))
		  ch)))

(defparameter *attached-functions*
  (make-hash-table :test #'eql)
  "Stores the mapping between attached function nodes and associated functions.
     The mapping is bi-directional.  If the key is an attached function node then the
     value is the name (a symbol) of the associated attached function.
     If the key is a symbol (the name of an attached function) the
     value is the associated attached function node.  Attached function must
     be functions of one argument which they expect to be bound to a node
     with the relation attachedFunction emanating from it.")

(defun procedural-attachment-p (node)
  "Returns t if node has an arc labelled attachedfunction
       to a node that has a function attached to it."
  (and (nodeset.n node 'attachedfunction)
       (gethash (function-of-attached-function node)
		*attached-functions*)))

(defmacro define-attachedFunction (attachedFunction vars &body forms)
  "Creates the function definition of the attached function named attachedFunction.
     VARS should be a (possibly empty) list of arc relations
        that get bound to the appropriate node sets.
     However, if any VAR is enclosed in parentheses,
        it gets bound to a member of the appropriate node set.
     FORMS syntax is just as it is for `defuns'."
  (let ((fun-node-var (gensym))
	(strippedvars 
	 (mapcar #'(lambda (v) (if (atom v) v (first v)))
		 vars)))
    `(prog1
	 (defun ,attachedFunction (,fun-node-var)
	   ,@(when (null vars)
	       `((declare (ignore ,fun-node-var))))
	   ((lambda ,strippedvars ,@forms)
	    ,@(mapcar #'(lambda (rel)
			  (if (atom rel)
			      `(ns-to-attachment-arg
				(sneps:nodeset.n ,fun-node-var ',rel))
			    `(first
			      (ns-to-attachment-arg
			       (sneps:nodeset.n ,fun-node-var
						',(first rel))))))
		      vars)))
       (when (member :compiler *features*) 
	 (compile ',attachedFunction))
       )))

(defmacro attach-function (&rest nodeset-fnct)
  "NODESET-FNCT must be a list of the form
        '( ... nodesetform functionname ...),
   where NODESETFORM evaluates to a singleton nodeset,
     and FUNCTIONNAME is a symbol that names an attached function.
     This function will store each attached function in
     *attached-functions* with the node as key."
  (declare (special *attached-functions*))
  (do ((nsflist (cddr nodeset-fnct) (cddr nsflist))
       (ns (first nodeset-fnct) (first nsflist))
       (fn (second nodeset-fnct) (second nsflist)))
      ((and (null ns) (null fn)) t)
    (when (null ns)
      (error "~&There is an extra function name: ~A~%" fn))
    (when (null fn)
      (error "~&There is an extra nodeset: ~A~%" ns))
    (unless (symbolp fn)
      (error "~&~A must be a symbol, but isn't.~%" fn))
    (unless (fboundp fn)
      (error "~&~A must name a function, but doesn't.~%" fn))
    (setf ns (sneps::buildeval ns t))
    (when (sneps:others.ns ns)
      (error "~&Ambiguous attached function node: ~A~%" ns))
    (setf (gethash (sneps:choose.ns ns) *attached-functions*) fn)
    (sneps:set.sv 'snepsul::attached-functions
		  (insert.ns (choose.ns ns)
			     (sneps:value.sv 'snepsul::attached-functions)))))

(defun execute-attached-function (node subst)
  "Applies the given attached function to the given node."
  (funcall (gethash (function-of-attached-function node)
		    *attached-functions*)
	   (match::applysubst node subst)))

(defun function-of-attached-function (node)
  "Returns the action node of the given ACT-NODE."
  (choose.ns (nodeset.n node 'attachedfunction)))

(defun ns-to-attachment-arg (ns)
  "Returns a list of objects corresponding to the members of the nodeset ns:
       if the member is a variable node, include it;
       if the member is a node whose name looks like a number, include the number;
       else transform the node into a symbol."
  (let (list)
    (sneps:do.ns (node ns list)
		 (push
		  (if (isvar.n node)
		      node
		    (sneps:node-to-lisp-object node))
		  list))))

(defun nodize-sub-terms (subst)
  "Returns a substitution like subst but modified
       so that the term of each pair is either nil or a node."
  (mapcar #'(lambda (pair)
	      (cons (car pair)
		    (let ((trm (cdr pair)))
		      (cond ((sneps:node-p trm) trm)
			    ((null trm) nil)
			    (t (sneps:lisp-object-to-node trm))))))
	  subst))
