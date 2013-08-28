;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: find.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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




;; Altered for ACL 6 by FLJ

(in-package :sneps)


; ==========================================================================
;
; find
; ----
;
;       arguments     : snepsul-exp - <snd>
;
;       returns       : result - <node set>
;
;       description   : This function use the informations in the 
;                       snepsul-exp to find a node set which every node 
;                       satisfy the specifications in the snepsul-exp.
;
;       implementation: This function call the function find1 to the node
;                       bind set which satisfy the snepsul-exp, and then
;                       transform the node bind set to node set.
;
;                                          written:  CCC 08/16/83
;                                          modified: CPF 09/22/88
;                                                    njm 04/27/89
;                                                    hc  07/18/93
;
(defsnepscom find ((&rest snd) (top ns bns tbns fns))
  (let ((crntct (processcontextdescr snd)))
    (declare (special crntct))
    (values (in-context.ns (nbs-to-ns (find1 (fevalsnd (getsndescr snd))))
			   (iscontext-given snd))
	    crntct)))
 
;
; ==========================================================================
;
; in-context.ns
; -------------
;
;       arguments     : ns - <node set>
;                       def - <boolean>
;
;       returns       : result - <node set>
;
;       description   : It returns a set with all nodes in `ns' which
;                       are present in the current context.                       
;                       
;
;       implementation: This function calls function in-context1.ns in 
;                       order to verify if the dominating nodes of each 
;                       node in `ns' is present in the current context.
;                       
;                       
;
;                                          written:  cpf 10/03/88
(defun in-context.ns (ns def)
  (apply 'append
	 (mapcar #'(lambda (n)
		     (or (unless def
			   (if (isnew.ctcs (node-asupport n))
			       (makeone.ns n))) 
			 (if (isassert.n n) (makeone.ns n))
			 (if (in-context1.ns (dominating-asupp-nodes.n n))
			     (makeone.ns n))))
		 ns)))
;
; ==========================================================================
;
; in-context1.ns
; --------------
;
;       arguments     : ns - <node set>
;
;       returns       : <boolean>     
;
;       description   : It verifies if any one of the nodes in `ns' is
;                       asserted.                       
;                       
;
;       implementation: 
;                                              
;
;                                          written:  cpf 10/03/88
;                                                    hc/njm 05/18/89
;                                                    scs 06/15/89
;
(defun in-context1.ns (ns)
      (do.ns (node ns nil)
	(when (isassert.n node)
	  (return t))))
;
;
; ==========================================================================
;
; list-nodes
; ----------
;
;       arguments     : snepsul-exp - <snd>
;                 
;       returns       : result - <node set>
;
;       description   : This function returns all nodes that belong 
;                       to the belief space defined by the context.
;
;
;                                          written:  njm 05/11/89
;                                          modified: hc  07/18/93
;
(defsnepscom list-nodes ((&optional cname) find)
  (let ((crntct (or cname (value.sv 'defaultct))))
    (declare (special crntct))
    (values (in-context.ns (value.sv 'nodes) t)
	    crntct)))

;
;
; ==========================================================================
;
; list-hypotheses
; ---------------
;
;       arguments     : snepsul-exp - <snd>
;                 
;       returns       : result - <node set>
;
;       description   : This function returns all hypotheses that define 
;                       the specified context.
;
;
;                                          written:  njm 05/11/89
;                                          modified: hc  07/18/93
;
(defsnepscom list-hypotheses ((&optional cname) find)
  (let ((crntct (or cname (value.sv 'defaultct))))
    (declare (special crntct))
    (values (context-hyps (value.sv crntct))
	    crntct)))

;
; ==========================================================================
(defsnepscom beliefs-about ((&rest snepsul-exp) (top ns bns fns))
  "Returns a set of all the asserted nodes
        that dominate the nodes specified by the snepsul expressions in snepsul-exp."
  (let* ((crntct (processcontextdescr snepsul-exp))
	 (crntctname crntct)
	 (ns (in-context.ns
	      (nseval (getsndescr snepsul-exp))
	      crntct))
	 (dominating (new.ns)))
    (declare (special crntct crntctname))
    (do.ns (n ns dominating)
	   (setf dominating
	     (union.ns (dominatingnodes.n n) dominating)))
    (values (remove-if-not.ns 
	     #'(lambda (n) (isassert.n n))
	     (union.ns ns dominating)) crntct)))
; ==========================================================================
;
; fevalsnd 
; --------
;
;       arguments     : snd - <snd>
;
;       returns       : (((path . nbs) ... ) . ((path . sv) ... ))
;
;       description   : This function will organize the given snd to
;                       two parts. The first part contain cable bind set 
;                       which have node bind set and path together.  
;                       The second part contain a set which contain sneps 
;                       variable and path together.  Because this format
;                       has caused problems in the past a small trace
;                       follows now:
;* (define a b c)
;* (build a (build c joe a fred) b (build c tom a (build a carl c john)))
;
;* (find a ?x b (find a ?y c tom))
;1 <Enter> fevalsnd ((a (? x) b (find a (? y) c tom)))
;|2 <Enter> fevalsnd ((a (? y) c tom))
;|2 <EXIT>  fevalsnd  ((((c) (tom))) ((a) . y))
;1 <EXIT>  fevalsnd  ((((b) (m3 (y m2)))) ((a) . x))
;(m4)
;
;* (find a (find c joe) b (find a (find a carl c ?x) c tom))
;1 <Enter> fevalsnd ((a (find c joe) b (find a (find a carl c (? x)) c tom)))
;|2 <Enter> fevalsnd ((c joe))
;|2 <EXIT>  fevalsnd  ((((c) (joe))))
;|2 <Enter> fevalsnd ((a (find a carl c (? x)) c tom))
;| 3 <Enter> fevalsnd ((a carl c (? x)))
;| 3 <EXIT>  fevalsnd  ((((a) (carl))) ((c) . x))
;|2 <EXIT>  fevalsnd  ((((c) (tom)) ((a) (m2 (x john)))))
;1 <EXIT>  fevalsnd  ((((b) (m3 (x john))) ((a) (m1))))
;(m4)
;
;       implementation: Use findeval to eval the sneps expression since the 
;                       findeval will return a <node bind set>.
;                       is.qv is defined in "findhelp.l"
;
;                                          written:  CCC 08/16/83
;                                          modified: CCC 11/21/83
;                                          comment modified: JG 3/3/86
;                                          modified: ssc  5/10/89
;
(defun fevalsnd (snd)
  (do ((snd1 snd (rest.snd snd1))
       (path nil)
       (nsf nil)
       (path-nbs-bag nil)
       (path-svar-bag nil)
       (qvar nil))
      ((isnew.snd snd1) (cons path-nbs-bag path-svar-bag))
    (setq path (checkpath (relationform.snd snd1))
	  nsf (checknodesetform (nodesetform.snd snd1)))
    (cond ((is.qv nsf)
	   (setq qvar (svar.qv nsf))
	   (if (not (is.sv qvar)) (new.sv qvar))
	   (setq path-svar-bag (cons (cons path qvar) path-svar-bag)))
	  (t
	   (setq path-nbs-bag
		 (cons (cons path (findeval nsf)) path-nbs-bag))))))
 
;
; ==========================================================================
;
; checkqvars
; ----------
;
;       arguments     : qvars - ((path . sv) ... )
;
;       returns       : <node bind set>
;
;       nonlocal-vars : result - <node bind set>
;                       The nodes in the result are those found by
;                       considering only constant nodes (see checkconsts)
;                       or all the nodes in the network if the node
;                       description passed to "find" has only questionmark
;                       variables in the node set position.
;
;       description   : This function try to check every node in result by
;                       calling function checkqvars1, to see it have the 
;                       given qvars.
;
;       side-effects  : the result is changed to store the new result.
;
;                                          written:  CCC 08/16/83
;                                          modified: CCC 11/21/83
;                                          comment modified: JG 3/3/86
;                                          modified: ssc  5/10/89
;
;
(defun checkqvars (qvars result)
  (mapcan #'(lambda (nb)
	      (declare (special nb))
	      (checkqvars1 (node.nb nb) (bindset.nb nb) qvars))
	  result))
 
;
; ==========================================================================
;
; checkqvars1
; -----------
;
;      arguments     : n - <node>
;                      bs - <bind set>
;                      qvars - ((path.(sv)) ... )
;
;      returns       : <nb> or nil
;
;      description   : This function try to check if the node "n" has the
;                       cables specified in "qvars" subject to the constraints
;                       specified in "bs".  If the intersection of the new
;                       restriction and old restriction is not empty, then
;                       replace the intersect restrictions for the old.
;                       Otherwise, return failure.
;
;                                         written:  CCC 08/16/83
;                                         modified: CCC 11/21/83
;
; (defun checkqvars1 (n bs qvars)
;  (declare (special n bs qvars))
;  (if (repeat (new-constraint-ns old-constraint-ns sv)
;       until(null qvars)
;              (setq new-constraint-ns
;                    (pathfrom (car (first qvars)) n)
;                    sv
;                    (cdr (first qvars))
;                    qvars
;                    (rest qvars))
;       while new-constraint-ns
;              (setq old-constraint-ns (nodeset.b (assoc sv bs)))
;       while (or (isnew.ns old-constraint-ns)
;		       (issubset.ns new-constraint-ns old-constraint-ns))
;              (setq bs (replace.bs (new.b sv new-constraint-ns) bs)))
;      (list (new.nb n bs))))

; This is an alternative to the above definition without using REPEAT.
; It was derived using the macroexpansion for the above form and making
; some minor modifications. The control structure 'should' be exactly
; the same. hc Oct.19, 89
;
; Use it now to allow proper cross-referencing with Lucid's CREF:
; hc Dec.6, 90 - and maybe keep using it to get rid of this stupid REPEAT
;
(defun checkqvars1 (n bs qvars)
  (declare (special n bs qvars))
  (let (new-constraint-ns old-constraint-ns sv)
    (if (loop (if (null qvars)
		  (RETURN t))
	      (setq new-constraint-ns (pathfrom (car (first qvars)) n)
		    sv (cdr (first qvars))
		    qvars (rest qvars))
	      (if (null new-constraint-ns)
		  (RETURN nil))
	      (setq old-constraint-ns (nodeset.b (assoc sv bs)))
	      (if (null (or (isnew.ns old-constraint-ns)
			    (issubset.ns new-constraint-ns old-constraint-ns)))
		  (RETURN nil))
	      (setq bs (replace.bs (new.b sv new-constraint-ns) bs)))
	(list (new.nb n bs)))))

;
; ==========================================================================
;
; findeval
; --------
;
;       arguments     : snepsul-exp - <snepsual-exp>
;
;       returns       : <node bind set>
;
;       description   : Evaluate the given snepsul expression.  If the 
;                       snepsul expression begins with a sneps function 
;                       name, then evaluate that function with the 
;                       remaining lst elements as arguments for that 
;                       function.  Also, if the function is
;                       find or findassert, change it to insidefind and 
;                       insidefindassert so they will return a <node bind set> 
;                       instead of a node set. Otherwise, call findeval-map
;                       to try to find-evaluate the elements of the snepsul
;                       expression one by one.  Also make sure the result 
;                       return by the function is a <node bind set>.
;
;       implementation: similar to sneval, except the result returned must 
;                       be a <node bind set>.
;
;                                          written  : CCC 11/21/83
;                                          modified : SSC 11/04/87
;
;

(defun findeval (snepsul-exp)
    (prog (result)
          (declare (special result))
          (setq result
                (cond ((null snepsul-exp) (new.nbs))
                      ((numberp snepsul-exp) (findeval (un-ize snepsul-exp)))
                      ((atom snepsul-exp) (findeval-atom snepsul-exp))
                      ((isfns.com (first snepsul-exp))
                       ((lambda (ev-result)
			  (if (atom ev-result) (findeval ev-result) ev-result))
			(protect-eval (eval (find-inside snepsul-exp)))))
                      (t (findeval-map snepsul-exp))))
          (return (cond ((is.nbs result) result) (t (ns-to-nbs result))))))
 
; ==========================================================================
;
; checkpath
; ---------
;
;       arguments     : path - <path>
;
;       returns       : nil if path is not a path, otherwise return 
;                       path.
;
;       description   : test the path is a path or not.  If it is a relation,
;                       then make it a path.
;
;       side-effects  : if path is not a path or relation, give a sneps error.
;
;       implementation: the function doesn't actully check the path is
;                       a path or not, if path is a list, the function
;                       will assume it is a path, if path is a symbol,
;                       the function will check it is a relation or not.
;                       do it this way because it will take two many time
;                       to go through the whole list.
;
;                                          written  : CCC 11/10/83
;                                          modified : CCC 11/21/83
;
(defun checkpath (path)
    (declare (special path))
    (cond ((is.p path) path)
          ((is.r path) (rel-to-path path))
          (t
           (sneps-error (format nil " Illegal relation or path: ~A" path)
                        'find
                        'checkpath))))

 
; ==========================================================================
;
; find1
; -----
;
;       arguments     : fsnd - (((path . nbs) ... ) . ((path . sv) ... ))
;
;       returns       : <node bind set>
;
;       description   : use the information in fsnd to find a <node bind set>
;                       where every node satisfies the specification in
;                       the fsnd and their binding.
;
;       implementation: If consts is not empty, call checkconst to get 
;                       the <node bind set> which every node satisfy the 
;                       speifications in consts and then if the result 
;                       nodebind set and qvars are not empty, call 
;                       checkqvars for the qvars and return the results.  
;                       Otherwise, if qvars is not empty, get all the 
;                       nodes in the system and check them against the 
;                       specification by call checkqvars and return the 
;                       result.  Note that "result" is sideffected by
;                       checkqvars.
;
;                                          written  : CCC 11/21/83
;                                          modified :
;                                          comment modified: JG 3/3/86
;                                          modified:  ssc  5/10/89
;
;
(defun find1 (fsnd)
  (when fsnd
    (let* ((consts (car fsnd)) 
	   (qvars (cdr fsnd))
	   (result (checkconsts consts)))
      (cond (consts
	     (if (and result qvars) (checkqvars qvars result) result))
	    (qvars (setq result (ns-to-nbs (value.sv 'nodes)))
		   (checkqvars qvars result))))))
 
; ==========================================================================
;
; checkconsts
; -----------
;
;       arguments     : consts - ((path . nbs) ... )
;
;       returns       : <boolean>
;
;       nonlocal-vars : result - <node bind set>
;
;       description   : find a <node bind set> which nodes satisfy the 
;                       specifications in consts with the binding.
;
;       side-effects  : result is the <node bind set> that satisfy the consts.
;
;       implementation: call pathto to get the <node bind set> which satisfy 
;                       the first element of consts, and then call pathto
;                       again for the rest elements of consts and do
;                       a intersection  of the new result with the old 
;                       result. Wheneven the result become empty, 
;                       return nil to indicate operation failure.
;
;                                          written  : CCC 11/21/83
;                                          modified : ssc  5/10/89
;
(defun checkconsts (consts)
  (do* ((result (pathto (car (first consts)) (cdr (first consts)))
		(intersect.nbs (pathto (car (first consts1))
                                       (cdr (first consts1)))
                               result))
	(consts1 (rest consts) (rest consts1)))
       ((null result) nil)
    (unless consts1 (return result))))
 
; ==========================================================================
;
; findeval-atom
; -------------
;
;       arguments     : a - <atom>
;
;       returns       : <node set>
;
;       description   : if "a" is the <node access> of a <node>, return
;                       a the corresponding <node set>, otherwise return
;                       a <new node set>.
;
;
;                                          written  : CCC 11/22/83
;                                          modified : ejm 08/02/84
;                                          modified : scs 06/08/87
;                                          modified : scs 02/05/88
;
(defun findeval-atom (a)
  (cond ((stringp a) (findeval-atom (string-to-symbol a)))
	((is.n a) (makeone.ns a))
	((node a) (makeone.ns (node a)))
	(t (new.ns))))
 
; ==========================================================================
;
; findeval-map
; ------------
;
;       arguments     : snepsul-exp - <snepsul-exp>
;
;       returns       : <node set>
;
;       description   : return a nodeset which satisfy the given sneps-exp
;
;       side-effects  : 
;
;       implementation: map and findeval-atom the elements of snepsul-exp one
;                       by one and make a union of them.
;
;                                          written  : CCC 11/25/83
;                                          modified : SCS 06/08/87
;
(defun findeval-map (snepsul-exp)
  (let ((result (new.ns)))
    (loop (if (null snepsul-exp) (return result))
          (cond ((numberp (first snepsul-exp))
                 (setq result
                       (union.ns result
                                 (findeval-atom
				   (un-ize (first snepsul-exp))))
                       snepsul-exp
                       (rest snepsul-exp)))
		((stringp (first snepsul-exp))
                 (setq result
                       (union.ns result
                                 (findeval-atom
				   (string-to-symbol (first snepsul-exp))))
                       snepsul-exp
                       (rest snepsul-exp)))
                ((atom (first snepsul-exp))
                 (setq result
                       (union.ns result
                                 (findeval-atom (first snepsul-exp)))
                       snepsul-exp
                       (rest snepsul-exp)))
                (t
                 (sneps-error " illegal snepsul expression "
                              'find
                              'findeval-map))))))
 
; ==========================================================================
;
; insidefind
; ----------
;
;       arguments     : snepsul-exp - <snepsul-exp>
;
;       returns       : <node bind set>
;
;       description   : call the find1 to get the <node bind set> which satisfy
;                       the given snepsul-exp.  This function is called
;                       from "find-inside" by literally assembling its name.
;
;                                          written  : CCC 11/22/83
;                                          comment modified: JG 3/3/86
;                                          modified : scs 06/15/89
;
(defmacro insidefind (&rest snepsul-exp)
   `(let ((crntct (processcontextdescr ',snepsul-exp)))
      (declare (special crntct))
      (find1 (fevalsnd (getsndescr ',snepsul-exp)))))
 
; ==========================================================================
;
; find-inside
; -----------
;
;       arguments     : snepsul-exp - <snepsul-exp>
;
;       returns       : <snepsul-exp>
;
;       description   : return a snepsul-exp which use functions "insidefind*"
;                       instead of "find*" functions.
;
;                                          written  : CCC 11/23/83
;                                          modified :
;
 
(defun find-inside (snepsul-exp)
    (let ((first-se (first snepsul-exp)) (rest-se (rest snepsul-exp)))
      (cond 
       ((string= (build-namestring first-se) 
		 (build-namestring :find)) 
	(cons (intern (build-namestring :inside first-se)
		      (find-package 'sneps))
	      rest-se))
       (t snepsul-exp))))
 
; ==========================================================================
;
; checknodesetform
; ----------------
;
;       arguments     : nsf - <nodesetform>
;
;       returns       : error if nsf is not a nodesetform, otherwise return 
;                       nsf.
;
;       description   : test the nsf is a nsf or not.
;
;       side-effects  : if nsf is not a nsf or relation, give a sneps error.
;
;       implementation: the function doesn't actully check the nsf is
;                       a nsf or not, if nsf is a list, the function
;                       will assume it is a nsf.
;                       Do it this way because it will take two many time
;                       to go through the whole list.
;
;                                          written  : CCC 11/10/83
;                                          modified : CCC 11/21/83
;
 
(defun checknodesetform (nsf)
    (declare (special nsf))
    (cond ((is.nsf nsf) nsf)
          (t (sneps-error (format nil " Illegal node set form: ~A" nsf)
			  'find
			  'checknodesetform))))
 



    
    




