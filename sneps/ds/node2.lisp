;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: node2.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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



; =============================================================================
;
; Test Functions
;
; =============================================================================

(defun isnodeid (id)
  "Returns T if `id' is a legal node identifier prefix,
      viz. one of: v, p, b, tm, tv, tp.
      Returns NIL otherwise."
  ;; written:  ejm 7/26/83
  (get 'gennewnode id))

(defun istempnodeid (id)
  "Returns T if `id' is a legal identifier prefix for a temporary node,
   viz: tm, tv, or tp.
   Returns NIL otherwise."
  ;; written:  ejm 7/26/83
   (member id '(tm tv tp)))
;
; ==============================================================================
;
; Auxiliary Functions
;
; ==============================================================================

(defun incridcounter (id)
  "Increments the integer suffix associated with the identifier prefix `id'.
   Returns `id'."
  ;; written:  ejm 7/26/83
   (setf (get 'gennewnode id)
          (cl:+ (get 'gennewnode id) 1)))

(defun idcounter (id)
  "Returns the integer last used for a node identifier with prefix `id'."
  ;; written:  ejm 7/26/83
  (get 'gennewnode id))

; ==============================================================================
;
; Node Counter                    ;hi 3/28/99
;
; ==============================================================================

(let (*Node-Counter*)
  
  (defun get-node-counter ()
    *Node-Counter*)
  
  (defun reset-node-counter ()
    (setf *Node-Counter* 0))
  
  (defun set-node-counter (count)
    (setf *Node-Counter* count))
  
  (defun inc-node-counter ()
    (incf *Node-Counter*)))

; ==============================================================================
;
; Constructor Functions
;
; ==============================================================================
;

; ==============================================================================
;
; node
; ----
;
;        arguments     : ident - <identifier> 
;
;        returns       : <node> or NIL
;
;        description   : Given an <identifier> "ident", it returns the
;                        the <node> whose <identifier> is "ident".
;
;                                        written:  ejm 7/26/83
;                                        modified: ssc 11/04/87
;                                                  scs    ? 
;                                                  (hc,njm 3/29/89)
;                                                  hc 10/2/90
;

;; This is a kludge: On the TI-Explorers GET also takes a list instead
;; of a symbol. SNePSLOG uses that in various places, and until it
;; gets cleaned up we catch that case here. (hc, 10/1/90)


;;; Update on 6/20/04 by SCS
;;; Previously, if node were given a symbol to convert into a node,
;;;   it would do the following:
;;;      1.  If the symbol already names a node, return that node.
;;;      2.  If the symbol, converted to all lower case letters
;;;             in the snepsul package names a node, return that node.
;;;      3.  If the symbol, converted to capitalized initial letter and lower
;;;             case subsequent letters in the snepsul package names a node,
;;;             return that node.
;;;      4.  Else create a node with the symbol as its name.
;;; The following version of node will, instead, simply do the following:
;;;      1.  If the symbol already names a node, return that node.
;;;      2.  Else create a node with the symbol as its name,
;;;             retaining the package and case conventions of the symbol.

(defun node (ident
	     &aux
	     (atomicIdent
	      (if (consp ident) (first ident) ident))
	     (name
	      (typecase atomicIdent
		(node atomicIdent)
		(symbol atomicIdent)
		(number (un-ize atomicIdent))
		(string (intern atomicIdent))
		(t (sneps-error
		    (format nil
			    "Function node called with ~s, ~
                  which is neither a node, a symbol, a number, nor a string." 
			    atomicIdent)
		    'sneps 'node)))))
  (get name '=snode))


;
(defun newnode (ident type &optional order)
  "Creates and returns a new permanent node with identifier `ident' and type `type'.
      `Type must be one of: :base :mol :var :pat.
   Returns NIL if a node with that identifier already exists."
  ;; written:  ejm 7/26/83
  ;; modified: scs 2/13/87, hi 3/28/99
  (unless (node ident)
     (let ((n (make-node :na ident 
			 :order (or order (inc-node-counter)) 
			 :type type 
			 :perm t)))
       (setf (get ident '=snode) n)
       (set.sv 'nodes (insert.ns n (value.sv 'nodes)))
       n)))

(defun newtempnode (ident type &optional order)
  "Creates and returns a new temporary node with identifier 'ident' and type 'type'.
   'type' must be one of: :mol :var :pat."
;;written : ejm 07/26/83
  ;;modified : scs 02/11/87, hi 3/28/99
  (make-node :na ident 
	     :order (or order (inc-node-counter))
	     :type type))

;
(defun newpbase.n (ident)
  "Creates and returns a new permanent base node whose identifier is `ident'.
   Returns NIL if one such already exists."
  ;; written:  ejm 7/29/83
  (newnode ident :base))

;
(defun gennewnode (id type)
  "Generates and returns a new node of type `type' and identifier `id'#
      `id' must be one of: b, m, v, p, tm, tv, tp.
      `type must be one of: :base :mol :var :pat."
  ;; written:  ejm 7/26/83
  ;; modified: scs 2/11/87
  ;;           ssc 5/10/89
  (cond ((isnodeid id)
	  (cond ((istempnodeid id)
		 (incridcounter id) 
		 (newtempnode
		   (intern
		     (concatenate 'string 
				  (symbol-name id)
				  (prin1-to-string (idcounter id)))
		     :snepsul)
		   type))
		(t (do ((result nil
				(newnode (intern
					  (concatenate
					      'string
					    (symbol-name id)
					    (prin1-to-string (idcounter id)))
					  :snepsul)
					 type)))
		       (result result)
		     (incridcounter id)))))
	 (t (error "function gennewnode -- wrong id -- ~s " id))))

;;
;; Keeps track of the last integer used to form a node identifier with
;; given prefix.
;;
;;
;;                      modified:  njm  09/27/88
;;
(mapc #'(lambda (pname value) (setf (get 'gennewnode pname) value))
      '(b m v p tm tv tp) '(0 0 0 0 0 0 0))

(defun genpbase.n ()
  "Creates and returns a new permanent base node with a system generated identifier."
  ;; written:  ejm 7/29/83
  ;; modified: scs 2/20/87
  (gennewnode 'b :base))

(defun genpmol.n ()
  "Creates and returns a new permanent molecular node with system generated identifier."
  ;; written:  ejm 7/29/83
  ;; modified: scs 2/20/87
   (gennewnode 'm :mol))

(defun genpvar.n ()
  "Creates and returns a new permanent variable node with system generated identifier."
  ;; written:  ejm 07/29/83
  ;; modified: ejm 10/26/83
  ;; modified: scs 02/20/87
  (let ((varn (gennewnode 'v :var)))
     (set.sv 'varnodes (insert.ns varn (value.sv 'varnodes)))
     varn))

(defun genppat.n ()
  "Creates and returns a new permanent pattern node with system generated identifier."
  ;; written:  ejm 7/29/83
  ;; modified: scs 02/20/87
  ;; modified: scs 04/15/89
  (let ((patn (gennewnode 'p :pat)))
     (set.sv 'patterns (insert.ns patn (value.sv 'patterns)))
     patn))

(defun gentmol.n ()
  "Creates and returns a new temporary molecular node."
  ;; written  ejm 7/29/83
  ;; modified: scs 02/20/87
  (gennewnode 'tm :mol))

(defun gentvar.n ()
  "Creates and returns a new temporary variable node."
  ;; written:  ejm 7/29/83
  ;; modified: scs 2/20/87
  (gennewnode 'tv :var))
;
;
; ==============================================================================
;
; gentpat.n
; ---------
;
;       arguments     : none
;
;       returns       : <temporary pattern node>
;
;       description   : It creates a new <temporary pattern node>.
;
;       side-effects  : see function gennewnode
;
;                                        written:  ejm 7/29/83
;                                        modified:
;
;

(defun gentpat.n ()
   (gennewnode 'tp :pat))

;
;
; ==============================================================================
;
; is.n 
; ----
;
;        arguments      : u - <universal>
;
;        returns        : <boolean>
;
;        description    : It returns "true" if "u" is a <node>, or "false"
;                         otherwise.
;
;                                        written:  ejm 7/26/83
;                                        modified: scs 2/11/87
;
;
(defun is.n (u)
  (node-p u))
					
;
; ==============================================================================
;
; isperm.n 
; --------
;
;        arguments      : n - <node> 
;
;        returns        : <boolean>
;
;        description    : It returns "true" if "n" is a <permanent node>, or
;                         "false" otherwise.
;
;                                        written:  ejm 7/26/83
;                                        modified: scs 2/11/87
;
;
(defun isperm.n (n)
   (node-perm n))
;
;
; ==============================================================================
;
; isbase.n
; --------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if "n" is a <base node>,
;                       or "false" otherwise.
;
;                                        written:  ejm 7/29/83
;                                        modified: scs 2/11/87
;
;
(defun isbase.n (n)
   (eq (node-type n) :base))
;
;
; ==============================================================================
;
; ismol.n
; -------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if "n" is a <molecular node>,
;                       or "false" otherwise.
;
;                                        written:  ejm 7/29/83
;                                        modified: scs 2/11/87
;
;
(defun ismol.n (n)
     (eq (node-type n) :mol))
;
;
; ==============================================================================
;
(defun isvar.n (n)
  "Returns t if n is a <variable node>;
      else returns nil."
  (and (is.n n) (eql (node-type n) ':var)))
;
; ==============================================================================
;
; ispat.n
; -------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if "n" is a <pattern node>, or
;                       "false" otherwise.
;
;                                        written:  ejm 7/29/83
;                                        modified: scs 2/11/87
;
;
(defun ispat.n (n)
   (eq (node-type n) :pat))



; ==============================================================================
;
(defun node-to-number.n (n)
  "If the name of the node n looks like a number, returns the number."
  ;; written: scs 6/20/88
  ;; modified scs 08/10/06
  (let ((x (node-to-lisp-object n)))
    (if (numberp x)
	x
      (error "Non-number node passed to node-to-number.n: ~S" x))))
;
; ==============================================================================
;
; isnumber.n
; ----------
;
;       arguments     : n - <node>
;
;       returns       : <boolean.
;
;       description   : It returns "true" if "n" is a <number node>, or
;                       "false" otherwise.
;
;                                        written :  ejm 10/03/83
;                                        modified:  scs 02/11/87
;                                        modified:  scs 09/29/88
;                                        modified:  scs 08/10/06
;
(defun isnumber.n (n)
  ;; n is a <number node> iff the entire symbol-name of its node access forms a
  ;; number.
  (numberp (node-to-lisp-object n)))
	 

;(defmacro isnumber.n (n)
;   `(numberp (node-to-number.n ,n)))
;
; ==============================================================================
;
; nodeset.n
; ---------
;
;        arguments     : n - <node>
;                        r - <relation> 
;
;        returns       : <node set>
;
;        description   : It returns the <node set> related to the <node> "n"
;                        by the <relation> "r".
;
;                                        written:  ejm 7/26/83
;                                        modified: scs 2/13/87
;
;
(defun nodeset.n (n r)
   (getnodeset.fcs r (node-fcableset n)))
;
;
; =============================================================================
;
(defun quantifier-of.n (v)
  "Returns the quantifier arc that quantifies the variable node v, or NIL, if none."
  ;; written by: scs 4/22/88
  (or (and (nodeset.n v 'forall-) 'forall)
      (and (nodeset.n v 'exists-) 'exists)
      (and (nodeset.n v 'vars-) 'vars)
      (and (nodeset.n v 'pevb-) 'pevb)))
; ==============================================================================
;
; setnodeset.n
; ------------
;
;        arguments     : n  - <node>
;                        r  - <relation>
;                        ns - <node set> 
;
;        returns       : <node>
;
;        description   : It adds the <cable> <"r", "ns">
;                        to the cableset of <node> "n". 
;
;        side-effects  : It side effects "n". If the <relation> "r" was
;                        already in "n" the corresponding <node set>
;                        is replaced by the new one. If "ns" is a <new nodeset>
;                        the pair "r-ns" is removed from "n".
;
;                                        written:  ejm 07/26/83
;                                        modified: scs 02/13/87
;                                                  ssc 04/24/87
;                                                  hc   6/07/90
;
(defun setnodeset.n (n r ns)
  (setf (node-fcableset n)
         (cond ((isnew.ns ns)
	        (delete.fcs r (node-fcableset n)))
	       (t (replace.fcs r ns (node-fcableset n))))));
;
; ==============================================================================
; setfreevars.n
; -------------
;
;        arguments     : n - <node> 
;                        freevars - <node set>
;
;        returns       : <node>
;
;        description   : It inserts the <node set> containing the free
;                        variables dominated by <node> "n" in the 
;                        <node>'s information.
;                           
;
;                                        written:  ejm 09/30/83
;                                        modified: scs 02/13/87
;
;
(defun setfreevars.n (n freevars)
  (setf (node-freevars n) freevars)
  n)
;
;
; ==============================================================================
;
; updatenodeset.n
; ---------------
;
;        arguments     : n  - <node>
;                        r  - <relation> 
;                        newn - <node> 
; 
;        returns       : <node>
;
;        description   : Inserts "newn" in the <node set> related to "n" by "r"
;                        if the <node> "newn" is not already there.
;
;        side-effects  : It side effects the <node> "n".
;
;                                        written:  ejm 7/26/83
;                                        modified: scs 2/13/87
;;;                                                ssc 2/21/87
;
;
(defun updatenodeset.n (n r newn)
  (setf (node-fcableset n)
	 (insert.fcs r 
		     (insert.ns newn (new.ns))
		     (node-fcableset n))))
;
;
; ==============================================================================
;
; freevars.n
; ----------
;
;        arguments     : n - <node> 
;
;        returns       : <node set>
;
;        description   : It returns the <node set> containing the free
;                        variables dominated by <node> "n".
;                           
;
;                                        written:  ejm 7/26/83
;                                        modified: scs 2/13/87
;
;
(defun freevars.n (n)
   (or (node-freevars n)
        (and (isvar.n n)
	     (list n))))
;
;
; ==============================================================================
;
; iseq.n
; ------
;
;       arguments     : n1 - <node>
;                       n2 - <node>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if n1 and n2 are the same node,
;                       "false" otherwise.
;
;                                        written:  ejm 8/10/83
;                                        modified:
;
;
(defun iseq.n (n1 n2)
  (eq n1 n2))
;
;
; ==============================================================================
; 
; ==============================================================================
;
; isless.n
; --------
;
;       arguments     : n1 - <node>
;                       n2 - <node>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if n1 compares as less than n2;
;                       "false" otherwise.
;
;                                        written:  scs 06/06/87
;                                        modified:  hi 03/20/99
;
;
(defun isless.n (n1 n2)
  (< (node-order n2) (node-order n1)))
;
;
; ==============================================================================
; ==============================================================================
;
;   The following functions are particular to this implementation:
;
; ==============================================================================
;
; nodeaccess
; ----------
;
;        arguments     : n - <node> 
;
;        returns       : <atom>
;
;        description   : It returns the interned <atom> used to access the 
;                        uninterned atom representing the <node> "n".
;
;                                        written:  ejm 7/26/83
;                                        modified: scs 2/13/87
;
;
(defun nodeaccess (n)
  (node-na n))

; ==============================================================================
;
; Print-description.n
; -------------------
;
;       arguments     : n - <node>
;                       outunit - <unit>
;
;       returns       : nil
;
;       description   : It prints a description of "n" into "outunit" --
;                       a <dotted pair> composed of a description of
;                       the  <node> "n" itself and of a description  of
;                       its <flat cable set>.
;                       Not to be confused with the SNePSUL function
;                       "describe" which describes also the dominated
;                       <node>s.
;
;       side-effects  : It prints a description of the <node> into "outunit".
;
;                                        written :  ejm 10/04/83
;                                        modified:  scs 02/20/87
;                                                   ssc 02/21/87
;
(defun Print-description.n (n outunit)
  (format outunit "~A~%" (cons n (node-fcableset n))))

;
; ==============================================================================
;
; describe.n
; ----------
;
;       arguments     : n - <node>
;
;       returns       : <atom>
;
;       description   : It returns an  <atom> which is a description of
;                       the  <node> "n" to be printed.
;                       The description includes a "!" if n is an
;                       <assertion node>.
;
;                                        written :  ejm 06/05/84
;                                        modified:  scs 02/20/87
;
(defun describe.n (n) n)

;;;
;;; =============================================================================
;;;
;;; activation.n
;;; ------------
;;;
;;;       arguments     : n - <node>
;;;
;;;       returns       : <process>
;;;
;;;       description   : returns the activation process of "n"
;;;                       (assumes that "n" does have one)
;;;                       as of 11/13/06 activates n if it's not already activated.

;;;
;;;                                        written :  rgh 11/11/85
;;;                                        modified: scs 11/13/06
;;;
;;;
(defun activation.n (n)
  (activate.n n)
  (node-activation n))

;
; =============================================================================
;
; activated.n
; -----------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" has an activation process
;                        already attached, "false" otherwise
;
;                                        written :  rgh 11/11/85
;                                        modified:
;
;
(defun activated.n (n)
  (node-activation n))

; =============================================================================
;
; activate.n
; ----------
;
;       arguments     : n - <node>
;
;       description   : attaches an activation process to the <node> "n" if
;                        it does not already have one
;
;       side-effects  : affects the function cell of "n"
;
;       implementation: the process-id of the activation process is stored
;                        in the function cell of the uninterned atom which
;                        represents the <node> "n"
;
;                                        written :  rgh 11/11/85
;                                        modified:
;
;
(defun activate.n (n)
  (cond ((not (activated.n n))
	  (setf (node-activation n) (snip::node-to-process n)))))

(defun deactivate.n (node)
  "Sets the activation of NODE to nil and frees process registers."
  (when (activated.n node)
    (let ((activation (node-activation node)))
;;;      (format t "*** Deactivating node ~s, whose activation, ~s is:~%"
;;;	      node activation)
;;;      (multi::print-regs activation)
      (setf (node-activation node) nil)
;;;      (when (multi:is-process-name activation)
;;;	;; Unbind process name to make register values garbage-collectible
;;;	(makunbound activation))
      )))

;;; New version of activate-act.n
;;; Modified from old by flj on 2/12/04 (code provided by scs on 12/8/03)
(defun activate-act.n (n)
  "Node N is activated as an act
   without checking to see if it has a action arc,
   or to see if it has another activation."
  (setf (node-activation n) (snip::make-act n)))


;
;
; =============================================================================
;
; quantified-vars.n
; -----------------
;
;       arguments     : n - <node>
;
;       returns       : <node set>
;
;       description   : returns the <node set> containing the variable
;                       nodes bound by the quantifier (if any) emanating
;                       from the <node> "n"
;
;                                        written :  rgh 10/06/85
;                                        modified:
;
;
(defun quantified-vars.n (n)
  (let ((av (nodeset.n n 'forall))
         (ev (nodeset.n n 'exists))
	 (pvar (nodeset.n n 'vars))
         (pev (nodeset.n n 'pevb)))
      (cond ((not (isnew.ns av)) av)
            ((not (isnew.ns ev)) ev)
	    ((not (isnew.ns pvar)) pvar)
            ((not (isnew.ns pev)) pev)
            (t (new.ns)))))

(defun all-vars.n (n)
  ;;"get all variables specified in a pattern node"
  (union.ns (freevars.n n) (quantified-vars.n n)))
;
;
; =============================================================================
;
; dominates.n
; -----------
;
;       arguments     : m1 - <node>
;                       m2 - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "m1" dominates "m2",
;                               "false" otherwise
;
;                                        written :  rgh 07/31/85
;                                        modified:  rgh 08/02/85
;
;
(defun dominates.n (m1 m2)
  (do* ((rem-cs (n-to-downcs m1) (others.cs rem-cs))
	(c (choose.cs rem-cs) (choose.cs rem-cs)))
       ((or (null rem-cs)
	    (do* ((rem-ns (nodeset.c c) (others.ns rem-ns))
		  (n (choose.ns rem-ns) (choose.ns rem-ns)))
		 ((or (null rem-ns)
		      (iseq.n n m2)
		      (dominates.n n m2))
		  rem-ns)))
	rem-cs)))
;
;
; =============================================================================
;
; is-v-ent.n
; ----------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" is an or-entailment node,
;                               "false" otherwise
;
;                                        written :  rgh 11/11/85
;                                        modified:
;
;
(defun is-v-ent.n (n)
  (not (isnew.ns (nodeset.n n 'ant))))
;
;
; =============================================================================
;
; is-&-ent.n
; ----------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" is an and-entailment,
;                       "false" otherwise
;
;                                        written :  rgh  3/31/86
;                                        modified:
;
;
(defun is-&-ent.n (n)
  (and (not (isnew.ns (nodeset.n n '&ant)))
        (isnew.ns (nodeset.n n 'thresh))
        (isnew.ns (nodeset.n n 'pevb))))
;
;
; =============================================================================
;
; is-thresh.n
; -----------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" is a thresh rule, "false"
;                       otherwise
;
;                                        written :  rgh  3/31/86
;                                        modified:
;
;
(defun is-thresh.n (n)
  (and (not (isnew.ns (nodeset.n n 'thresh)))
        (not (isnew.ns (nodeset.n n 'arg)))))
;
;
; =============================================================================
;
; is-and-or.n
; -----------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" is an and-or, "false" otherwise
;
;                                        written :  rgh  3/31/86
;                                        modified:
;
;
(defun is-and-or.n (n)
  (not (isnew.ns (nodeset.n n 'min))))

(defun is-nor.n (n)
  "Returns T if the node represents a NOR, NIL otherwise."
  (let ((maxargs (nodeset.n n 'max)))
     (and maxargs (zerop (node-to-number.n (choose.ns maxargs))))))

(defun is-and.n (n)
  "Returns T if the node represents an AND, NIL otherwise."
  ;; written: scs 6/21/88
  (let ((minargs (nodeset.n n 'min))
	 (tot (cardinality.ns (nodeset.n n 'arg))))
     (and minargs tot (cl:= (node-to-number.n (choose.ns minargs)) tot))))



;
;
; ============================================================================
;
; is-not.n 
; ----
;
;        arguments      : node 
;
;        returns        : <boolean>
;
;        description    : Returns t if node represents a NOT, nil otherwise.
;
;
;                                        written:  scs/flj  6/20/04
;
;
;
(defun is-not.n (node)
  "Returns t if node represents a NOT, nil otherwise."
  (and (is-nor.n node)
       (cl:= 1 (cardinality.ns (nodeset.n node 'arg)))))






;
;
; =============================================================================
;
; is-num-ent.n
; ------------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "n" is a numerical entailment,
;                       "false" otherwise
;
;                                        written :  rgh  3/31/86
;                                        modified:
;
;
(defun is-num-ent.n (n)
  (and (not (isnew.ns (nodeset.n n 'thresh)))
        (not (isnew.ns (nodeset.n n '&ant)))))
;
;
; =============================================================================
;
; is-num-quant.n
; --------------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "n" is numerically quantified,
;                       "false" otherwise
;
;                                        written :  rgh  3/31/86
;                                        modified:
;
;
(defun is-num-quant.n (n)
  (not (isnew.ns (nodeset.n n 'pevb))))
;
;
; =============================================================================
;
; is-non-deriv.n
; --------------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" is "n" is a rule stating the
;                       non-derivability of its argument, "false" otherwise
;
;                                        written :  rgh  3/31/86
;                                        modified:
;
;
(defun is-non-deriv.n (n)
  (not (isnew.ns (nodeset.n n 'i-//-))))

(defun is-rule.n (n)
  (or (is-v-ent.n n)
      (is-&-ent.n n)
      (is-and-or.n n)
      (is-thresh.n n)
      (is-num-ent.n n)
      (is-num-quant.n n)))
;
;
; =============================================================================
;
(defun symbol-type.n (node-name)
  "Determines the type of a node with NODE-NAME."
  (let* ((node-name (symbol-name node-name)))
    (if (and (member (aref node-name 0) '(#\M #\P #\V))
	     (cl:>= (length node-name) 2)
	     (every #'digit-char-p (subseq node-name 1)))
	(case (aref node-name 0)
	  (#\M :MOL)
	  (#\P :PAT)
	  (#\V :VAR))
      :BASE)))

(defun n^ (node-name)
  ;; Yet another node referencer (used for forward references during printing).
  ;; If a node with NODE-NAME does not yet exist then a new node gets built
  ;; with its type inferred from NODE-NAME.
  (or (node node-name)
      (newnode node-name (symbol-type.n node-name))))

;
; =============================================================================
;
(defun copy-assertion-state.n (oldnode newnode)
  "If oldnode is asserted, makes newnode asserted.
   Returns newnode."
  ;; written by:  scs 5/12/88
  ;; modified by: njm 11/23/88
  (declare (special crntct))
  (when (isassert.n oldnode)
    (snip:addsupport.n (filter.ctcs (node-asupport oldnode)
				    (if (is.ct crntct) crntct (value.sv crntct)))
		       newnode))
  newnode)

;
; =============================================================================
;
;
; newjust
; --------
; 
;
;       arguments     : n - <node>
;                       ot    - <otag>
;                       ct    - <context>
;
;       returns       : <node>
;
;       description   : Takes as arguments a <node> `n', an <otag> `ot' and a 
;                       <context> "ct". 
;
;                       It adds a new derivation to the node `n', with 
;                       origin tag `ot' and context `ct'.
;
;                       Returns `n'.
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun newjust (n ot ct) 
  (setf (node-asupport n)
	 (insert.ctcs ot ct (node-asupport n)))
  n)
;
; =============================================================================
;
; is-act.n
; ----------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" an act node
;                               "false" otherwise
;
;                                        written :  dk 4/17/91
;                                        modified:
;
;
(defun is-act.n (n)
  (not (isnew.ns (nodeset.n n 'action))))
;
;==============================================================================
;
; is-do-if.n
; ----------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" a do-if transformer (rule) node
;                               "false" otherwise
;
;                                        written :  dk 6/2/93
;                                        modified:
;
;
(defun is-do-if.n (n)
  (not (isnew.ns (nodeset.n n 'if))))
;
;==============================================================================
;
; is-when-do.n
; ------------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" a when-do transformer (rule) node
;                               "false" otherwise
;
;                                        written :  dk 6/3/93
;                                        modified:
;
;
(defun is-when-do.n (n)
  (not (isnew.ns (nodeset.n n 'when))))


;
;==============================================================================
;
; is-whenever-do.n
; ----------------
;
;       arguments     : n - <node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "n" is a whenever-do transformer 
;                       (rule) node "false" otherwise.
;
;                                        written :  hi 3/31/99
;                                        modified:
;
;
(defun is-whenever-do.n (n)
  (not (isnew.ns (nodeset.n n 'whenever))))

; =============================================================================
;
; negate.n
; ----------
;
;       arguments     : n - <node>
;
;       description   : Returns a node that is the negation of node.
;                        
;
;                                        written :  scs/flj 06/27/04
;                                        modified:  scs/flj 08/01/04
;
;

(defun negate.n (n)
  "Returns a node that is the negation of node n."
  ;; Produces a non-nested result
  ;;   for all instances of and-or and of thresh 
  (choose.ns
  (find-or-build
   (putin.cs
    (new.c 'min (makeone.ns (lisp-object-to-node 0)))
    (putin.cs
     (new.c 'max (makeone.ns (lisp-object-to-node 0)))
     (putin.cs (new.c 'arg (makeone.ns n)) (new.cs)))))))


(defun other-contradictory-nodes.n (p)
  "Returns a nodeset of nodes that contradict the node p,
    other than that node returned by (negate.n p)."
  (union.ns
   ;; andor(0,0){... P ...} but not andor(0,0){P}
   (remove-if-not.ns
    #'(lambda (node)
	(and (is-nor.n node)
	     (not (is-not.n node))))
    (nodeset.n p 'arg-))
   ;; If P is andor(0,0){Q1, ..., Qn}, then Q1, ..., Qn
   (if (is-nor.n p)
       (nodeset.n p 'arg)
     (new.ns))))


;;; This version doesn't build any nodes,
;;;    just finds any contradictory nodes that are already built.
(defun contradictory-nodes.n (p &aux (notp (find-negate p)))
  "Returns a nodeset of extant nodes that contradict the node p."
  (if notp
      (insert.ns notp (other-contradictory-nodes.n p))
    (other-contradictory-nodes.n p)))



