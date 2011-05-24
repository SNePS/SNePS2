;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: hyp.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


;===========================================================================
;
;      Data Type : <snepsul-description>
;
;---------------------------------------------------------------------------
;
;      <snepsul-description> ::=
;
;      (<snepsul node description>)   |
;      (<snepsul node description> :context <svar>) |
;      (<snepsul node description> :context <snepsul expression> <svar>) |
;      (<snepsul node description> :context 'all-hyps)
;
; ==========================================================================
;
;    ATTENTION:
;
;      <snepsul node description> ::= <nodeset>* 
;
;                                                     
; ==========================================================================
;
;  legalcontext 
;  ------------
;
;
;      arguments     : ns - <nodeset>
;
;      returns       : <nodeset>
;
;      description   : Takes as argument a nodeset and verifies if all
;                      of them are assertions.
;
;
;
;      side-effects  : None.
;
;                                         written:  cpf 09/26/88
;                                         modified: njm/hc 05/10/89
;

(defun legalcontext (ns)
  "If all nodes in ns are assertions it returns ns; error otherwise"
  (if (issubset.ns ns (value.sv 'assertions))
      ns
      (sneps-error
	      (format nil "One or more nodes are not assertions ~A" ns)
	      'context
	      'legalcontext)))


; ==========================================================================
;
;  crntcontext 
;  -----------
;
;      arguments     : ctdescr - <s-expression>
;
;      returns       : <svar>
;
;      description   : It returns the name of the current context.
;                      Takes as argument a list with one of the forms:
;
;       1. ()
;       2. (<cname>)
;       3. (<snepsul expression> <cname>)
;       4. (all-hyps)
;      and returns:
;
;      if 1., the context name that is the value of the sys-svar DEFAULTCT;
;         2., the context name <cname>. If <cname> is not already a context
;             a new empty context with this name will be built.
;         3., the context name <cname> which will be set to the hypotheses
;             supplied in <snepsul expression>.
;         4., the context name  %%globalcontext%% containing  
;             all hypotheses introduced so far.
;
;
;      side-effects  : It can change contexts.
;
;                                         written:  cpf 09/26/88
;                                         modified: njm 10/13/88
;                                                   njm 05/11/89
;                                                   njm/hc 05/12/89
;
;
;
(defmacro crntcontext (ctdescr)
  "Returns the name of the current context from a context description"
  `(cond (;; Case 1:
	  (null ,ctdescr) (value.sv 'defaultct))
	 ;; Case 4:
	 ((eq (first ,ctdescr) 'all-hyps)
	  (set.sv '|%%globalcontext%%|
		  (fullbuildcontext
		    (legalcontext (value.sv 'assertions))
		    (new.cts)))
	  '|%%globalcontext%%|)
	 ;; Case 2:
	 ((and (symbolp (first ,ctdescr))
	       (null (cdr ,ctdescr)))
	  (unless (is.ct (value.sv (first ,ctdescr)))
	    ;; build a new empty context with this name
	    (name.ct (fullbuildcontext (new.ns) (new.cts))
		     (first ,ctdescr)))
	  (first ,ctdescr))
	 ;; Case 3:
	 ((and (symbolp (second ,ctdescr))
	       (null (cddr ,ctdescr)))
	  (name.ct (fullbuildcontext
		     (legalcontext (nseval (first ,ctdescr)))
		     (new.cts))   
		   (second ,ctdescr))
	  (second ,ctdescr))
	 (t (sneps-error
	      (format nil "Error in context description ~A" ,ctdescr)
	      'context
	      'crntcontext))))


;
; ==========================================================================
;
;  processcontextdescr
;  -------------------
;
;      arguments     : sd - <snepsul description> 
;
;      returns       : <svar>
;
;      description   : Takes as argument a list which is in one of the forms:
;
;         1a.  (<snepsul node description>)
;         1b.  (<snepsul node description> :context)
;         2.   (<snepsul node description> :context <cname>)
;         3.   (<snepsul node description> :context <snepsul expression> <cname>)
;         4.   (<snepsul node description> :context all-hyps)
;
;      and returns:
;
;      if 1., the context name that is the value of the sys-svar DEFAULTCT;
;         2., the context name <cname>. If <cname> is not already a context
;             a new empty context with this name will be built.
;         3., the context name <cname> which will be set to the hypotheses
;             supplied in <snepsul expression>.
;         4., the context name  %%globalcontext%% containing  
;             all hypotheses introduced so far.
;
;      side-effects  : It can change contexts.
;
;                                         written:  cpf 09/19/88
;                                         modified: hc  05/08/89
;                                                   njm/hc 05/12/89
;
;
;
(defun processcontextdescr (sd)
  "Returns the name of the current context from a snepsul description"
  (crntcontext (iscontext-given sd)))


;
; ==========================================================================
;
;  iscontext-given
;  ---------------
;
;      arguments     : sd - <snepsul description> 
;
;      returns       : <boolean>
;
;      description   : Takes as argument a list which is in one of the forms:
;
;         1a.  (<snepsul node description>)
;         1b.  (<snepsul node description> :context)
;         2.   (<snepsul node description> :context <cname>)
;         3.   (<snepsul node description> :context <snepsul expression> <cname>)
;         4.   (<snepsul node description> :context all-hyps)
;
;       It returns Nil in case 1. and the context description in cases 2.,3., and 4.
;                         
;
;                                         written:  cpf 09/19/88
;                                         modified: njm/hc 05/12/89
;
(defun iscontext-given (sd)
  "Returns T if the context is explicitly given in the snepsul description `sd',
   NIL otherwise."
  (cdr (member :context sd)))

;
; ==========================================================================
;
;  getsndescr
;  ----------
;
;      arguments     : sd - <snepsul description> 
;
;      returns       : <snepsul node description>
;
;      description   : It selects the snepsul node description from a
;                      snepsul description
;
;
;
;                                         written:  cpf 09/19/88
;                                         modified: njm 03/09/89
;                                                   hc  05/12/89
;
;
(defun getsndescr (sd)
  "Selects the snepsul node description from a snepsul description"
  (ldiff sd (member :context sd)))


;
; ==========================================================================
;
;  describe-context-1
;  ------------------
;
;
;      arguments     :  cname - <svar>
;
;      returns       :  no value.
;
;      side-effects  :  It prints a message containing the list of all
;                       assertions in context named `cname', the context
;                       restriction and all names of this context. Notice
;                       that one context can have more than one name.
;
;      modifications:   It's now describe-context-1, was list-context-1
;
;
;                                         written : cpf 09/29/88
;                                         modified: njm/hc 05/12/89
;                                                   
;
(defmacro describe-context-1 (cname)
  "Prints the assertions, context restriction and names of context `cname'"
  `(let ((crntct (value.sv ,cname)))
     (declare (special crntct))
     (format outunit
	     "~%~A~%"
	     (descrcontext crntct))
     (values)))

;
; ==========================================================================
;
;  describe-context
;  ----------------
;
;
;      arguments     :  cname - <svar> &optional
;
;      returns       :  no value.
;
; 
;      implementation: If no `cname' is given the value of the
;                      current default context is assumed. If 'cname'
;                      does not correspond to a context name   
;                      an error message is generated.
; 
;      side-effects  :  It prints a message containing the list of all
;                       assertions in context named `cname', the context
;                       restriction and all names of this context. Notice
;                       that one context can have more than one name.
;
;      modifications:   It's now describe-context, was list-context
;
;                                         written : cpf 09/29/88
;                                         modified: njm/hc 05/12/89
;                                                   hc  07/18/93
;
(defsnepscom describe-context ((&optional cname))
  "Prints all assertions, context restriction and names of context named CNAME"
  (declare (special outunit))
  (cond ((null cname) (describe-context-1 (value.sv 'defaultct)))
	((is.ct (value.sv cname))
	 (describe-context-1 cname))
	(t (sneps-error
	    (format nil "the following context is invalid: ~A"
		    cname)
	    'contexts
	    'describe-context))))

;
; ==========================================================================
;
;  set-context
;  -----------
;
;      arguments     :  hypset - <nodeset>
;                       cname - <svar>       &optional   
;
;      returns       :  no value
;
;      description   :  It buids a new <context> with the set of hypotheses 
;                       `hypset' and name it with `cname'.
;                       If a name is not supplied then it is assumed that
;                       it is a reference to the DEFAULTCT context.
;                       If there already exists a context named `cname', it
;                       removes that information from the old one.
;                       The new context is ckecked for inconsistency.
; 
;      implementation:  If one of the nodes in `hypset' is not an assertion   
;                       an error message is generated.
; 
;      side-effects  :  The side-effects of building contexts. It prints a
;                       message.
;
;
;                                         written : cpf 09/29/88
;                                         modified: mrc 10/19/88
;                                         modified: mrc 10/20/88
;                                                   njm 05/11/89
;                                                   hc  07/18/93
;
(defsnepscom set-context ((hypset &optional cname))
  "Builds a new context named CNAME defined by the nodeset HYPSET."
  (declare (special outunit))
  (let ((hyps (seq-to-ns (nseval hypset)))
	(ct-name (if (null cname)
		     (value.sv 'defaultct)
		   cname)))
    (if (issubset.ns hyps (value.sv 'assertions))
	(let ((newct (fullbuildcontext hyps (new.cts))))
	  (unless (snebr:ck-inconsistency newct)
	    (progn (name.ct newct ct-name)
		   (describe-context-1 ct-name))))
      (sneps-error
       (format nil "the following nodes are not hypotheses: ~A"
	       (compl.ns hyps (value.sv 'assertions)))
       'context
       'set-context))
    (values)))

;
; ==========================================================================
;
;  add-to-context
;  --------------
;
;
;      arguments     :  hypset - <nodeset>
;                       cname - <svar>       &optional
;
;      returns       :  no value
;
;      description   :  It adds to context named `cname', the assertions 
;                       within `hypset'. Calculates the restriction of 
;                       the new context.
;                       If a name is not supplied then it is assumed that
;                       it is a reference to the DEFAULTCT context.
; 
;      implementation:  If one of the nodes in `hypset' is not an assertion   
;                       an error message is generated.
; 
;      side-effects  :  The side-effects of building contexts. It prints a
;                       message.
;
;
;                                         written : cpf 09/29/88
;                                         modified: njm 10/13/88
;                                         modified: mrc 10/21/88
;                                                   njm 05/11/89
;                                                   hc  07/18/93
;
(defsnepscom add-to-context ((hypset &optional cname))
  "Adds the assertions of HYPSET to the context named CNAME."
  (declare (special outunit))
  (let ((hyps (seq-to-ns (nseval hypset)))
	(ct-name (if (null cname)
		     (value.sv 'defaultct)
		   cname)))
    (if (is.ct (value.sv ct-name))
	(if (issubset.ns hyps (value.sv 'assertions))
	    (let ((newct (fullbuildcontext
			  hyps (makeone.cts (value.sv ct-name)))))
	      (unless (snebr:ck-inconsistency newct)
		(progn (name.ct newct ct-name)
		       (describe-context-1 ct-name))))
	  (sneps-error
	   (format nil "the following nodes are not hypotheses: ~A"
		   (compl.ns hyps (value.sv 'assertions)))
	   'context
	   'add-to-context))
      (sneps-error
       (format nil "the value of ~A is not a context" ct-name)
       'context
       'add-to-context))))

;
; ==========================================================================
;
;  remove-from-context
;  -------------------
;
;
;      arguments     :  hypset - <nodeset>
;                       cname - <svar>       &optional   
;
;      returns       :  no value
;
;      description   :  It removes from context named `cname', the assertions 
;                       within `hypset'. Calculates the restriction of 
;                       the new context.
;                       If a name is not supplied then it is assumed that
;                       it is a reference to the DEFAULTCT context.
; 
;      implementation:  If one of the nodes in `hypset' is not an assertion   
;                       an error message is generated.
; 
;      side-effects  :  The side-effects of building contexts. It prints a
;                       message.
;
;
;                                         written : hc/njm 05/10/89
;                                         modified: njm    05/11/89
;                                                   hc     07/18/93
;                                                   scs    /05/28/07
;
(defsnepscom remove-from-context ((hypset &optional cname))
  "Removes the assertions in HYPSET from the context named CNAME."
  (declare (special outunit))
  (let ((hyps (seq-to-ns (nseval hypset)))
	(ct-name (if (null cname)
		     (value.sv 'defaultct)
		   cname)))
    (if (is.ct (value.sv ct-name))
	(if (issubset.ns hyps (context-hyps (value.sv ct-name)))
	    (let ((newct (fullbuildcontext
			  (compl.ns (context-hyps (value.sv ct-name)) hyps)
			  (new.cts))))
	      (unless (snebr:ck-inconsistency newct)
		(progn (name.ct newct ct-name)
		       (describe-context-1 ct-name))))
	  (sneps-error
	   (format nil "the following nodes do not belong to the context: ~A"
		   (compl.ns hyps (context-hyps (value.sv ct-name))))
	   'context
	   'remove-from-context))
      (sneps-error
       (format nil "the value of ~A is not a context" ct-name)
       'context
       'remove-from-context))))

;
; ==========================================================================
;
;  list-context-names
;  ------------------
;
;      arguments     :  NIL
;
;      returns       :  no value.
;
;      description   :  It prints all existing context names.
;
;      side-effects  :  It print a message.
;
;                                         written : cpf 09/29/88
;                                         modified: hc  07/18/93
;
(defsnepscom list-context-names (())
  "Prints all names of all existing contexts."
  (declare (special outunit))
  (let ((allnames nil))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (setq allnames (append (context-names val)
					allnames)))
	     (value.sv 'contexts))
    (format outunit "~%~A~%" allnames)
    (values)))

;
; ==========================================================================
;
;  set-default-context
;  ------------------
;
;      arguments     :  cname - <svar>
;
;      returns       :  no value.
;
;      description   :  Sets the sys variable DEFAULTCT to CNAME 
;
;      side-effects  :  It print a message.
;
;                                         written : njm 05/11/89
;                                         modified: hc  07/18/93
;
(defsnepscom set-default-context ((cname))
  "Sets the SNePSUL variable `defaultct' to CNAME."
  (declare (special outunit crntct))
  (if (is.ct (value.sv cname))
      (progn (set.sv 'defaultct cname)
	     (setf crntct cname)
	     (describe-context-1 cname)
	     (values))
    (sneps-error
     (format nil "the following context is invalid: ~A" cname)
     'context
     'set-default-context)))

;
; ==========================================================================



    
    




