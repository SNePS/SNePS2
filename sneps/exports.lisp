;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: exports.lisp,v 1.2 2013/08/28 19:07:24 shapiro Exp $

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





;;; added eval-when 9-02 FLJ
;;;(eval-when (compile load) 
  (in-package :snepsul)
;;;  )


(export '(r a c k u q s n d yes y ok sure no end))  

;; added eval-when 9-02 FLJ
;;;(eval-when (compile load) 
  (in-package :sneps)
;;;  )

;;;  Export the following symbols from the SNePS package.

(export '(sneps snepslog))

(export '(|#| ^ |=| |*| % |!| $ ? + - build assert define find find-or-build
	      find findassert findconstant findpattern findbase findvariable
	      clear-infer topsneval add resetnet erase lisp demo demo-internal
	      set-context set-default-context describe-context list-context-names
	      add-to-context copyright))
	      
(export '(defsnepscom undefsnepscoms
	  ;; and for backward compatibility:
	  =command =topcommand))

(export '(inunit outunit))

(export '(isbase.n ismol.n isvar.n ispat.n is.n isnumber.n isassert.n describe.n
	  nodeset.n iseq.n freevars.n assert.n
	  activate.n quantified-vars.n activation.n dominates.n activated.n
          activate-act.n is-v-ent.n
	  is-&-ent.n is-thresh.n is-and-or.n is-num-ent.n is-num-quant.n is-non-deriv.n
	  dominatednodes.n node-to-number.n is-nor.n is-and.n is-act.n
	  is-do-if.n is-when-do.n n-to-downcs pp-nodetree))

(export '(new.ns isnew.ns ismemb.ns choose.ns others.ns makeone.ns union.ns do.ns
	  insert.ns cardinality.ns apply-subst.ns remove.ns remove-if-not.ns
	  in-context.ns iseq.ns issubset.ns
	  new.cs insert.cs apply-subst.cs clean-quantifiers.cs issubset.cs))

(export '(pathfrom checkpath evalsnd protect-eval pseudolisp initialized-p
	  node-fcableset node-type node-freevars node-perm node-p node-na node-snepslog
	  copy-assertion-state.n slight-surface sneps-error surface
	  is.r new.r isdn.r set.sv value.sv isnew.svs 
	  new.fcs insert.fcs delete.fcs others.fcs relation.fcs do.fcs
	  down.fcs relationset.fcs downfcs.pbi))

(export '(new.cts isnew.cts ismemb.cts choose.cts others.cts makeone.cts union.cts
	  insert.cts cardinality.cts remove.cts issubset.cts repeat name.ct
	  value.sv isassert.n context-hyps context-restriction 
	  fullbuildcontext compl.ns
	  node-asupport node n^ c^ default-defaultct all-hyps
	  new.c relation.c nodeset.c
	  hyp der ext assertion restriction named snip crntct
	  is.ct iseq.ct isinconsis.ct issubset.ct
	  cts make.cts do.cts compl.cts isnew.ctcs is.ctcs 
	  new.ctcs insert.ctcs others.ctcs ot.ctcs contextset.ctcs
	  getcontextset.ctcs filter.ctcs ctcs-to-cts ctcs-to-ots
	  getsndescr processcontextdescr
	  getcontext nodeaccess update-contexts ok-update-contexts buildcontext
	  mark-inconsistent mark-okinconsistent updateall newjust context-names
	  ))

(export '(define-path undefine-path converse compose kstar kplus
	  relative-complement irreflexive-restrict exception
	  domain-restrict range-restrict))

(export '( nodes patterns varnodes variables defaultct commands topcommands
	  assertions bnscommands fnscommands rscommands nscommands command))

(export '(;; reader macros
	  star-reader dollar-reader percent-reader hash-reader
	  question-reader bang-reader equal-reader))

(export '(;; demo-tool user interface
	  with-demo-control demo-set demo-get demo-start
	  *demo-interactive-io*
	  ;; io-utils
	  read-single-character read-word read-word-insist read-filename
	  menu-item menu-item-key menu-item-label menu-item-value
	  make-menu-item copy-menu-item menu-item-p
	  menu-selection-insist
	  ;; Various menus
	  *available-demos-menu* *available-sneps-demos-menu*
	  *available-snepslog-demos-menu*
	  ;; A new readfunction
	  pseudolisp-read
	  ))

(export '(;; utilities
	  chew-up-output redefine-function in.environment substitute-tree
	  ))

(export '(;; with-snepsul user functions
	  with-snepsul with-snepsul-direct
	  with-snepsul-silent with-snepsul-direct-silent
	  *with-snepsul-eval-function* with-snepsul-standard-eval
	  with-snepsul-trace-eval with-snepsul-toplevel-style-eval))

(import '(;; with-snepsul user functions
	  with-snepsul with-snepsul-direct
	  with-snepsul-silent with-snepsul-direct-silent
	  *with-snepsul-eval-function* with-snepsul-standard-eval
	  with-snepsul-trace-eval with-snepsul-toplevel-style-eval)
	(find-package 'snepsul))

(export '(;; node/lisp transformers:
	  node-to-lisp-object lisp-object-to-node
	  ns-to-lisp-list lisp-list-to-ns
	  apply-function-to-ns))

(export '(is-not.n negate.n))

(import '(;; node/lisp transformers:
	  node-to-lisp-object lisp-object-to-node
	  ns-to-lisp-list lisp-list-to-ns
	  apply-function-to-ns)
	(find-package 'snepsul))

(import '(defsnepscom undefsnepscoms) (find-package 'snepsul))

(shadowing-import '(^^ % ?
		    fwd-infer exit stack sneps
		    hyp der ext assertion restriction named n^ c^)
	(find-package 'snepsul))

(import '(converse compose kstar kplus relative-complement irreflexive-restrict
		   exception domain-restrict range-restrict)
	(find-package 'snepsul))

(import '(nodes assertions relations patterns varnodes variables defaultct
	  commands topcommands bnscommands fnscommands rscommands nscommands
	  default-defaultct all-hyps)
	(find-package 'snepsul))

(defvar *initial-relations*           ;; added WHEN, DO, IF for SNeRE: dk
    ;; added act, action, precondition, effect for SNeRE: scs
    ;; added whenever for SNeRE: hi 3/31/99
    '(attachedfunction &ant ant arg forall cq dcq default emax emin etot exists
      max min pevb thresh threshmax
      when whenever do if vars suchthat 
      condition then else
      act plan goal action precondition effect
      object1 object2))

(export '*initial-relations*)

(mapcar #'(lambda (ident)
	    (let ((ident&conv (list ident (intern (concatenate 'string (symbol-name ident) "-")))))
	      (export ident&conv)
	      (import ident&conv (find-package 'snepsul)) 
	      (import ident&conv (find-package 'snepslog)) 
	      (import ident&conv (find-package 'snip))))
	*initial-relations*)

;; added eval-when 9-02 FLJ
;;;(eval-when (eval compile load) 
  (in-package :snepsul)
;;;  ) 

(export '(r a c k u q s n d yes y ok sure no end))  

;; Needed for SNePSLOG:
(export '(|.| |,| ? ?? ~ |:| { } ! _ => &=> v=> <=>
	  thresh andor all exists nexists |0| |1|))

;; added eval-when 9-02 FLJ
;;;(eval-when (compile load) 
  (in-package :sneps)
;;;  ) 



(export 'build-namestring)    ;; for acl6 (FLJ)

(export 'contradictory-nodes.n)
    
    




