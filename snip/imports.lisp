;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: imports.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(defvar *USER-PROCESS*				; used in user.lisp, deduce.lisp, message.lisp
						; and message.lisp
	)

(defvar ADDED-NODES				;
	)

(defvar *infertrace* nil)
(defvar DEDUCTION-RESULTS			;
	)
;;;
;;;
;;;       Following are the various multi-registers
;;;   (THEY ARE MADE GLOBAL VARIABLES OF THIS PACKAGE)
;;;

(defvar *TYPE*)
(defvar *NODE*) 
(defvar *KNOWN-INSTANCES*)
(defvar *REPORTS*)
(defvar *REQUESTS*)
(defvar *INCOMING-CHANNELS*)
(defvar *OUTGOING-CHANNELS*)
(defvar *RULE-USE-CHANNELS*)
(defvar *INTRODUCTION-CHANNELS*)
(defvar *PENDING-FORWARD-INFERENCES*)
(defvar *RULE-HANDLER*)
(defvar *USABILITY-TEST*)
(defvar *DEDUCED-NODES*)
(defvar *TOTAL-DESIRED*)
(defvar *POS-DESIRED*)
(defvar *NEG-DESIRED*)
(defvar *POS-FOUND*)
(defvar *NEG-FOUND*)
(defvar *NUM-QUANT-POS-INSTANCES*)
(defvar *NUM-QUANT-NEG-INSTANCES*)
(defvar *DEDUCTION-RESULTS*)

(import '(
;        match:*INITSUB*
;	match:*INITMNRS*
;       match:*ORIG-MNRS*
;	match:*RENAMESUB*
;	match:*MNRS*
;	match:*SOURCE-CONSTS*
;	match:*CHANGED*
;	match:*S-MCS*
;	match:*t-mcs*
;	match:*TARGET-CONSTS*
;	match:*SUB*
;	match:*UNI*
;	match:*UNODES*
;	match:*FINALMNRS*
;	match:*SOURCE-SUB*
;	match:*TARGET-SUB*
	match:restrict-binding-to-pat
;	match:match.mcs
;	match:allconsts.mcs
;	match:rename.mcs
;	match:isnew.mcs
;	match:choose.mcs
;	match:others.mcs
;	match:mapcar.mcs
;       match:make.uni
	match:match-in-context
	match:forward-match-in-context
	match:tnode.supmatching
	match:source-sub.supmatching
	match:target-sub.supmatching
	match:target-sup.supmatching
	match:others.supmatchingset
	match:choose.supmatchingset
	match:isnew.supmatchingset
        match:matchingset-to-supmatchingset
	match:do.supmatchingset
;      	match:relation.mc
;	match:mnodeset.mc
;;; Those items commented out conflict with the SNePS package
;;; (or are obsolete, HC Oct.11, 89).
;	match:new.mnrs
;	match:mcableset.mnrs
;;	match:is.mnrs
;	match:isbound.mnrs
;	match:choose.mns
;	match:isnew.mns
;	match:others.mns
;	match:mapcar.mns
	match:mvar.mb
;       match:new.mb
	match:mnode.mb
;	match:ismsym.mn
;	match:isnode.mn
;	match:equivalent.mn
	match:new.sbst
	match:is.sbst
	match:value.sbst
	match:putin.sbst
	match:iseq.sbst
	match:isnew.sbst
	match:cardinality.sbst
	match:term.sbst
	match:choose.sbst
	match:restrict.sbst
	match:isbound.sbst
	match:mnode.sbst
	match:others.sbst
	match:do.sbst
	match:union.sbst
	match:is-compatible.sbst
	))

(import '(multi:new multi:dp multi:regfetch multi:regstore
	  multi:initiate multi:multip multi:evnts multi:in-trace multi:ev-trace
	  multi:*QUEUES* multi:*NAME* multi:*PRIORITY*))

(import '(match:new.set match:is.set match:isnew.set match:insert.set match:make.set
	  match:makeone.set match:putin.set match:compl.set
	  match:union.set match:remove.set match:choose.set match:others.set
	  match:do.set match:intersect.set match:ismemb.set
	  match:issubset.set match:iseq.set match:makeset))

(import '(sneps:is.n sneps:isbase.n sneps:isvar.n sneps:ismol.n sneps:ispat.n sneps:describe.n
	  sneps:isnumber.n sneps:n-to-downcs sneps:pp-nodetree sneps:nodeset.n
	  sneps:iseq.n sneps:freevars.n sneps:assert.n sneps:node-to-number.n
	  sneps:is-nor.n sneps:is-and.n 
	  sneps:isassert.n sneps:new.ns sneps:isnew.ns sneps:ismemb.ns sneps:do.ns
	  sneps:choose.ns sneps:others.ns sneps:makeone.ns sneps:union.ns sneps:insert.ns
	  sneps:remove.ns sneps:cardinality.ns
	  sneps:apply-subst.cs sneps:clean-quantifiers.cs sneps:issubset.cs
;	  sneps:match.cs
	  sneps:crntct))

(import '(sneps:fullbuildcontext sneps:issubset.ct sneps:cts
	  sneps:isnew.cts sneps:new.cts sneps:makeone.cts sneps:choose.cts sneps:others.cts
	  sneps:ismemb.cts sneps:insert.cts sneps:make.cts))

(import '(sneps:activate.n sneps:quantified-vars.n sneps:activation.n sneps:dominates.n
	  sneps:activated.n sneps:is-v-ent.n sneps:is-&-ent.n sneps:is-thresh.n
	  sneps:is-and-or.n sneps:is-num-ent.n sneps:is-num-quant.n
	  sneps:is-do-if.n is-act.n sneps:is-when-do.n sneps:activate-act.n
	  sneps:is-non-deriv.n))

(import '(sneps:hyp sneps:der))

(import '(sneps:defsnepscom))

(import 'sneps:build-namestring)    ;; for acl6 (FLJ)

(import '(sneps:ctcs-to-cts sneps:value.sv))
    
(import '(snebr:*most-entrenched-props*))    




