;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: make.lisp,v 1.1 2011/05/24 17:59:38 mwk3 Exp $

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


(defun make-v-ent (n)
   (multi:new 'rule              ; NAME:
        'or-entailment           ; TYPE:
        n                        ; NODE:
        (new.iset)               ; KNOWN-INSTANCES:
        (new.repset)             ; REPORTS:
        (new.chset)              ; REQUESTS:
        (new.feedset)            ; INCOMING-CHANNELS:              
        (new.chset)              ; OUTGOING-CHANNELS:
        (new.cqchset)            ; RULE-USE-CHANNELS:
	(new.ichset)             ; INTRODUCTION-CHANNELS:
        (new.repset)             ; PENDING-FORWARD-INFERENCES:
        nil                      ; PRIORITY:
        'rule-handler.v-ent      ; RULE-HANDLER:
        'usability-test.v-ent))  ; USABILITY-TEST:

(defun make-&-ent (n)
   (multi:new 'rule              ; NAME:
        'and-entailment          ; TYPE:
        n                        ; NODE:
        (new.iset)               ; KNOWN-INSTANCES:
        (new.repset)             ; REPORTS:
        (new.chset)              ; REQUESTS:
        (new.feedset)            ; INCOMING-CHANNELS:
        (new.chset)              ; OUTGOING-CHANNELS:
        (new.cqchset)            ; RULE-USE-CHANNELS:
	(new.ichset)             ; INTRODUCTION-CHANNELS:
        (new.repset)             ; PENDING-FORWARD-INFERENCES:
        nil                      ; PRIORITY:
        'rule-handler.&-ent      ; RULE-HANDLER:
        'usability-test.&-ent))  ; USABILITY-TEST:

(defun make-thresh (n)
   (multi:new 'rule              ; NAME:
        'thresh                  ; TYPE:
        n                        ; NODE:
        (new.iset)               ; KNOWN-INSTANCES:
        (new.repset)             ; REPORTS:
        (new.chset)              ; REQUESTS:
        (new.feedset)            ; INCOMING-CHANNELS:
        (new.chset)              ; OUTGOING-CHANNELS:
        (new.cqchset)            ; RULE-USE-CHANNELS:
	(new.ichset)             ; INTRODUCTION-CHANNELS:
        (new.repset)             ; PENDING-FORWARD-INFERENCES:
        nil                      ; PRIORITY:
        'rule-handler.thresh     ; RULE-HANDLER:
        'usability-test.thresh)) ; USABILITY-TEST:

(defun make-nor (n)
   (multi:new 'rule              ; NAME:
	'nor                     ; TYPE:
        n                        ; NODE:
        (new.iset)               ; KNOWN-INSTANCES:
        (new.repset)             ; REPORTS:
        (new.chset)              ; REQUESTS:
        (new.feedset)            ; INCOMING-CHANNELS:
        (new.chset)              ; OUTGOING-CHANNELS:
        (new.cqchset)            ; RULE-USE-CHANNELS:
	(new.ichset)             ; INTRODUCTION-CHANNELS:
        (new.repset)             ; PENDING-FORWARD-INFERENCES:
        nil                      ; PRIORITY:
        'rule-handler.nor        ; RULE-HANDLER:
        'usability-test.nor))    ; USABILITY-TEST:

(defun make-and (n)
   (multi:new 'rule              ; NAME:
        'and                     ; TYPE:
        n                        ; NODE:
        (new.iset)               ; KNOWN-INSTANCES:
        (new.repset)             ; REPORTS:
        (new.chset)              ; REQUESTS:
        (new.feedset)            ; INCOMING-CHANNELS:
        (new.chset)              ; OUTGOING-CHANNELS:
        (new.cqchset)            ; RULE-USE-CHANNELS:
	(new.ichset)             ; INTRODUCTION-CHANNELS:
        (new.repset)             ; PENDING-FORWARD-INFERENCES:
        nil                      ; PRIORITY:
        'rule-handler.and        ; RULE-HANDLER:
        'usability-test.and))    ; USABILITY-TEST:

(defun make-and-or (n)
   (multi:new 'rule              ; NAME:
        'and-or                  ; TYPE:
        n                        ; NODE:
        (new.iset)               ; KNOWN-INSTANCES:
        (new.repset)             ; REPORTS:
        (new.chset)              ; REQUESTS:
        (new.feedset)            ; INCOMING-CHANNELS:
        (new.chset)              ; OUTGOING-CHANNELS:
        (new.cqchset)            ; RULE-USE-CHANNELS:
	(new.ichset)             ; INTRODUCTION-CHANNELS:
        (new.repset)             ; PENDING-FORWARD-INFERENCES:
        nil                      ; PRIORITY:
        'rule-handler.and-or     ; RULE-HANDLER:
        'usability-test.and-or)) ; USABILITY-TEST:

(defun make-num-ent (n)
   (multi:new 'rule                ; NAME:
        'num-entailment            ; TYPE:
        n                          ; NODE:
        (new.iset)                 ; KNOWN-INSTANCES:
        (new.repset)               ; REPORTS:
        (new.chset)                ; REQUESTS:
        (new.feedset)              ; INCOMING-CHANNELS:
        (new.chset)                ; OUTGOING-CHANNELS:
        (new.cqchset)              ; RULE-USE-CHANNELS:
	(new.ichset)               ; INTRODUCTION-CHANNELS:
        (new.repset)               ; PENDING-FORWARD-INFERENCES:
        nil                        ; PRIORITY:
        'rule-handler.num-ent      ; RULE-HANDLER:
        'usability-test.num-ent))  ; USABILITY-TEST:

(defun make-num-quant (n)
  (multi:new 'num-quant.rule
        'num-quantifier              ; TYPE:
        n                            ; NODE:
        (new.iset)                   ; KNOWN-INSTANCES:
	nil                          ; NUM-QUANT-POS-INSTANCES:
	nil                          ; NUM-QUANT-NEG-INSTANCES:
        (new.repset)                 ; REPORTS:
        (new.chset)                  ; REQUESTS:
        (new.feedset)                ; INCOMING-CHANNELS:
        (new.chset)                  ; OUTGOING-CHANNELS:
        (new.cqchset)                ; RULE-USE-CHANNELS:
	(new.ichset)                 ; INTRODUCTION-CHANNELS:
        (new.repset)                 ; PENDING-FORWARD-INFERENCES:
        nil                          ; PRIORITY:
        'rule-handler.num-quant      ; RULE-HANDLER:
        'usability-test.num-quant))  ; USABILITY-TEST:

(defun make-non-deriv (n)
   (multi:new 'rule                  ; NAME:
        'non-derivable               ; TYPE:
        n                            ; NODE:
        (new.iset)                   ; KNOWN-INSTANCES:
        (new.repset)                 ; REPORTS:
        (new.chset)                  ; REQUESTS:
        (new.feedset)                ; INCOMING-CHANNELS:
        (new.chset)                  ; OUTGOING-CHANNELS:
        (new.cqchset)                ; RULE-USE-CHANNELS:
	(new.ichset)                 ; INTRODUCTION-CHANNELS:
        (new.repset)                 ; PENDING-FORWARD-INFERENCES:
        nil                          ; PRIORITY:
        'rule-handler.non-deriv      ; RULE-HANDLER:
        'usability-test.non-deriv))  ; USABILITY-TEST:

(defun make-do-if (n)
   (multi:new 'rule              ; NAME:
	'do-if                   ; TYPE:
        n                        ; NODE:
        (new.iset)               ; KNOWN-INSTANCES:
        (new.repset)             ; REPORTS:
        (new.chset)              ; REQUESTS:
        (new.feedset)            ; INCOMING-CHANNELS:              
        (new.chset)              ; OUTGOING-CHANNELS:
        (new.cqchset)            ; RULE-USE-CHANNELS:
	(new.ichset)             ; INTRODUCTION-CHANNELS:
        (new.repset)             ; PENDING-FORWARD-INFERENCES:
        nil                      ; PRIORITY:
        'rule-handler.do-if      ; RULE-HANDLER:
        'usability-test.do-if))  ; USABILITY-TEST:

(defun make-when-do (n)
   (multi:new 'rule              ; NAME:
	'when-do                 ; TYPE:
        n                        ; NODE:
        (new.iset)               ; KNOWN-INSTANCES:
        (new.repset)             ; REPORTS:
        (new.chset)              ; REQUESTS:
        (new.feedset)            ; INCOMING-CHANNELS:              
        (new.chset)              ; OUTGOING-CHANNELS:
        (new.cqchset)            ; RULE-USE-CHANNELS:
	(new.ichset)             ; INTRODUCTION-CHANNELS:
        (new.repset)             ; PENDING-FORWARD-INFERENCES:
        nil                      ; PRIORITY:
        'rule-handler.when-do    ; RULE-HANDLER:
        'usability-test.when-do)); USABILITY-TEST:

;Added hi 3/31/99
(defun make-whenever-do (n)
   (multi:new 'rule                    ; NAME:
	'whenever-do                   ; TYPE:
        n                              ; NODE:
        (new.iset)                     ; KNOWN-INSTANCES:
        (new.repset)                   ; REPORTS:
        (new.chset)                    ; REQUESTS:
        (new.feedset)                  ; INCOMING-CHANNELS:              
        (new.chset)                    ; OUTGOING-CHANNELS:
        (new.cqchset)                  ; RULE-USE-CHANNELS:
	(new.ichset)                   ; INTRODUCTION-CHANNELS:
        (new.repset)                   ; PENDING-FORWARD-INFERENCES:
        nil                            ; PRIORITY:
        'rule-handler.whenever-do      ; RULE-HANDLER:
        'usability-test.whenever-do))  ; USABILITY-TEST:


(defun make-non-rule (n)
   (multi:new 'non-rule   ; NAME:
        n                 ; NODE:
        (new.iset)        ; KNOWN-INSTANCES:
        (new.repset)      ; REPORTS:
        (new.chset)       ; REQUESTS:
        (new.feedset)     ; INCOMING-CHANNELS:
        (new.chset)       ; OUTGOING-CHANNELS:
        (new.repset)      ; PENDING-FORWARD-INFERENCES:
        nil))             ; PRIORITY:




;;; This version of  make-act issues a warning
;;;    if the node it's called on does not look like an act to sneps:is-act.n. 

(defun make-act (n)
  (unless (sneps:is-act.n n)
    (warn
     "*** The node ~S is being activated as an act, ~
             but it doesn't look like an act to sneps:is-act.n. ***"
	  n))
   (multi:new 'act                 ; NAME:
        n                          ; NODE:
	nil			   ; CONTEXT-NAME:
	(new.repset)		   ; REPORTS:
	(new.ns)		   ; PRECONDITIONS:
	(new.ns)		   ; EFFECTS:
	(new.ns)                   ; PLANS
	nil		 	   ; AGENDA:
        nil))			   ; PRIORITY:





    
    




