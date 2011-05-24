;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: node0.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


; =============================================================================
; Data Type:  <node>
; =============================================================================
;

(defstruct (node
	     (:print-function node-printer)
	     )
  (na nil)	   ; Node Access, nil if this is a temporary node
  (order nil)      ; Node order. A positive integer corresponding to
                   ; the prder of creation. Added HI 3/20/99.
  (type ':unknown) ; Node type. Possibilities are :base :mol :var :pat :unknown
                   ; NOTE: type :unknown can also appear from read.fcs -- ssc 02/26/87
  (perm nil)	   ; Flag. T if node is permanent.
  (height 0)       ; Length of longest descending path
  (freevars nil)   ; The sneps variable nodes that are free in this node.
  (fcableset nil)  ; The node's flat-cable-set (a disembodied property list).
  (gi-node nil)	   ; The ginseng node for displaying this node graphically.
  (activation nil) ; the process associated with this node.
  (asupport nil)   ; The assumption-support.
  (jsupport nil)   ; The justification-support.
  (contexts nil)   ; The contexts where the node is present as an hyp.
  (snepslog nil))  ; the snepslog version of this node  pam 89/1/3


(defun node-printer (obj stream depth)
  (declare (ignore depth))
  ;;
  ;; Test whether CRNTCT is bound, because if it is not (due to some
  ;; sneps bug) debugging (inspecting, tracing) doesn't work because
  ;; these functions try to print nodes using this print function
  ;;
  ;; (buildcontext nil) = (buildcontext (new.ns))
  ;; it was used nil because this file has to be loaded before
  ;; macro new.ns.
  ;;
  (let ((crntct (if (boundp 'crntct) crntct (buildcontext nil))))
    (declare (special crntct))
    (write-string (symbol-name (node-na obj)) stream)
    (if (isassert.n obj) (write-string "!" stream))))
