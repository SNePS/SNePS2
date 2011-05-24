;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: exports.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :match)


(export '(match-in-context forward-match-in-context
			   restrict-binding-to-pat bindingof))

(export '(tnode.supmatching source-sub.supmatching target-sup.supmatching 
	  target-sub.supmatching others.supmatchingset
	  choose.supmatchingset do.supmatchingset isnew.supmatchingset
	  matchingset-to-supmatchingset))

(export '(mvar.mb mnode.mb))
	  
(export '(new.sbst value.sbst
	  is.sbst putin.sbst iseq.sbst isnew.sbst cardinality.sbst term.sbst
	  choose.sbst restrict.sbst isbound.sbst mnode.sbst others.sbst
	  do.sbst union.sbst putin.sbst union.sbst restrict.sbst term.sbst
	  is-compatible.sbst cardinality.sbst issubset.sbst))

(export '(new.set is.set isnew.set insert.set make.set makeone.set putin.set compl.set
	  union.set remove.set choose.set others.set do.set intersect.set ismemb.set
	  issubset.set iseq.set makeset))

(export '(supporting-nodes top-node))



    
    




