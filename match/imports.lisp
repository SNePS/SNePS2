;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MATCH; Base: 10 -*-

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



(in-package :match)


(import 'sneps:crntct)

(import '(sneps:down.fcs sneps:downfcs.pbi sneps:relationset.fcs sneps:do.fcs sneps:find-or-build
	  sneps:do.ns sneps:node-type sneps:value.sv sneps:insert.ns sneps:ismemb.ns
	  sneps:cardinality.ns sneps:freevars.n))

(import '(sneps:isbase.n sneps:isvar.n sneps:ismol.n sneps:ispat.n
   	  sneps:is-and-or.n sneps:is-&-ent.n sneps:is-v-ent.n sneps:is-thresh.n
	  sneps:is-num-ent.n sneps:is-non-deriv.n sneps:is-num-quant.n
	  sneps:node-to-number.n sneps:nodeset.n))

(import '(sneps:new.c sneps:new.cs sneps:insert.cs))

(import '(sneps:makeone.ns))

(import '(sneps:arg sneps:&ant sneps:ant sneps:cq sneps:dcq sneps:thresh sneps:threshmax
	  sneps:emin sneps:emax sneps:etot sneps:when sneps:do sneps:if))

(import 'sneps:build-namestring)  ;; added for acl6 compatibility (FLJ)



    
    




