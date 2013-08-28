;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEBR; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: imports.lisp,v 1.2 2013/08/28 19:07:24 shapiro Exp $

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




(in-package :snebr)


(import '(sneps:is.n sneps:describe.n sneps:nodeset.n sneps:node-to-number.n
	  sneps:new.ns sneps:isnew.ns sneps:ismemb.ns sneps:nodeaccess sneps:node sneps:node-asupport
	  sneps:choose.ns sneps:others.ns sneps:makeone.ns sneps:union.ns
	  sneps:remove.ns sneps:cardinality.ns sneps:insert.ns sneps:compl.ns))

(import '(sneps:is.ct sneps:new.cts sneps:isnew.cts sneps:ismemb.cts
	  sneps:choose.cts sneps:others.cts sneps:makeone.cts sneps:union.cts 
	  sneps:insert.cts sneps:remove.cts sneps:cardinality.cts sneps:issubset.cts
	  sneps:ctcs-to-cts sneps:update-contexts sneps:repeat sneps:buildcontext
    sneps:ok-update-contexts sneps:fullbuildcontext sneps:clear-infer
	  sneps:value.sv sneps:name.ct sneps:isassert.n sneps:context-hyps
    sneps:defsnepscom))

(import '(sneps:arg sneps:arg-))

(import 'sneps:build-namestring)  ;; for acl6 (FLJ)




    
    




