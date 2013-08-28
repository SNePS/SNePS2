;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XGINSENG; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: result-layout.lisp,v 

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

(in-package :xginseng)
(create-instance 'M3 node
  (:box '(1227 1481 33 30))
  (:name "M3!")
  (:sneps-node (sneps:node 'SNEPSUL::M3)))
(dolist (node '(M3))
  (push (eval node) (g-value display-window :nodes-on-display))
  (opal:add-component display-aggregate (eval node)))
(opal:add-component display-aggregate (make-arc M3 SOCRATES "member") :back)
(opal:add-component display-aggregate (make-arc M3 MORTAL "class") :back)
