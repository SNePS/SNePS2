;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XGINSENG; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: rule-layout.lisp,v 

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
(create-instance 'SOCRATES node
  (:box '(918 1312 69 30))
  (:name "SOCRATES")
  (:sneps-node (sneps:node 'SNEPSUL::SOCRATES)))
(create-instance 'M2 node
  (:box '(1000 1168 33 30))
  (:name "M2!")
  (:sneps-node (sneps:node 'SNEPSUL::M2)))
(create-instance 'MAN node
  (:box '(1066 1311 36 30))
  (:name "MAN")
  (:sneps-node (sneps:node 'SNEPSUL::MAN)))
(create-instance 'MORTAL node
  (:box '(1476 1310 56 30))
  (:name "MORTAL")
  (:sneps-node (sneps:node 'SNEPSUL::MORTAL)))
(create-instance 'P2 node
  (:box '(1404 1188 30 30))
  (:name "P2")
  (:sneps-node (sneps:node 'ENGLEX:P2)))
(create-instance 'P1 node
  (:box '(1147 1188 30 30))
  (:name "P1")
  (:sneps-node (sneps:node 'ENGLEX:P1)))
(create-instance 'V1 node
  (:box '(1274 1265 30 30))
  (:name "V1")
  (:sneps-node (sneps:node 'SNEPSUL::V1)))
(create-instance 'M1 node
  (:box '(1273 1070 33 30))
  (:name "M1!")
  (:sneps-node (sneps:node 'SNEPSUL::M1)))
(dolist (node '(SOCRATES M2 MAN MORTAL P2 P1 V1 M1))
  (push (eval node) (g-value display-window :nodes-on-display))
  (opal:add-component display-aggregate (eval node)))
(opal:add-component display-aggregate (make-arc M2 SOCRATES "member") :back)
(opal:add-component display-aggregate (make-arc M2 MAN "class") :back)
(opal:add-component display-aggregate (make-arc P2 V1 "member") :back)
(opal:add-component display-aggregate (make-arc P2 MORTAL "class") :back)
(opal:add-component display-aggregate (make-arc P2 V1 "member") :back)
(opal:add-component display-aggregate (make-arc P2 MORTAL "class") :back)
(opal:add-component display-aggregate (make-arc P1 V1 "member") :back)
(opal:add-component display-aggregate (make-arc P1 MAN "class") :back)
(opal:add-component display-aggregate (make-arc P1 V1 "member") :back)
(opal:add-component display-aggregate (make-arc P1 MAN "class") :back)
(opal:add-component display-aggregate (make-arc M1 P2 "cq") :back)
(opal:add-component display-aggregate (make-arc M1 P1 "ant") :back)
(opal:add-component display-aggregate (make-arc M1 V1 "forall") :back)
(opal:add-component display-aggregate (make-arc M1 P2 "cq") :back)
(opal:add-component display-aggregate (make-arc M1 P1 "ant") :back)
(opal:add-component display-aggregate (make-arc M1 V1 "forall") :back)
(opal:add-component display-aggregate (make-arc M1 P2 "cq") :back)
(opal:add-component display-aggregate (make-arc M1 P1 "ant") :back)
(opal:add-component display-aggregate (make-arc M1 V1 "forall") :back)
