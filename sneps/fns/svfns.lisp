;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: svfns.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


; ==========================================================================
;
; * 
; -
;
;      arguments     : sv - <svar>
;
;      returns       : <snepsset>
;
;      description   : Gets the value of the sneps variable "sv".
;                      This value is a <snepsset>, and is one of the 
;                      following: <node set>, <relation set>, <svar set>, 
;                      or <command set>.
;
;                                         written:  CCC 08/02/83
;                                         modified: ejm 10/23/83
;                                                   njm 04/28/89
;                                                   hc  07/18/93
;
(defsnepscom * ((id) (top ns bns tbns fns rs) t)
  (values (value.sv id) (value.sv 'defaultct)))


; ==========================================================================
;
; $ 
; -
;
;      arguments     : sv - <svar>
;
;      returns       : <node set> 
;
;      description   : Creates a new <permanent variable node>, which 
;                      becomes the value of the <svar> "sv".
;
;      side-effects  : Adds the new variable node to the value of 
;                      "varnodes".
;
;                                         written : CCC 08/02/83
;                                         modified: ejm 10/11/83
;                                                   hc  07/18/93
;
(defsnepscom $ ((id) (bns) t)
  (let ((svar (genpvar.n)))
    (setf (node-snepslog svar) id)
    (set.sv id (makeone.ns svar))))

; ==========================================================================
;


(defsnepscom |#| ((id) $ t)
  (set.sv id (makeone.ns (genpbase.n))))



    
    




