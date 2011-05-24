;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: intext.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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
;;; These shadowing, exporting, and importing operations should go
;;;    into the appropriate exports and imports files
;;; But they can't because that would mess up the loading of the SNePS system

(export 'load)
(shadowing-import 'load :snepslog)


;; intext
;; ------
;;
;;       arguments     : file - <filename> 
;;
;;       returns       : <nothing>
;;
;;       description   : User function to be called from the Sneps 
;;                       environment. It opens FILE and uses it as the
;;                       source of the input stream for the read-eval-print
;;                       loop of Sneps until encountering eof on FILE.
;;                       FILE is then closed and the source of input 
;;                       used prior to opening FILE is reinstated as 
;;                       the source of input stream (just like demo
;;                       but no input echoing).
;;
;;       side-effects  : It changes the source of input and it prints
;;                       a message at the beginning and the end.
;;
;;                                          written : jgn 09/01/83
;;                                          modified: ejm 02/28/84, 06/01/84
;;                                                    hc  11/22/91
;;                                                    hc  07/18/93
;;
(defsnepscom intext ((file))
  (declare (special outunit))
  (format outunit "~&Loading file ~A.~%" file)
  (load file :format :snepsul)
  (values))
