;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ENGLEX; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

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


(in-package :englex)


;;;  Export the following symbols in the ENGLEX package.

(export
 '(*lexicon* lexin lookup verbize verbize-y verbthread wordize getf
	flistify first-atom get-lexical-feature lookup-lexical-feature))

;; Ugly but necessary:
(import '(snepsul:n snepsul:q))


(export '(ctgy   adj n noun v verb multi-start
	  multi-rest
	  num    sing singular plur pl plural
	  ;; past
	  pastp
	  ;; plur
	  pprt
	  ;; pres
	  presp
	  presprt
	  tense  pres past p future futr ftr fut
	  root
	  stative
	  p1 person1 firstperson p2 person2 secondperson p3
	  decl
	  int interrogative interrog intrg ques question ynq q
	  imp imper imperative impr command request req
	  inf infinitive infin
	  gnd gerund ing grnd
	  non-prog
	  progr prgr prg progress progressive ;; prog (in LISP)
	  non-perf
	  perf pft prft prfct perfect perfective
	  active
	  pass passive
	  affirmative
	  neg nega negative negated ;; not (in LISP)
	  may must can shall will))

;;; Lusardi's new features and variables:
(export '(adv contr prep pron *Trace* *Count-New-Strings*))

;;;   Import these ENGLEX functions in the SNePSUL package.

(import '(#|lexin|# pres presf past pptf pastp
		wordize verbize verbize-y verbthread flistify )
	(find-package 'snepsul))

;;; 23 Feb 2011 ABCL-specific code to fix bug:
;;; When a SNePS command was later defined for lexin via
;;; defsnepscom, ABCL 0.24.0 was crashing when defsnepscom called                 
;;; shadowing-import. Importing it initially with shadowing-import
;;; solves the problem. - JPB
#+abcl (shadowing-import 'lexin (find-package 'snepsul))
#-abcl (import 'lexin (find-package 'snepsul))


;;;   Import this feature-mania into the SNePSUL package

(import '(ctgy   adj ;; n (already in SNePSUL)
	             noun v verb multi-start
	  multi-rest
	  num    sing singular plur pl plural
	  ;; past
	  ;; pastp (also a function)
	  ;; plur
	  pprt
	  ;; pres 
	  presp
	  presprt
	  tense  ;; pres past (also functions)
	         p future futr ftr fut
	  root
	  stative
	  p1 person1 firstperson p2 person2 secondperson p3
	  decl
	  int interrogative interrog intrg ques question ynq ;; q (in SNePSUL)
	  imp imper imperative impr command request req
	  inf infinitive infin
	  gnd gerund ing grnd
	  non-prog
	  progr prgr prg progress progressive ;; prog (in LISP)
	  non-perf
	  perf pft prft prfct perfect perfective
	  active
	  pass passive
	  affirmative
	  neg nega negative negated ;; not (in LISP)
	  may must can shall will)
	(find-package 'snepsul))

;;; Lusardi's new features:
(import '(adv contr prep pron)
	(find-package 'snepsul))



    
    




