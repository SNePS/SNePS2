;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013 Research Foundation of 
;;                          State University of New York

;; Version: $Id: svar.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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




(in-package :sneps)


; =============================================================================
; Data Type:  <svar> ::=
; =============================================================================
;
;
; =============================================================================
;
; is.sv
; -----
;
;       arguments     : sexpr - <any type>
;
;       returns       : <boolean>
;
;       description   : If sexpr is a <svar> the function returns t, otherwise
;                       it returns nil.
;
;       implementation: A (get sexpr ':val) is not correct because it does not
;                       dintinguish between the property with value "nil" and
;                       the absence of the property.
;
;                                        written:  ejm 7/28/83 
;                                        modified:
;
;
(defun is.sv (sexpr)
  (member ':val (symbol-plist sexpr) :test #'eq))
;
;
; =============================================================================
;
; new.sv
; ------
;
;       arguments     : ident - <identifier>
;
;       returns       : <svar> or nil
;
;       description   : It creates a new <svar> (sneps variable) with
;                       <identifier> ident if it does not exist yet.
;
;       side-effects  : It creates a new <atom> to represent <svar> and
;                       side effects its plist.
;
;                                        written:  ejm 7/28/83
;                                        modified:
;
;
(defmacro new.sv (ident)
   `(cond ((is.sv ,ident) nil)
	  (t (setf (get ,ident ':val) nil)
	     (pushnew ,ident (get 'variables ':val))
             ,ident)))
;
; =============================================================================
;
; newsys.sv
; ---------
;
;       arguments     : ident - <identifier>
;
;       returns       : <svar> 
;
;       description   : It creates a new <system svar> (pre-defined svar) with
;                       <identifier> ident if it does not exist yet.
;
;       side-effects  : It creates a new <atom> to represent <svar> and
;                       side effects its plist.
;
;                                        written:  ejm 6/12/84
;                                        modified: scs 5/4/88
;
;
(defun newsys.sv (ident)
  (cond ((is.sv ident) nil)
	(t (setf (get ident '=sysvar) t
		 (get ident ':val) nil)
	   (if (not (eq ident 'variables))
	       (pushnew ident (get 'variables ':val)))
	   ident)))
;
;
; =============================================================================
;
; issys.sv
; --------
;
;       arguments     : sexpr - <any type>
;
;       returns       : <boolean>
;
;       description   : If sexpr is a <system svar>, i.e., a pre-defined <svar>,
;                       the function returns t, otherwise it returns nil.
;
;                                        written:  ejm 6/12/84 
;                                        modified:
;
;
(defun issys.sv (sexpr)
  (get sexpr '=sysvar))
;
;
; =============================================================================
;
; isnew.sv
; --------
;
;       arguments     : svar - <svar>
;
;       returns       : <boolean>
;
;       description   : If svar is a <newsvar> the function returns t, otherwise
;                       it returns nil.
;
;                                        written:  ejm 7/28/83 
;                                        modified:
;
;
(defun isnew.sv (svar)
   (cond ((is.sv svar) (not (get svar ':val)))
          (t
	   (error "function isnew.sv -- argument ~S is not a variable"
		  svar))))
;
;
; =============================================================================
;
; value.sv
; --------
;
;       arguments     : svar - <svar>
;
;       returns       : <value>
;
;       description   : It returns the <value> of svar.
;
;                                        written:  ejm 7/28/83
;                                        modified:
;
;
(defun value.sv (svar)
  (get svar ':val))
;
;
; =============================================================================
;
; set.sv
; ------
;
;       arguments     : svar - <svar>
;                       seqval  - <value>
;
;       returns       : <value>
;
;       description   : Given the <svar> "svar" and the sequence <value> 
;                       "seqval", it sets the <value> of svar to seqval.
;                       If the old value of "svar" was a context then the
;                       names' slot of that context is updated. 
;
;       side-effects  : It side effects the plist of svar.
;                       The name's slot of a context can be side-effected.
;
;                                        written:  ejm  7/28/83
;                                        modified: njm 10/03/88
;
;
(defun set.sv (svar seqval)
  (cond ((is.sv svar)
	 (when (is.ct (value.sv svar))
	   (removename.ct (value.sv svar) svar))
	 (setf (get svar ':val) seqval))
	(t (new.sv svar) (setf (get svar ':val) seqval))))
;
;
; =============================================================================
;
; update.sv
; ---------
;
;       arguments     : svar - <svar>
;                       val  - <value>
;
;       returns       : <value>
;
;       description   : Given the <svar> svar and the individual <value> val
;                       it adds val to the <value> of svar, if it is not
;                       there yet.
;
;       side-effects  : It side effects the plist of svar.
;
;                                        written:  ejm 7/28/83
;                                        modified:
;
;
;(defmacro update.sv (svar indval)
;   `(cond ((and (is.n ,indval) (is.ns (value.sv ,svar)))
;           (set.sv ,svar (insert.ns ,indval (value.sv ,svar))))
;          ((and (is.r ,indval) (is.rs (value.sv ,svar)))
;           (set.sv ,svar (insert.rs ,indval (value.sv ,svar))))
;          (t (error "function update.sv -- illegal type"))))
;
;
; =============================================================================
;
; iseq.sv
; -------
;
;       arguments     : svar1 - <svar>
;                       svar2 - <svar>
;
;       returns       : <boolean>
;
;       description   : It compares two <svar>s and returns "true" if they are 
;                       the same variable, "false" otherwise.
;
;                                        written:  ejm 07/28/83
;                                        modified: ejm 09/06/83
;
;
(defun iseq.sv (svar1 svar2)
  (eq svar1 svar2))
;
;
; =============================================================================
;
; read.sv
; -------
;
;       arguments     : inunit - <unit>
;
;       returns       : <svar>
;
;       description   : It reads a <svar> from "inunit".
;                       It converts the sets of <node access> to <nodesets>.
;
;
;                                        written :  ejm 10/04/83
;                                        modified:  scs 05/03/88
;                                                   hc  06/29/93
;
(defun read.sv (inunit)
  (read-plist (read inunit) '(:val =sysvar) inunit))

(defun read-plist (symbol properties inunit)
  ;; First removes all PROPERTIES of SYMBOL, and then defines all
  ;; properties specified in the disembodied plist on INUNIT.
  (let ((plist (read inunit)))
    ;; Clear old properties:
    (dolist (property properties)
      (remprop symbol properties))
    ;; Set new ones that are defined:
    (do ((plist plist (cddr plist)))
	((null plist) symbol)
      (setf (get symbol (car plist)) (cadr plist)))))
;
;
; =============================================================================
;
; Print.sv
; --------
;
;       arguments     : sv - <svar>
;                       outunit - <unit>
;
;       returns       : nil
;
;       description   : It Prints a <svar> to "outunit".
;
;       side-effects  : It prints the <svar>
;
;                                        written :  ejm 10/04/83
;                                        modified:  hc  06/29/93
;
(defun print.sv (svar outunit)
  (format outunit "~%~s" svar)
  (print-plist svar '(:val =sysvar) outunit))

(defun print-plist (symbol properties outunit)
  ;; Prints a disembodied plist of all non-NIL-valued
  ;; PROPERTIES of SYMBOL to OUTUNIT.
  (with-readable-nodes-and-contexts
      (let ((first t))
	(format outunit "~%(")
	(dolist (property properties)
	  (when (get symbol property)
	    (if first
		(setq first nil)
	      (terpri outunit))
	    (format outunit "~s ~s" property (get symbol property))))
	(format outunit ")"))))
