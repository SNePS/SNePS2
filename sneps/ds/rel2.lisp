;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: rel2.lisp,v 1.2 2013/08/28 19:07:25 shapiro Exp $

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


;;; =============================================================================
;;; Data Type:  <relation>
;;; =============================================================================
;;;
;;; 
;;; =============================================================================
;;;
;;; new.r        
;;; -----
;;;
;;;       arguments     : id - <identifier>
;;;
;;;       returns       : <relation>
;;;
;;;       description   : creates a new <relation> having the name "id". In
;;;                       addition, it automatically creates the converse
;;;                       <relation> having the name "id-".
;;;                       It also adds the <relation> to the list of
;;;                       <relation>s "relations".
;;;
;;;       side-effects  : it side-effects the <atom>s "id", "id-", and
;;;                       "relations".
;;;
;;;                                        written:  mja 07/28/83
;;;                                        modified: ejm 02/11/84, 06/05/84
;;;                                        modified: ssc 11/04/87
;;;                                        modified: scs 06/23/89
;;;                                        modified: scs 11/02/06

(defun new.r (id &optional (pkg *package*)) 
  "Creates a new arc label."
  (let* ((cidname (concatenate 'string (symbol-name id) "-"))
	 (snepsulpkg (find-package :snepsul))
	 (snepslogpkg (find-package :snepslog))
	 (id- (or
	       ;; If id- is already in snepsul or snepslog pkg, use it
	       (find-symbol cidname snepsulpkg)
	       (find-symbol cidname snepslogpkg)
	       ;; Otherwise, create a new symbol
	       (intern cidname (choosePackageForConverse
				id snepsulpkg snepslogpkg pkg)))))
    ;; Import id- into the default pkg
    ;;(import id- pkg)
    ;; Import id- into the snepsulpkg and snepslogpkg
    (import id- snepsulpkg)
    (import id- snepslogpkg)
    (setf (get id '=conv) id-
	  (get id '=dnrel) t
	  (get id- '=conv) id
	  (get id- '=uprel) t)
    (set.sv 'relations
	    (insert.rs id (value.sv 'relations)))
    id))

(defun choosePackageForConverse (rel snepsulpkg snepslogpkg defaultpkg)
  "Returns the best package to use
        for the converse of the relation rel.
        snepsulpkg and snepslog pkg are those packages.
        defaultpkg is the default package to use if can't find a better one.
     It's already known that there's no symbol named crelname
        accessible in snepsulpkg or in snepslogpkg."
  (cond
   ;; If the home package of rel is snepsulpkg or snepslogpkg, use that
   ((eq (symbol-package rel) snepsulpkg) snepsulpkg)
   ((eq (symbol-package rel) snepslogpkg) snepslogpkg)
   ;; Else, if rel is accessible in snepsulpkg or snepslogpkg, use that
   ((find-symbol (symbol-name rel) snepsulpkg) snepsulpkg)
   ((find-symbol (symbol-name rel) snepslogpkg) snepslogpkg)
   ;; Else, use the defaultpkg
   (t defaultpkg)))
;
;
;
; =============================================================================
;
; converse.r
; ----------
;
;       arguments     : r - <relation>
;
;       returns       : <relation>
;
;       description   : returns the converse-relation to "r".
;
;                                        written:  mja 07/28/83
;                                        modified: ejm 06/05/84
;
;
(defun converse.r (r)
  (get r '=conv))
;
; =============================================================================
;
; undefine.r
; ----------
;
;       arguments     : r - <relation>
;
;       returns       : <nothing>
;
;       description   : it undefines the <relation> "r" and its converse
;                       <relation>
;
;       side-effects  : it side-effects the property list of the <atom>s 
;                       representing  the <relation> and its converse,
;                       and removes "r" from "relations".
;
;                                        written : ejm 06/05/84
;                                        modified: 
;
;
(defun undefine.r (r)
  (let ((conv (converse.r r)))
    (remprop conv '=conv)
    (remprop conv '=uprel))
  (remprop r '=conv)
  (remprop r '=dnrel)
  (set.sv 'relations 
	  (remove.rs r (value.sv 'relations))))
;
;
;
; =============================================================================
;
; isup.r       
; ------
;
;       arguments     : r - <relation>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "r" is an <=uprelation>, otherwise
;                       "false".
;
;                                        written:  mja 07/28/83
;                                        modified: ejm 06/05/84
;
;
(defun isup.r (r)
   (get r '=uprel))
;
;
; =============================================================================
;
; isdn.r       
; ------
;
;       arguments     : r - <relation>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "r" is a <=dnrelation>, otherwise
;                       "false".
;
;                                        written:  mja  07/28/83 
;                                        modified: ejm 06/05/84
;
;
(defun isdn.r (r)
  (get r '=dnrel))
;
;
; =============================================================================
;
; isquant.r
; ---------
;
;       arguments     : r - <relation>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "r" is a quantifier <relation>,
;                       otherwise "false".
;
;                                        written:  mja 08/08/83
;                                        modified: dk (added VARS)
;
;
(defun isquant.r (r)
  (member r '(forall exists pevb vars) :test #'eq))
;
;
; =============================================================================
;
; read.r
; ------
;
;       arguments     : inunit - <unit>
;
;       returns       : <relation>
;
;       description   : It reads a <relation> from "inunit" and defines it
;                       together with any associated path definition.
;
;       side-effects  : It sides effect the <plist> of the <relation>.
;
;                                        written :  ejm 10/04/83
;                                        modified:  hc  06/29/93
;

(defun read.r (inunit)
  (let ((r (read inunit)))
    (new.r r)))

(defun read-path.r (inunit)
  ;; Reads the path information of a relation
  (let* ((r (read inunit)))
    ;; Read/add path information if there was any:
    (read-plist r '(:pathdef :fwd-paths) inunit)
    (read-plist (converse.r r) '(:pathdef :fwd-paths) inunit)))

;
; =============================================================================
;
; Print.r
; -------
;
;       arguments     : r - <relation>
;                       outunit - <unit>
;
;       returns       : nil
;
;       description   : Prints the <relation> "r" to "outunit".
;
;       side-effects  : It prints the <relation>
;
;                                        written :  ejm 10/04/83
;                                        modified:  hc  06/29/93
;
;

(defun print.r (r outunit)
  (format outunit "~%~s" r))

(defun print-path.r (r outunit)
  ;; Prints the path definition associated with R.
  (format outunit "~%~s" r)
  (print-plist r '(:pathdef :fwd-paths) outunit)
  (print-plist (converse.r r) '(:pathdef :fwd-paths) outunit))

;
;
; =============================================================================

(defun isless.r (rel1 rel2)
  "defines an ordering among relations"
  (cond ((eq rel1 rel2) nil) ; a relation is not less than itself
	((and (isup.r rel1) (isup.r rel2)) ; up relations have the same ordering as down
	 (isless.r (converse.r rel1) (converse.r rel2)))
	((isup.r rel1) t) ; up relations sort before down relations
	((isup.r rel2) nil)
	;; both are down relations
	(t (let* ((priority-list '(FORALL EXISTS PEVB
				       MIN MAX THRESH THRESHMAX EMIN EMAX ETOT
				       ANT &ANT CQ DCQ ARG
				       FNAME DEFAULT)) ; an explicit ordered list
		  (less-list (member rel1 priority-list)))
	     (cond (less-list ; rel1 is a priority relation
		    (cond ((member rel2 less-list) t) ; rel2 is a lower priority relation
			  ((member rel2 priority-list) nil) ;rel2 is a higher priority relation
			  (t) ; rel2 is not a priority relation
			  ))
		   ((member rel2 priority-list) nil) ; rel2 is a priority relation
		   (t ; neither is a priority relation
		    (string< rel1 rel2)) ; so alphabetize
		   )))))



    
    




