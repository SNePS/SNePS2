;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: oinnet.lisp,v 1.3 1995/04/20 00:44:53 snwiz Exp

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


;; Backwards compatibility package to `innet' old-style network files with
;; a `SNEPS::|SNePS network|' ID at their beginning:
;;
;; Usage:
;; - Load this file into a Lisp image that has SNePS loaded.
;; - do `(oinnet "YOUROLDFILE")' at the SNePS prompt.
;; - save the network with `(outnet "YOURNEWFILE")' so you'll be able to
;;   `innet' it with the new version of `innet'.
;; - You're done.


;; Yes, this IS ugly!

; ==========================================================================
;
; oinnet
; ------
;
;       arguments     : fl - <filename> 
;
;       returns       : <nothing>
;
;       description   : "User function" to be called from SNePS environment.   
;                        It opens file fl and, if the content of file fl
;                        is a SNePS network, it inputs the network, 
;                        otherwise simply returns appropriate message.
;
;       side-effects  : It changes the entire <network> and prints a
;                       message. All previous existing contexts, nodes
;                       are erased before loadind the stored net.
;        
;
;        
;
;                                        written:  jgn 08/27/83
;                                        modified: ejm 06/01/84
;                                                  ssc 02/13/87
;                                                  njm 09/26/88
;                                                   hc 06/30/93
;                                                   hc 07/18/93
;
(defsnepscom oinnet ((fl) (top) t)
  (with-open-file (inunit (cl-user:sneps-translate fl) :direction :input)
    (cond ((eq (read inunit) '|SNePS network|)
	   ;;
	   ;; resets the existing net
	   ;;
	   (mapc #'(lambda (n)
		     (remprop (nodeaccess n) '=snode))
		 (value.sv 'nodes))
	   (set.sv 'nodes nil)
	   (set.sv 'varnodes nil)
	   (mapc #'(lambda (v)
		     (unless (eq v 'relations) (remprop v :val)))
		 (get 'variables :val))
	   (set.sv 'variables '(relations))
	   ;;
	   ;; loads the net stored in file "inunit"
	   ;;
	   (mapc #'(lambda (p)
		     (setf (get 'gennewnode p) (read inunit)))
		 '(b m v p tm tv tp))
	   (setf (get 'gennewcontext 'c) (read inunit))
	   (oinsysvars inunit)
	   (dolist (r (value.sv 'relations)) (new.r r))
	   (set.sv 'contexts (make-hash-table :test #'equal))
	   (dotimes (x (read inunit)) (oread1.ct inunit))
	   (dotimes (x (read inunit)) (oread2.ct inunit))
	   (dotimes (x (read inunit)) (oread.n inunit))
	   (format t "~%~%Old-style SNePS Network loaded from file: ~A~%" fl)
	   (values))
	  (t (sneps-error
	      (format nil "~A is not an old-style SNePS network file" fl)
	      'oinnet
	      'oinnet)))))

;
; ==========================================================================
;
; oinsysvars
; ---------
;
;       arguments     : inunit - <input port> 
;
;       returns       : ignored
;
;       nonlocal-vars : The system variables (side-effected by read.sv)
;
;       description   :  Calls read.sv to read S-expressions from the input port and 
;                       set the values of the system variables to
;                       these S-expressions.
;
;       side-effects  : read.sv sets the values of the system variables 
;
;                                          written : jgn 10/10/83
;                                          modified: ssc 02/25/87
;                                          modified: ssc 05/03/88
;
;
(defun oinsysvars (inunit)
  (dotimes (i 3)
    (oread.sv inunit)))

;; Need various old-style read functions (hc, 06/30/93):

(defun oread.sv (inunit)
  (let ((sv (read inunit)))
    (setf (symbol-plist sv) (read inunit))
    sv))

(defun oread1.ct (inunit)
  "It reads and reconstructs a <context> from `inunit', assuming that the 
   information was printed by print1.ct. The restriction slot will be read 
   and restored by read2.ct"
  (let* ((ca (read inunit)) ; context access is no longer a part of the context struct 
	 (order (read inunit))
	 (hyps (mapcar #'(lambda (na) (or (node na)
					  (newnode na (symbol-type.n na))))
		       (read inunit))) 
	 (c (newcontext order hyps)))
    (declare (ignore ca))
    (mapcar #'(lambda (v) (name.ct c v))
	    (read inunit))
    (setf (context-kinconsistent c) (read inunit))))

(defun oread2.ct (inunit)
  "It reads and reconstructs the restriction slot of a <context> from `inunit'
   assuming that the information was printed by print2.ct"
  ;; context accesses and context restrictions no longer exist.
  (read inunit)				; read and ignore the context access
  (read inunit)				; read and ignore the context restriction
  )

(defun oread.n (inunit)
  (let* ((na (read inunit))
	 (type (read inunit))
	 (n (or (node na) 
		(newnode na type))))
    (if (eq ':VAR (setf (node-type n) type)) ; in case the pre-existing node
				; had no type set.
	(set.sv 'varnodes (insert.ns n (value.sv 'varnodes))))
    (setf (node-perm n) (read inunit))
    (setf (node-freevars n) (mapcar #'(lambda (nv) (or (node nv)
						       (newnode nv ':var)))
				    (read inunit)))
    (setf (node-fcableset n) (oread.fcs inunit))
    (if (getcontextset.ctcs 'hyp
			    (setf (node-asupport n) (oread.ctcs inunit)))
	(set.sv 'assertions (insert.ns n (value.sv 'assertions))))))

(defun oread.fcs (inunit)
  (mapcar #'(lambda (u)
	      (cond ((is.r u)
		     u)
		    (t (mapcar #'(lambda (na)
				   (or (node na)
				       (newnode na (symbol-type.n na))))
			       u))))
	  (read inunit)))

(defun oread.ctcs (inunit)
  "reads a <context cable set> from `inunit'"
  (mapcar #'(lambda (u)
	      (cond ((is.ot u) u)
		    (t (mapcar #'context u))))
	  (read inunit)))




    
    




