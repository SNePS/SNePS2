;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 2006--2011
;; Research Foundation of State University of New York

;; Version: $Id: load.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(defun load (file &key (format :snepslog))
  "Executes the contents of the file as a series of
        either SNePSLOG commands or SNePSUL commands,
        depending on the value of format,
        which must be either :snepslog, :sneps, or :snepsul.
     Any asserts called by the file are done batched at the end.
     And they are all asserted into the current context."
  (declare (special *assertLater* *newPropositions*))
  (case format
    (:snepslog (snepslog-load file))
    ((:snepsul :sneps) (sneps-load file))
    (t (error "Unrecognized format: ~S" format))))

(defvar *null-stream-output*
    #+allegro
  (make-instance
      'excl:null-simple-stream
    :external-format
    (excl:find-external-format :default))
  #-allegro
  (defvar *null-stream-output* (make-broadcast-stream))
  "The null stream")

(export '*null-stream-output*)

(defun snepslog-load (file)
  "Executes the contents of the file as a series of SNePSLOG commands."
  ;; Can issue individual SNePSLOG commands with (tell "<command>")
  (if snepslog::*SNePSLOGRunning*
      (demo-internal file 
		     :output-stream 
		     *null-stream-output*
		     :use-batch-assert t)
    (progn 
       (setf *assertLater* 1)
       (unwind-protect
	   (with-open-file (strm file :direction :input)
	     (snepslog
	      :inunit 
	      (make-concatenated-stream
	       strm (make-string-input-stream (format nil "~%lisp")))
	      :outunit *null-stream-output*))
	 (batchAssert *newPropositions*))
       (setf *newPropositions* nil
	     *assertLater* 0)))
       t)

(defun sneps-load (file)
  "Executes the contents of the file as a series of SNePSUL commands."
  (setf *assertLater* 1)
  (unwind-protect
      (let ((*package* (find-package 'snepsul))
	    (outunit
	     *null-stream-output*)
	    inunit
	    )
	(declare (special inunit))
	(sneps-init)
	(with-open-file (strm file :direction :input)
	  (setf inunit
	    (make-concatenated-stream
	     (make-string-input-stream
	      (format nil
		      "~%^(setf sneps:outunit sneps:*null-stream-output*)~%"))
	     strm
	     (make-string-input-stream (format nil "~%(lisp)"))))
	  (sneps-loop)
	  t))  
    (batchAssert *newPropositions*))
  (setf *newPropositions* nil
	*assertLater* 0))
  
(defun batchAssert (ns)
  "Asserts each node in the list ns.
  Creates at most (+ (length ns) 2) new contexts:
     1 empty context;
     1 context for each node in ns;
     1 context containing all nodes in ns.
  Finally, checks the nodes in ns, in order, for being contradictory." 
  ;;(declare (special inunit outunit crntct))
  (declare (special crntct))
  ;; Assert each node in ns
  (loop for n in ns
      for nct = (buildcontext (makeone.ns n))
      do (setf (node-asupport n) 
	   (insert.ctcs 'hyp nct (node-asupport n)))
	 (set.sv 'assertions (insert.ns n (value.sv 'assertions))))
  ;; Put the nodes into the current context
  (name.ct (fullbuildcontext ns
			     (makeone.cts
			      (if (is.ct crntct) crntct (value.sv crntct))))
	   (value.sv 'defaultct))
  ;; Now check for any contradictions
  (loop with cntxt = (value.sv 'defaultct)
      for n in ns
      do (snebr:ck-contradiction n cntxt 'assertion)))
