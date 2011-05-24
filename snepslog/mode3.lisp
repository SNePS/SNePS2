;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: mode3.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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



(in-package :snepslog)  ;;; added ":"  FLJ 9/2/02

(defconstant *PROPARGS* (make-hash-table :test #'eql)
  "Key is pred; value is (list-of-arcs)")

(defconstant *ARGSPROP* (make-hash-table :test #'equal)
  "Key is (ordered-list-of-arcs); value is arc to pred or (pred)")

(defconstant *argsdesc* 
    (make-hash-table :test #'equal)
  "Keys is (relation-name alpha-ordered-list-of-arcs); values is a compiled description
   generating function.")

(defconstant *snip-arc-desc-pairs*
   
  '((forall ant cq) "for all variables ([forall]), if one or more of {[ant]}, then [cq]"
    (forall &ant cq) "for all variables ([forall]), if [&ant], then [cq]"
    (ant cq) "if one or more of [ant], then [cq]"
    (&ant cq) "if [&ant], then [cq]"
    (thresh &ant cq) "if at least [thresh] of [&ant], then [cq]"
    (min max arg) "at least of [min] and at most of [max] of [arg]"
    (thresh threshmax arg) "either fewer than [thresh] or more than [threshmax] of [arg]"
    (emin emax etot pevb &ant cq) "of the [etot] [pevb] that satisfy [&ant], at least [emin] and at most [emax] also satisfy [cq]")
   "Listing of the snif arc structures and descriptions for them.")

(defconstant *mode3-pred-arc-pairs*
    '(achieve (action object1) "achieve [object1]" 
      \Act\Plan (nil act sneps:plan) "to perform [act] do [plan]"
      believe (action object1) "believe [object1]"
      disbelieve (action object1) "disbelieve [object1]"
      adopt (action object1) "adopt policy [object1]"
      unadopt (action object1) "unadopt policy [object1]"
      do-all (action object1) "perform [object1]"
      do-one (action object1) "perform one of [object1]"
      \Effect (nil act effect) "the effect of [act] is [effect]"
      else (nil else) "otherwise perform [else]"
      \Goal\Plan (nil goal plan) "in order to acheive [goal] do [plan]"
      if (nil condition then) "if [condition], then perform [then]"
      ifdo (nil if do) "if [if] do [do]"
   \Precondition (nil act precondition) "[precondition] is required in order to perform [act]"
      snif (action object1) "snif: [object1]"
      sniterate (action object1) "sniterate: [object1]"
      snsequence (action object1 object2) "perform [object1] then [object2]"
      whendo (nil when do) "when [when] do [do]"
      wheneverdo (nil whenever do) "whenever [whenever] do [do]"
   withall (action vars suchthat do else) "withall bindings of [vars] such that [suchthat] do [do] else [else]"
   withsome (action vars suchthat do else) "with one binding of [vars] such that [suchthat] do [do] else [else]")
  "Listing of the mode3 predicates and their corresponding arcs. This
   is a p-list. Added 9/19/06 - mwk3")
    
(defvar *SILENT-MODE-3* nil
  "If T, mode-3 feedback isn't given.")

(defun set-mode-3 (&optional (silent nil))
  (declare (special *silent-mode-3* *br-tie-mode* *br-auto-mode*))
  (resetnet t)
  (setf *silent-mode-3* silent)
  (make.snepslog3)
  (unless *silent-mode-3*
    (format outunit "~%In SNePSLOG Mode 3.~%Use define-frame <pred> <list-of-arc-labels>.~%~%"))
  (loop
      for (pred arcs string) on *mode3-pred-arc-pairs* by #'cdddr
      do (define-frame pred arcs string))
  (loop for (arcs string) on *snip-arc-desc-pairs* by #'cddr
      do (make-description-function (cons nil arcs) string))
  (setf *br-tie-mode* t)
  (setf *br-auto-mode* t)
  (setf (symbol-function 'snebr::br-totalorder)
    (symbol-function 'fluent)))

(defun inMode3p ()
  "Returns t if SNePSLOG is currently in mode 3;
       nil otherwise."
  (eq (get 'mode 'make-relation) #'make-relation.3))


(defun describe-terms (node-list) 
  "Takes a node-list and prints to the snepslog prompt the
   descriptions for those nodes, if available. Added 9/13/06 - mwk3"
  (loop 
      for n in node-list
      do (format sneps:outunit "~A.~%"
		 (string-upcase (get-node-description-string n) 
				:start 0 :end 1))))

(defun get-node-description-string (n)
  "Returns a string describing a node, if available. Added 9/13/06 - mwk3"
   (if (sneps:is-not.n n)
      (concatenate 'string 
	"it is not the case that "
	(format nil "~{~A~#[~; and ~:;, ~]~}"
		(mapcar
		 #'get-node-description-string 
		 (sneps:nodeset.n n 'sneps::arg))))
    (cond 
     ((sneps:isbase.n n)
      (string (sneps:node-na n)))
     
     ((sneps:isvar.n n)
      (string (sneps:node-snepslog n)))
     ((or (sneps:ismol.n n) (sneps:ispat.n n))
      (let ((func (get-description-function 
		   n
		   (remove #\-  
			   (loop 
			       for elm in (sneps:node-fcableset n) by #'cddr
			       collect elm)
			   :key (lambda (s) 
				  (char (symbol-name s) 
					(1- (length (symbol-name s)))))))))
	(if func
	    (funcall func n)
	  "<not-yet-specified-by-user>"))))))


(defun make-node-set-description-string (ns)
  "Returns a string representing a set of nodes, this replaces the
   nicer lisp format statement, but makes it so changes to nodeset
   representations in the future will not require this function and
   those that rely on it to be rewritten. Added 9/15/06 - mwk3"
  (let ((ns-cpy ns))
    (cond 
     ((cl:= (sneps:cardinality.ns ns) 1) 
      (get-node-description-string (sneps:choose.ns ns)))
     ((cl:= (sneps:cardinality.ns ns) 2)
      (concatenate 'string 
	(get-node-description-string (sneps:choose.ns ns))
	" and "
	(get-node-description-string 
	 (sneps:choose.ns (sneps:others.ns ns)))))
     (t (loop 
	    until (cl:<= (sneps:cardinality.ns ns-cpy) 2)
	    for result = (concatenate 'string 
			   (get-node-description-string 
			    (sneps:choose.ns ns-cpy))
			   ", ")
	    then (concatenate 'string 
		   result
		   (get-node-description-string (sneps:choose.ns ns-cpy))
		   ", ")
	    do (setf ns-cpy (sneps:others.ns ns-cpy))
	    finally (return (concatenate 'string 
			result 
			(get-node-description-string (sneps:choose.ns ns-cpy))
			", and "
			(get-node-description-string 
			 (sneps:choose.ns (sneps:others.ns ns-cpy))))))))))
     
	
(defun get-description-function (n arc-list)
  "Retrieves the function that describes the node with the specified
   arc-list. First checks to see if it can retrieve it with the pred
   name followed by the arc-list, if not retrieves it with the cableset
   alone, if possible. This extra work was done due to certain SNeRE
   mode-3 constructs using the same arc sets."
  (let ((pred (first (second (sneps:node-snepslog n)))))
    (or (gethash (cons (intern pred :snepslog) arc-list) *argsdesc*)
	(gethash (cons nil arc-list) *argsdesc*))))

(defun make-description-function (ordered-arc-list desc-string)
  "Parses desc-string and creates a function that returns a description
   for the case-frame arcs. Added 9/13/06 - mwk3"
  (let* ((desc-string-copy desc-string)
	 (arc-list 
	  (loop  
	      for lb-pos = (position #\[ desc-string-copy)
	      for rb-pos = (position #\] desc-string-copy)
	      until (not lb-pos)
	      collect (intern 
		       (with-input-from-string 
			   (str (subseq desc-string-copy 
					(1+ lb-pos) rb-pos)) (read str)) :snepslog) 
	      do (setf desc-string-copy 
		   (concatenate 'string 
		     (subseq desc-string-copy 0 lb-pos) 
		     "~A"
		     (subseq desc-string-copy (1+ rb-pos)))))))
    (when (not (subsetp arc-list ordered-arc-list))
      (parseError "~A are not arcs in this caseframe.~%"
	     (set-difference
	      arc-list
	      (remove nil ordered-arc-list))))
    (setf (gethash  ordered-arc-list  *argsdesc*) 
      (lambda (n) 
	(apply #'format nil desc-string-copy
	       (loop 
		   for arc in arc-list
		   for node-set = (sneps:nodeset.n n arc)
		   collect
		     (make-node-set-description-string node-set)))))))

(defun define-frame (pred list-of-args &optional desc-str)
  "The first on the LIST-OF-ARGS is the arc to PRED (NIL if ignored).
   The rest on LIST-OF-ARGS are the arcs for the arguments, in order. 
   Modified to include optional description string code. -mwk3"
  (declare (special *propargs* *argsprop* *silent-mode-3*))
  (check-type pred symbol)
  (let ((ordered-arcs (sort (copy-seq (if (first list-of-args)
					 list-of-args
				       (rest list-of-args)))
			    #'sneps::isless.r)))
    (cond
     ((and (gethash pred *propargs*) 
	   (gethash ordered-arcs *argsprop*))
      (warn "~A is already associated with the same case-frame. Ignoring
redeclaration." pred) (return-from define-frame))
     ((gethash pred *propargs*)
      (parseError "~A is already associated with the case-frame ~A."
	     pred (gethash pred *propargs*)))) 
    (check-type list-of-args list)
    (assert (every #'symbolp list-of-args) (list-of-args)
      "The list of arc labels must be an unquoted list of symbols.")
    (assert (every #'identity (rest list-of-args)) (list-of-args)
      "No arc label after the first may be nil.")
    (let ((predchoice (or (first list-of-args) (list pred))))
    (mapc #'define-if-not-yet-defined ordered-arcs)
    (when (and (gethash ordered-arcs *argsprop*)
	       (not (equal (gethash ordered-arcs *argsprop*) predchoice)))
      (parseError
       "That case-frame is already associated with ~:[the relation~;the predicate~] ~A."
	     (consp (gethash ordered-arcs *argsprop*))
	     (gethash ordered-arcs *argsprop*)))
    (setf (gethash pred *propargs*) list-of-args
	  (gethash ordered-arcs *argsprop*) predchoice)
    (when desc-str
      (let ((arc-key 
	     (if (first list-of-args)
		 (cons pred ordered-arcs)
	       (cons nil ordered-arcs))))
	  (make-description-function arc-key desc-str))))
  (unless *silent-mode-3*
    (feedback pred list-of-args))))

(defun make.snepslog3 ()
  "changes the way snepslog relations are represented in sneps.
   Makes the arcs to the arguments be indexed on the predicate"
  ;; Initialize hash tables to predefined case-frames
  (clrhash *PROPARGS*)
  (clrhash *ARGSPROP*)
  ;; Set Mode 3 functions
  (setf (get 'mode 'relation-argument.list) #'relation-argument.list_mode.3
	(get 'mode 'relation-predicate) #'relation-predicate_mode.3
	(get 'mode 'make-relation) #'make-relation.3))

(defun make-relation.3 (action relation arguments)
  "Receives an action, a relation, and the arguments of the relation.
   Returns the SNePSUL command that perform the action on the relation,
   following the protocol of mode 3."
  (declare (special *PROPARGS*))
  (cons action
	(mapcan #'(lambda (arc arg)
		    (when arc (list arc arg)))
		(gethash relation *PROPARGS*)
		(cons relation arguments))))

(defun relation-predicate_mode.3 (node)
  "Given a sneps node as argument (it should be a relation node),
   returns the relation predicate.
   Works only in snepslog version 3."
  (multiple-value-bind (predchoice foundp)
      (gethash (node-caseframe node) *ARGSPROP*)
    (when foundp
      (if (consp predchoice) (first predchoice)
	(let* ((snepslogversion
	       (sneps:node-snepslog 
		(sneps:choose.ns (sneps:nodeset.n node predchoice))))
	  (pred (cond ((consp snepslogversion)
		 (if (eq (first snepslogversion) 'snepslog::snepslog-version)
		     (second snepslogversion)
		   snepslogversion))
		((null snepslogversion)
		 (sneps:node-na
		  (sneps:choose.ns (sneps:nodeset.n node predchoice))))
		(t (sneps:node-snepslog 
		    (sneps:choose.ns (sneps:nodeset.n node predchoice)))))))
	  (if (stringp pred)
	      (intern pred :snepslog)
	    pred))))))

(defun relation-argument.list_mode.3 (node)
  "Given a sneps node as argument (it should be a relation node),
   returns the relation arguments.
   Works only in snepslog version 3."
  (mapcar #'(lambda (arc) (sneps:nodeset.n node arc))
	  (rest (gethash (relation-predicate_mode.3 node) *PROPARGS*))))

(defun node-caseframe (node)
  (let (caseframe)
    (do* ((cs (sneps:node-fcableset node) (cddr cs))
	  (arc (first cs) (first cs)))
	((null cs) (nreverse caseframe))
      (when (sneps:isdn.r arc) (push arc caseframe)))))

(defun get-snepslog-version (node)
  (if (typep node 'node)
      (let ((node-snepslog (sneps:node-snepslog node)))
	(if (listp node-snepslog)
	    (getf node-snepslog 'snepslog-version)
	  node-snepslog))
    node))

(defun feedback (pred list-of-args)
  (format outunit "~&~A" pred)
  (showargs (1- (length list-of-args)))
  (format outunit " will be represented by {")
  (showframe pred list-of-args)
  (format outunit "}~%"))

(defun showargs (len)
  (format outunit "(x1")
  (dotimes (i (1- len))
    (format outunit ", x~D"  (+ i 2)))
  (format outunit ")"))

(defun showframe (pred arcs)
  (if (first arcs)
      (format outunit "<~A, ~A>, <~A, x1>" (first arcs) pred (second arcs))
    (format outunit "<~A, x1>" (second arcs)))
  (let ((i 2))
    (dolist (arc (cddr arcs))
      (format outunit ", <~A, x~D>" arc i)
      (incf i))))
