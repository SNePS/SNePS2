;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: dd.lisp,v 1.2 2011/08/19 19:15:06 shapiro Exp $

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
#+allegro
(use-package :javatools.jlinker)

; ==========================================================================
;
; describe 
; --------
;
;      arguments     : snepsul-exp - <snepsul-exp>
;
;      returns       : <node set>
;
;      description   : This calls "sneval" to evaluate "snepsul-exp" to 
;                      get the desired <node set>. 
;                      It prints the description of each <node> in the 
;                      <node set> that has not yet been described during 
;                      the process; the description includes the 
;                      description of all <node>s dominated by the <node>.
;                      It returns the <node set>.
;
;      implementation: Stores the <node>s which have already been described 
;                      in "describe-nodes".
;                      Before tracing the description of a <node>, it 
;                      checks whether the <node> was already been described 
;                      to avoid describing the same <node> repeatedly. 
;                      The variable "describe-nodes" is updated by "des1".
;
;      side-effects  : Prints the <node>'s descriptions.
;
;                                         written:  CCC 07/28/83
;                                         modified: CCC 09/26/83
;                                                   ejm 10/10/83
;                                                   njm 09/28/88
;                                                   njm  4/27/89
;                                                    hc  1/06/92
;                                                    hc  7/18/93
;
(defsnepscom describe ((&rest snepsul-exp) (top ns bns tbns fns))
  (declare (special outunit))
  (let* ((crntct (processcontextdescr snepsul-exp))
	 (ns (in-context.ns (nseval (getsndescr snepsul-exp))
			    crntct))
	 (described-nodes (new.ns))
	 (full nil))
    (declare (special crntct described-nodes full))
    (when ns
      (terpri outunit)
      (mapc #'(lambda (n)
		(if (not (ismemb.ns n described-nodes))
		    (PP-nodetree (des1 n) outunit)))
	    ns))
    (values ns crntct)))


; ==========================================================================
;
; full-describe 
; -------------
;
;      arguments     : snepsul-exp - <snepsul-exp>
;
;      returns       : <node set>
;
;      description   : This calls "sneval" to evaluate "snepsul-exp" to 
;                      get the desired <node set>. 
;                      It prints the description of each <node> in the 
;                      <node set> that has not yet been described during 
;                      the process; the description includes the 
;                      description of all <node>s dominated by the <node>
;                      and the description of the support of the node, if 
;                      it exists.
;                      It returns the <node set>.
;
;      implementation: Stores the <node>s which have already been described 
;                      in "describe-nodes".
;                      Before tracing the description of a <node>, it 
;                      checks whether the <node> was already been described 
;                      to avoid describing the same <node> repeatedly. 
;                      The variable "describe-nodes" is updated by "des1".
;                      The variable "full" informs "des1" that a description 
;                      of the support of nodes is required. 
;
;      side-effects  : Prints the <node>'s descriptions.
;
;                                         written :  njm 09/28/88
;                                         modified:  njm 04/27/89
;                                                     hc  1/06/92
;                                                     hc  7/18/93
;
(defsnepscom full-describe ((&rest snepsul-exp) describe)
  (declare (special outunit))
  (let* ((crntct (processcontextdescr snepsul-exp))
	 (ns (in-context.ns (nseval (getsndescr snepsul-exp))
			    (iscontext-given snepsul-exp)))
	 (described-nodes (new.ns))
	 (full T))
    (declare (special described-nodes crntct full))
    (when ns
      (terpri outunit)
      (mapc #'(lambda (n)
		(if (not (ismemb.ns n described-nodes))
		    (PP-nodetree (des1 n) outunit)))
	    ns))
    (values ns crntct)))
 
;
; ==========================================================================
;
; des1 
; ----
;
;      arguments     : n - <node>
;
;      returns       : <sequence> 
;
;      nonlocal-vars : described-nodes - <node set> - defined in "describe"
;                      full            - <boolean>  - defined in "describe"
;
;      description   : This checks if the <node> has been described or not.  
;                      If not, it puts the <node> in "describe-nodes" and 
;                      gets the <cable set> of the <node>. 
;                      For each <cable> in the <cable set>, if the 
;                      <relation> is an ascending <relation>, it does 
;                      nothing.
;                      If not an ascending <relation> it lists the 
;                      <relation> and the description(s) of the <node set> 
;                      which the <relation> points to.
;                      To get the description(s) of the <node set>, "des1" 
;                      calls itself for each <node> in the <node set>.
;                      Before tracing the description of a <node>, it 
;                      checks if the <node> has already been described 
;                      to avoid describing the same <node> repeatedly.
;                      Depending on the value of the variable "full", a
;                      description of the node's support is can be appended 
;                      to the end of its description. 
;                      
;      side-effects  : Variable "described-nodes" is side-effected.
;
;                                         written:  CCC 07/28/83
;                                         modified: CCC 09/26/83
;                                                   ssc 02/11/87
;                                                   njm 09/28/88
;
					
;;; Previous version converted a dominated node that looks like a number
;;;    into a number when placing it into the description.
;;; This version leaves it as a node.
(defun des1 (n)
  (declare (special described-nodes full))
  (cons (describe.n n)
	(unless (ismemb.ns n described-nodes)
	  (setf described-nodes (insert.ns n described-nodes)) 
	  ;;  describe the cable set
	  (append 
	   (mapcan
	    #'(lambda (c)
		(unless (isup.r (relation.c c))
		  (list
		   (cons (relation.c c)
			 (mapcar #'(lambda (n1)
				     (cond ((or (isbase.n n1) (isvar.n n1))
					    (setf described-nodes
					      (insert.ns n1 described-nodes))
					    n1)
					   (t (des1 n1))))
				 (nodeset.c c))))))
	    (n-to-cs n))
	   ;;  describe the context cable set
	   (when full
	     (descrcontext-cable-set (node-asupport n)))))))

;
; ==========================================================================
;
; dump 
; ----
;
;      arguments     : snepsul-exp - <snepsul-exp>
;
;      returns       : <node set>
;
;      description   : It calls "sneval" to evaluate "snepsul-exp" to get 
;                      the desired <node set>.  It prints out the <node> 
;                      together with its <cable set> for each <node> in 
;                      the <node set>.
;
;      side-effects  : Prints out the <node> and its <cable set>.
;                     
;
;                                         written:  CCC 07/28/83
;                                         modified: CCC 09/26/83
;                                                   ejm 10/10/83
;                                                   njm 04/27/89
;                                                    hc  1/06/92
;                                                    hc  7/18/93
;
(defsnepscom dump ((&rest snepsul-exp) describe)
  (declare (special outunit))
  (let* ((crntct (processcontextdescr snepsul-exp))
	 (ns (in-context.ns
	      (nseval (getsndescr snepsul-exp))	       
	      crntct)))
    (declare (special crntct))
    (when ns
      (terpri outunit)
      (mapc #'(lambda (n)
		(princ (list n (node-fcableset n)) outunit)
		(terpri outunit))
	    ns))
    (values ns crntct)))
 
;
; ==========================================================================
;
; PP-nodetree
; -----------
;
;       arguments     : nodetree - <nodetree>
;                       stream - an output stream
;
;       returns       : <never mind>
;
;       description   : Pretty print the given <nodetree> to <stream>.
;
;       side-effects  : Pretty print the <nodetree>.
;
;                                          written : CCC 09/26/83
;                                          modified: hc 10/2/90
;                                                        1/6/92
;                                                    aec 1/23/96
 
(defun PP-nodetree (nodetree &optional (stream outunit))
  (format stream "~&")
  (write nodetree :stream stream :pretty t :escape nil)
  (terpri stream))


;;; This version uses describe if neither a grammar nor a lexicon is loaded.
(defsnepscom surface ((&rest snepsul-exp))
  "Hands its argument node set to the generation grammar starting in state G."
  (declare (special outunit parser:*atn-arcs-hashtable* englex:*lexicon*))
  (cond ((and (not (initialized-p parser:*atn-arcs-hashtable*))
	      (not (initialized-p englex:*lexicon*)))
	 (eval `(full-describe ,snepsul-exp)))
	((not (initialized-p parser:*atn-arcs-hashtable*))
	 (sneps-error
	  (format
	   nil "No ATN grammar is loaded. You must load a grammar via~
           ~%`(atnin \"<atn-grammar-filename>\")' before using `surface'.")
	  'describe 'surface))
	((not (initialized-p englex:*lexicon*))
	 (warn "No lexicon is loaded. You should load a lexicon via~
           ~%`(lexin \"<lexicon-filename>\")' before using `surface'."))
	(t (let* ((parser:*trace-level* -1)
		  (parser:*all-parses* nil)
		  (response
		   (parser:flatten
		    (parser::internal-parse (nseval snepsul-exp) 'snepsul::G))))
	     (declare (special parser:*trace-level* parser:*all-parses*))
	     (when response
	       (format outunit
		       "~%~a.~%"
		       (string-trim "()" (princ-to-string response))))
	     (values)))))


(defun  slight-surface (node &optional (stream nil))
  (format stream "~A" node))

(defun format.ns (nsf)
  "Formats a nodesetform that contains either infix or postfix operations into 
   prefix form for evaluation."
  (declare (special nsf))
  (loop
    (cond ((eq (second nsf) '!)
	   (setf nsf
		 (cons (list '! (first nsf)) (cddr nsf))))
	  ((isrearrange.com (second nsf))
	   (cond ((third nsf)
		  (setf nsf
			(cons (list (second nsf) (first nsf) (third nsf)) (cdddr nsf))))
		 (t (sneps-error (format nil
					 "Infix operator missing second operand: ~A"
					 (cdr nsf))
				 '|Evaluation of node description|
				 'format.ns))))
	  (t (return nsf)))))

(defun ddeval (snepsul-exp)
    (let (result)
      (declare (special result))
      (setq result
	    (cond ((null snepsul-exp) (new.ns))
		  ((numberp snepsul-exp) (ddeval (un-ize snepsul-exp)))
		  ((atom snepsul-exp) (nseval snepsul-exp))
		  ((isfns.com (first snepsul-exp))
		   ((lambda (ev-result)
		      (if (atom ev-result) (ddeval ev-result) ev-result))
		    (eval (find-inside snepsul-exp))))
		  (t (nseval snepsul-exp))))
      result))

; ==========================================================================
;
; descrcontext-cable-set 
; ----------------------
;
;      arguments     : ctcs - <context cable set>
;
;      returns       : <sequence>
;
;      description   : it lists the description of the <context cable set> 
;                      "ctcs".
;
;
;                                         written :  njm 09/28/88
;                                         modified: 
;                                                   
;
(defun descrcontext-cable-set (ctcs)
  "Lists the description of the <context cable set> `ctcs'"
  (cond ((isnew.ctcs ctcs) nil)
	(t (append (descrcontext-cable (ot.ctcs ctcs)
						 (contextset.ctcs ctcs))
		   (descrcontext-cable-set (others.ctcs ctcs))))))



; ==========================================================================
;
; descrcontext-cable 
; ------------------
;
;      arguments     : otag - <otag>
;                      cts - <context set>
;
;      returns       : <sequence>
;
;      description   : it lists the description of the <context cable> 
;                      defined by the "otag" and "cts".
;
;
;                                         written :  njm 09/28/88
;                                         modified: 
;                                                   
;
(defun descrcontext-cable (otag cts)
  "Lists the description of the <context cable> defined by `otag'
   and `cts'"
  (cond ((isnew.cts cts) nil)
	(t (cons (list otag (descrcontext (choose.cts cts)))
		   (descrcontext-cable otag (others.cts cts))))))



;;;========================= Code for show ==============================


(export '(load-sneps-text eval-query end  sneps-gui-show print-graph))
  

;;; The Jung version of show can only be run in allegro. Disable the
;;; Jung version of show if the SNePS GUI started this SNePS process

#+allegro
(when cl-user:*use-gui-show* 
  (defvar *bc* 
      ""
    "The character that separates different entries in a classpath.")
  
  #+mswindows (setf *bc* ";") 
  #-mswindows (setf *bc*  ":") 
  
  (defvar *dc* 
      ""
    "The character that separates directories in a path.")
  
  #+mswindows (setf *dc* "\\") 
  #-mswindows (setf *dc* "/")
  
  
  (defvar *sneps-gui-directory* 
      ""
    "Directory containing the sneps gui files.")
  
  
  ;;;Create SNePSGUIConstructor
  (def-java-class (sneps-gui "SNePSGUIShow")
      () () () ())

  ;;;Create and show GUI
  (def-java-constructor new-sneps-gui (sneps-gui "boolean"))
  (def-java-method (set-visible "setVisible") (sneps-gui "boolean"))
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Create and describe the classes jlinker needs to work with
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Create SNePSGUINode and SNePSGUIArc Constructors (and general edge/vertex)
  (def-java-class (sneps-gui-node "SNePSGUINode") ()
    (((gsn-mol "MOL") :reader gsn-mol)
     ((gsn-base "BASE") :reader gsn-base)
     ((gsn-var "VAR") :reader gsn-var)
     ((gsn-pat "PAT") :reader gsn-pat))
    () ())
  
  (def-java-class (vertex "edu.uci.ics.jung.graph.Vertex") () () () ())
  (def-java-class (sneps-gui-arc "SNePSGUIArc")
      () () () ())
  (def-java-class (edge "edu.uci.ics.jung.graph.Edge") () () () ())
  
;;;SNePSGUINode(node_access, description, snepslog)
  (def-java-constructor new-sneps-gui-node 
      (sneps-gui-node "java.lang.String" "java.lang.String"
		      "java.lang.String" "int"))
  
;;;SNePSGUIArc(arc_label, from_node, to_node)
  (def-java-constructor new-sneps-gui-arc
      (sneps-gui-arc "java.lang.String" "SNePSGUINode" "SNePSGUINode"))
  
;;;Create SparseGraph constructor and methods
  (def-java-class 
      (sparse-graph "edu.uci.ics.jung.graph.impl.SparseGraph")
      () () () ())
  (def-java-constructor new-sparse-graph (sparse-graph))
  
  (def-java-method (add-vertex  "addVertex") 
      (sparse-graph "edu.uci.ics.jung.graph.Vertex"))
  (def-java-method (add-edge  "addEdge") 
      (sparse-graph "edu.uci.ics.jung.graph.Edge"))
  
;;;More GUI functions needed
  (def-java-method (display-network "displayNetwork") 
      (sneps-gui "edu.uci.ics.jung.graph.impl.SparseGraph"))
  (def-java-method (get-node-access "getNodeAccess")
      (sneps-gui-node))

  
  (defun get-node-id (node)
    "Returns the java type needed for drawing a SNePS node."
    (check-type node node)
    (cond
     ((isvar.n node) (gsn-var))
     ((isbase.n node) (gsn-base))
     ((ispat.n node) (gsn-pat))
     ((ismol.n node) (gsn-mol))))
  
  
  (defun sneps-gui-show (&optional nodeset)
    "Modified from the show command. Creates java versions of the sneps
  nodes and arcs for display."
    (when (not (jlinker-query))
      (setf *sneps-gui-directory*
	      (concatenate 'string 
		cl-user:*sneps-directory*
		*dc*
		"SnepsGUI"))
    ;; MS windows supposedly doesn't need the jl-config file
      #-mswindows
      (cl:load (concatenate 'string
		 cl-user:*sneps-directory*
		 *dc* "Jlinker" *dc*  "jl-config"))
      
      ;; Force it if necessary!
      (when cl-user:*force-jlinker-config* 
	(cl:load (concatenate 'string
		   cl-user:*sneps-directory*
		   *dc* "Jlinker" *dc* "jl-config")))
    
      ;; Set up the classpath variable
      (setf (sys:getenv "CLASSPATH")
	(concatenate 
	    'string
	  "."
	  *bc* *sneps-gui-directory* *dc*
	  *bc* *sneps-gui-directory* *dc* "SnepsGUIShow.jar"
	  *bc* cl-user:*jung-directory* *dc* "jung-1.7.6.jar"
	  *bc* cl-user:*colt-directory* *dc* "colt.jar"
	  *bc* cl-user:*colt-directory* *dc* "concurrent.jar"
	  *bc* cl-user:*commons-directory* *dc* "commons-collections-3.2.jar"
	  *bc* cl-user:*commons-directory* *dc* "commons-collections-testframework-3.2.jar"
	  *bc* cl-user:*xerces-directory* *dc* "resolver.jar"
	  *bc* cl-user:*xerces-directory* *dc* "xercesImpl.jar"
	  *bc* cl-user:*xerces-directory* *dc* "xercesSamples.jar"
	  *bc* cl-user:*xerces-directory* *dc* "xml-apis.jar"
	  *bc* *sneps-gui-directory* *dc* "swing-layout-1.0.jar"
	  *bc* *sneps-gui-directory* *dc* "AbsoluteLayout.jar"
	  *bc* cl-user:*sneps-directory* *dc* "Jlinker" *dc* "jlinker.jar"
	  *bc* cl-user:*jimi-directory* *dc* "jimi-1.0.jar"
	  (if cl-user:*eps-dump-directory*
	      (concatenate 'string 
		*bc* cl-user:*eps-dump-directory* *dc* "dump.jar")
	    "")))
      (jlinker-init)) 
    (let* ((sneps-gui-ptr (new-sneps-gui cl-user:*eps-dump-directory*))
	 (dsg (new-sparse-graph))
	   (vertex-list '())
	   (exp (if nodeset nodeset (sneps:* 'sneps:nodes)))
	   (crntct (processcontextdescr exp))
	   (ns (nseval (getsndescr exp)))
	   (described-nodes (new.ns))
	   (full nil))
      (declare (special sneps-gui-ptr dsg vertex-list crntct 
			described-nodes full ))
      (set-visible sneps-gui-ptr t)    
      (when ns
	(mapc #'(lambda (n)
		  (if (not (ismemb.ns n described-nodes))
		      (print-graph (des1 n))))
	      ns))
      (values ns crntct)
      (display-network sneps-gui-ptr dsg)))
  
  (defun print-graph (nodetree)
    "Modified from the show command. Creates java versions of the sneps
  nodes and arcs for display. Helper function."
    (declare (special dsg vertex-list))
    (let ((m-node (car nodetree))) 
      ;;loop through nodetree elements i.e. (arc node ...)
      (loop for list-elem in (cdr nodetree) 
			     ;;loop through each node in (arc node1 node2 ...)
	  do (loop for node in (cdr list-elem)
		 do (let* ((v1-string  (format nil "~A" 
					       (if (eql *package* (find-package :snepslog))
						   (snepslog::m->wff m-node)
						 m-node)))
			   (v1 
			    (or (first (member v1-string 
					       vertex-list 
					       :key #'get-node-access
					       :test #'string=))
				(first (cl:push (add-vertex dsg 
							    (new-sneps-gui-node 
							     v1-string
							     (snepslog::get-node-description-string m-node)
							     (snepslog::snepslog-print m-node nil)
							     (get-node-id m-node))) 
						vertex-list))))
			   (v2-sneps-node (if (atom node) node
					    (print-graph node)))
			   (v2-string  (format nil "~A" 
					       (if (eql *package* (find-package :snepslog))
							(snepslog::m->wff v2-sneps-node)
							v2-sneps-node)))
			   (v2 (or (first (member v2-string
						  vertex-list 
						  :key #'get-node-access
					      :test #'string=))
				   (first (cl:push (add-vertex dsg 
							       (new-sneps-gui-node 
								v2-string
							    (snepslog::get-node-description-string
							     v2-sneps-node)
							    (snepslog::snepslog-print v2-sneps-node nil)
							    (get-node-id v2-sneps-node)))
						   vertex-list))))
			   (e1 (new-sneps-gui-arc 
				(format nil "~A" (car list-elem))
				v1 v2)))
		      (add-edge dsg e1))))
      
      ;; return m-node
      (values m-node))))
  

#-allegro
(defun sneps-gui-show (ns)
  "Empty function for non-allegro lisps, or if the sneps gui started
this instance of sneps"
  nil)

;;; ==========================================================================
;;;
;;; parse-show-params
;;; -----------------
;;;
;;;      arguments     : snepsul-exp - <snepsul-exp>
;;;
;;;      returns       : list of the form:
;;;                      (list-of-nodes file-name
;;;                          output-format is-file-name-generated)
;;;
;;;      description   : this function attempts to locate the optional
;;;                      params to "show" command (i.e. :file and :format)
;;;                      if one or both of the parameters are not present
;;;                      their values are set to the default ones
;;;
;;;      implementation: if :file parameter is not found, a temp file
;;;                      name in /tmp directory is generated
;;;                      if :format parameter is not found, it is
;;;                      set to "gif"
;;;                      is-file-name-generated is needed to inform
;;;                      "show" that a file in /tmp will be created
;;;                      and will have to be deleted
;;;
;;;      side-effects  : none
;;;
;;;                                         written: ddligach 03/11/2004
;;;
(defun parse-show-params (snepsul-exp)
  (let ( ;; get optional file name param
	  (file-param (member :file snepsul-exp))
	;; get optional format param
	(format-param (member :format snepsul-exp))) 
    (list 
     ;; extract nodes
     (butlast snepsul-exp 
	      (max (length file-param) (length format-param)))
     
     ;; extract file parameter value
     (if (null (second file-param))
	 (string (gensym "/tmp/sneps-graph-")) ; default file name
       (string (second file-param)))
     
     ;; extract format parameter value
     (if (null (second format-param))
	 (string 'gif)		; default format
       (string (second format-param)))
     
     ;; indicate whether file name was passed as a param 
     ;; (and therefore, whether a temporary file 
     ;; in /tmp directory was generated)
     (null (second file-param)))))







;;; ==========================================================================
;;;
;;; show
;;; ----
;;;
;;;      arguments     : snepsul-exp - <snepsul-exp>
;;;
;;;      returns       : <node set>
;;;
;;;      Note          : Functionality is different depending on the value of
;;;                      cl-user:*use-gui-show*
;;;
;;;                      If *use-gui-show* is 't':
;;;
;;;      description   : This command is similar to describe. However, instead
;;;	                 of printing the description of each node, it produces
;;;                      a graphical representation of the network
;;;                      that can be manipulated, and saved to a file (right 
;;;                      click the image, and choose "Save As..."
;;;
;;;                      Sample calls:
;;;                      (show m1 m2 m3)
;;;                      (show *nodes)
;;;                     ;;;
;;;      implementation: The implementation is similar to describe. 
;;;		         The difference is that instead of printing
;;;                      the description of each node, it creates
;;;                      a Java representation of the node, which is sent to 
;;;                      a Java client for display.
;;;
;;;      side-effects  : none
;;;
;;;                                         written: mwk3 5/31/07 
;;;
;;;                     
;;;                      If *use-gui-show* is 'nil':
;;;
;;;      description   : This command is similar to describe. However, instead
;;;	                 of printing the description of each node, it produces
;;;                      a graphical representation of the network.
;;;                      This command takes two optional parameters:
;;;             
;;;                      :file <file_name> 
;;;                      :format <ps or gif>
;;;
;;;                      If no file name is specified, a temporary file
;;;                      is generated in /tmp directory.
;;;                      Default output format is "gif"
;;;                      Sample calls:
;;;                      (show m1 m2 m3)
;;;                      (show *nodes)
;;;                      (show m7 m8 :format ps)
;;;                      (show *nodes :file tmpFileName :format ps)
;;;
;;;      implementation: The implementation is similar to describe. 
;;;		         The difference is that instead of printing
;;;                      the description of each node, it opens
;;;                      a file and calls "generate-dot-file", which writes 
;;;                      a representation of the network in the DOT language. 
;;;                      Subsequently, the 'dot' compiler is invoked and an
;;;                      output file (gif or ps) is produced. The output file
;;;                      is displayed using xv or gv.
;;;
;;;      side-effects  : none
;;;
;;;                                         written: ddligach 02/21/2004 
;;;
(defsnepscom show ((&rest snepsul-exp) (top ns bns tbns fns))
  (declare (special outunit))
  (if cl-user:*use-gui-show*
      (let ((ns (nseval (getsndescr snepsul-exp))))
	(sneps-gui-show ns))

    ;; get gif and dot file mames
    (let* ((param-list (parse-show-params snepsul-exp))
	   (exp (first param-list))	; set of nodes
	   (file-name (second param-list)) ; file name
	   (output-format (third param-list)) ; output file format
	   (temp-file-generated (fourth param-list)) ; tmp file present?
	   (dot-file-name (merge-pathnames file-name "*.dot")) ; dot file
	   (output-file-name
	    (merge-pathnames file-name 
			     (format nil "*.~A" output-format)))) ; out file
	;; open file and write network specs in "dot" language
      (with-open-file (dot-stream dot-file-name
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
      (format dot-stream "digraph RE { ~%") ; write opening line to file
      ;; get nodes and write them to file (similar to "describe")
      ;; Original limited nodes shown to those in the context.
      ;;   I suspect this was just following what describe does.
      ;;   I changed that to allow all requested nodes to be shown.
      (let* ((crntct (processcontextdescr exp))
	     ;; (ns (in-context.ns (nseval (getsndescr exp)) crntct))
	     (ns (nseval (getsndescr exp)))
	     (described-nodes (new.ns))
	     (full nil))
	(declare (special crntct described-nodes full))
	(when ns
	  (terpri outunit)
	  (mapc #'(lambda (n)
		    (if (not (ismemb.ns n described-nodes))
			(generate-dot-file (des1 n) dot-stream)))
		ns))
	(values ns crntct))
      ;; done writing to file, now display the results
      (format dot-stream "} ~%"))	; write closing line to file
    ;; generate output file (gif or ps)
    ;; display it (with xv or gv)
    ;; remove it if it is a temporary file
      #-allegro
      (ext:run-shell-command 
       (format nil "dot -T~A ~A > ~A; ~A ~A; ~A &" 
	       output-format dot-file-name output-file-name
	       (if (equal output-format "ps") 
		   "gv" 
		 "xv") output-file-name
		 (if temp-file-generated
		     (format nil "/bin/rm -f ~A.*" file-name)
		   "")))
      #+allegro
      (excl:shell
       (format nil "dot -T~A ~A > ~A; ~A ~A; ~A &" 
	       output-format dot-file-name output-file-name
	       (if (equal output-format "ps") 
		   "gv" 
		 "xv") output-file-name
		 (if temp-file-generated
		   (format nil "/bin/rm -f ~A.*" file-name)
		 "")))))) 

;;; ==========================================================================
;;;
;;; generate-dot-file
;;; -----------------
;;;
;;;      arguments     : nodetree - <nodetree>
;;;                      dot-stream - an output stream
;;;
;;;      returns       : m-node
;;;
;;;      description   : Converts <nodetree> to dot language and 
;;;                      writes it to <stream> 
;;;
;;;      implementation: This function assumes that <nodetree> has
;;;                      the following general structure:
;;;                      (m-node (arc node* nodetree* )* )
;;;                         where '*' is a kleene star
;;;                      The outter loop walks through every element of 
;;;                      the nodetree. The inner loop parses each particular
;;;                      element of the nodetree and writes its 'dot'
;;;                      representation to dot-stream.
;;;                      General 'dot' file format:
;;;                          A -> B [label="C"]
;;;                          A and B are nodes, C is an ark between A and B
;;;
;;;      side-effects  : Writes to file the network's representation in
;;;                      'dot' language.
;;;
;;;                                         written: ddligach 02/21/2004
;;;
(defun generate-dot-file (nodetree dot-stream)
  (let ((m-node (car nodetree))) 
    (when (null (cdr nodetree))		; handle the case of showing a base node
      (format dot-stream "\"~A\"; ~%" m-node))
    ;; loop through nodetree elements i.e. (arc node ...)
    (loop for list-elem in (cdr nodetree) 
			   ;; loop through each node in (arc node1 node2 ...)
	  do (loop for node in (cdr list-elem)
		   do (format dot-stream "\"~A\" -> \"~A\" [label=\"~A\"]; ~%"
			      ;; match first '~A'
			      (if (eql *package* (find-package :snepslog))
				  (snepslog::m->wff m-node)
				m-node)
			      (if (atom node) 
				  node	; match second '~A' 
					; this is not an atomic node
					; so, it must be a nodetree
					; make a recursive call
				(let ((v2-sneps-node (generate-dot-file node dot-stream)))
				  (if (eql *package* (find-package :snepslog))
				      (snepslog::m->wff v2-sneps-node)
				    v2-sneps-node)))
			      (car list-elem)))) ; match third '~A' (an ark)
    ;; return m-node
    (values m-node)))
