;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XGINSENG; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: layout.lisp,v 1.1 2011/05/24 17:59:36 mwk3 Exp $

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


(in-package :xginseng)


;; Contains functions to save and restore XGinseng layouts of
;; SNePS networks:


(defun save-node (node &optional (stream t))
  "Writes an appropriate create-instance form to STREAM which
can generate the XGinseng NODE."
  (format
   stream
   "~&(create-instance '~a node~
    ~%  (:box '~s)~
    ~%  (:name ~s)~
    ~%  (:sneps-node (sneps:node '~s)))"
   (kr:name-for-schema node)
   (get-value node :box)
   (get-value node :name)
   (sneps:node-na (get-value node :sneps-node))
   ))

;; This does not yet take care of double arcs!!
(defun save-layout (file)
  "Saves all nodes and arcs displayed in the display-window to FILE. The
relative positions will be stored in a way such that restore-layout can
generate the exact same layout."
  (with-open-file (out (user:sneps-translate file) :direction :output)
    (let ((nodes (get-value display-window :nodes-on-display)))
      ;; Read symbols into XGinseng package
      (format out "(in-package :xginseng)~%")
      ;; Save the nodes
      (dolist (node nodes)
	(save-node node out))
      ;; Add all the nodes to the display-window and aggregate
      (format
       out
       "~&(dolist (node '~a)~
        ~%  (push (eval node) (g-value display-window :nodes-on-display))~
        ~%  (opal:add-component display-aggregate (eval node)))"
       (mapcar #'kr:name-for-schema nodes))
      ;; Find all arcs...
      (dolist (node nodes)
	(dolist (down-cable (get-value node :down-cables))
	  ;;...and generate them and add them to the aggregate
	  (format
	   out
	   "~%(opal:add-component display-aggregate (make-arc ~a ~a ~s) :back)"
	   (kr:name-for-schema node)
	   (kr:name-for-schema (second down-cable))
	   (get-value (first down-cable) :label1))))
      (terpri out)
      )))

;; A simple restore function - DANGEROUS, does not check whether
;; there is any important stuff in the display window that should
;; not get overridden!!
(defun restore-layout (file &optional (interactive t))
  "Restores a previously saved layout from FILE. If INTERACTIVE is T
XGinseng waits for user input after reading the FILE, if it is NIL it
just returns. NOTE: This function blindly adds stuff to the
display-window - this is DANGEROUS - use only if display-window is
empty!! Also assumes that all the proper SNePS nodes are already
created."
  (sneps:in.environment
   :functions ((inter:main-event-loop #'inter:exit-main-event-loop))
   :eval (progn
	   (xginseng::xginseng)
	   (user:sneps-load file)
	   (opal:update display-window)))
  (cond (interactive
	 (inter:main-event-loop))
	(t (inter:exit-main-event-loop))))


;; This should probably make it into the regular release:

(defun cleanup ()
  "Function to call in case something goes wrong. Should get rid of most
of the undesirable Garnet/Xginseng state that leads to never ending errors
upon new invocations of XGinseng due to a previous error."
  (dolist (object '(display-window button-window dialogue-window
		    prompt file-prompt click-prompt reply display-aggregate))
    (when (boundp object)
      (when (schema-p (eval object))
      (opal:destroy (eval object)))
      (makunbound object)))
  ;; Destroy any stray nodes/arcs/double-arcs that were missed
  ;; by the previous step:
  (dolist (package '(xginseng kr-debug))
    (do-symbols (symbol (find-package package))
      (multiple-value-bind (sym type)
	  (find-symbol (symbol-name symbol) package)
	(when (and (not (eq type :inherited))
		   (boundp sym)
		   (kr:schema-p (eval sym))
		   (not (member sym '(arc double-arc node)))
                   (or (is-a-p (eval sym) arc)
		       (is-a-p (eval sym) double-arc)
		       (is-a-p (eval sym) node)))
	  (opal:destroy (eval sym))
	  (makunbound sym)
	  )))))



    
    




