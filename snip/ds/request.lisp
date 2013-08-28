;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: request.lisp,v 1.2 2013/08/28 19:07:27 shapiro Exp $

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




(in-package :snip)


; =============================================================================
;
; <request>  ::=  <channel>
;
; =============================================================================
;
; RECOGNIZERS    is-cq-to-rule.req   : <request> --> <boolean>
;                is-rule-to-ant.req  : <request> --> <boolean>
;                is-node-to-node.req : <request> --> <boolean>
;                is-from-user.req    : <request> --> <boolean>
;
; =============================================================================
;
; is-cq-to-rule.req
; -----------------
;
;       arguments     : request  -  <request>
;
;       returns       : <boolean>
;
;       description   : determines whether or not the request is from a node
;                       in consequent position to a dominating rule node
;
;       implementation: checks to see if the request was sent by a node which
;                       is at the end of a cq, arg, or dcq arc, since these
;                       are the only arcs which point to the consequent of a
;                       rule.
;
;                                        written :  rgh  6/11/85
;                                        modified:  rgh  3/08/86
;                                                     dk 6/2/93
; added the check to an IF arc for a DO-IF rule -: dk
;
;
(defmacro is-cq-to-rule.req (request)
   `(let ((sender (destination.ch ,request)))
       (and (is.n sender)
            (or (ismemb.ns sender (nodeset.n *NODE* 'cq))
                (ismemb.ns sender (nodeset.n *NODE* 'arg))
                (ismemb.ns sender (nodeset.n *NODE* 'if)) ; a DO-IF rule
                (ismemb.ns sender (nodeset.n *NODE* 'dcq))))))
;
; 
; =============================================================================
;
; is-rule-to-cq.req
; ------------------
;
;       arguments     : request  -  <request>
;
;       returns       : <boolean>
;
;       description   : determines whether or not the request is from a rule
;                       node to a node in consequent position of the rule
;
;       implementation: checks to see if the request was sent by a node which
;                       is at the end of a cq-, or arg- arc, since
;                       these are the only arcs which point to a rule for
;                       which the current node is in consequent position.
;
;                                        written :  cpf     11/04/88
;                                        modified:  cpf/njm 07/12/89
;
;
(defmacro is-rule-to-cq.req (request)
  `(let ((sender (destination.ch ,request)))
    (and (is.n sender)
	 (or (ismemb.ns sender (nodeset.n *NODE* 'sneps::cq-))
	     (ismemb.ns sender (nodeset.n *NODE* 'sneps::arg-))))))
;
;
; =============================================================================
;
; is-rule-to-ant.req
; ------------------
;
;       arguments     : request  -  <request>
;
;       returns       : <boolean>
;
;       description   : determines whether or not the request is from a rule
;                       node to a node in antecedent position of the rule
;
;       implementation: checks to see if the request was sent by a node which
;                       is at the end of a ant-, arg-, or &ant- arc, since
;                       these are the only arcs which point to a rule for
;                       which the current node is in antecedent position
;
;                                        written :  rgh  6/11/85
;                                        modified:  rgh  3/08/86
;
;
(defmacro is-rule-to-ant.req (request)
   `(let ((sender (destination.ch ,request)))
       (and (is.n sender)
            (or (ismemb.ns sender (nodeset.n *NODE* 'ant-))
                (ismemb.ns sender (nodeset.n *NODE* 'arg-))
                (ismemb.ns sender (nodeset.n *NODE* '&ant-))))))
;
;
; =============================================================================
;
; is-node-to-node.req
; -------------------
;
;       arguments     : request  -  <request>
;
;       returns       : <boolean>
;
;       description   : determines whether or not the request is from a node
;                       at the other end of a match channel
;
;       implementation: checks to see if the request is neither of the type
;                       cq-to-rule nor rule-to-ant, since they are the only
;                       other possible types of requests
;
;                                        written :  rgh  6/11/85
;                                        modified:
;
;
(defmacro is-node-to-node.req (request)
  `(and (not (is-cq-to-rule.req ,request))
	(not (is-rule-to-ant.req ,request))
	(not (is-rule-to-cq.req ,request))))
;
;
; =============================================================================
;
; is-from-user.req
; ----------------
;
;       arguments     : request - <request>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "request" is from the USER-PROCESS
;                       which was set up by a call to deduce, "false"
;                       otherwise
;
;                                        written :  rgh  3/08/86
;                                        modified:
;
;
(defmacro is-from-user.req (request)
  `(is-user.dest (destination.ch ,request)))
;
;
; =============================================================================



    
    




