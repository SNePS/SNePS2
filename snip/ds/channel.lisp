;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2011 Research Foundation of 
;;                          State University of New York

;; Version: $Id: channel.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :snip)


; =============================================================================
;
; <channel>  ::=  ( <filter> <switch> <context> <destination> <valve> )
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.ch      :  <universal> --> <boolean>
;
; CONSTRUCTORS   make.ch    :  <filter> x <switch> x <context> x
;                                   <destination> x <valve> --> <channel>
;
; SELECTORS      filter.ch      : <channel> --> <filter>
;                switch.ch      : <channel> --> <switch>
;                context.ch     : <channel> --> <context>
;                destination.ch : <channel> --> <destination>
;                valve.ch       : <channel> --> <valve>
;
; TESTS          isopen.ch      : <channel> --> <boolean>
;                isclosed.ch    : <channel> --> <boolean>
;                equivalent.ch  : <channel> x <channel> --> <boolean>
;
; UTILITY        open.ch      : <channel> -->
;                close.ch     : <channel> -->
;
; =============================================================================



;
; =============================================================================
;
; make.ch
; -------
;
;       arguments     : f - <filter>
;                       s - <switch>
;                       c - <context>
;                       d - <destination>
;                       v - <valve>
;
;       returns       : <channel>
;
;       description   : returns the <channel> consisting of the components
;                        passed as arguments
;
;                                        written :  rgh 07/29/85
;                                        modified:  rgh 08/19/85
;                                        modified:  cpf 10/06/88
;
(defun make.ch (f s c d v)
  (vector f s c d v))
;
;
; =============================================================================
;
; filter.ch
; ---------
;
;       arguments     : channel  -  <channel>
;
;       returns       : <filter>
;
;       description   : selects the filter of the channel
;
;                                        written :  rgh 05/09/85
;                                        modified:  rgh 06/11/85
;
;
(defun filter.ch (channel)
  (svref channel 0))
;
;
; =============================================================================
;
; switch.ch
; ---------
;
;       arguments     : channel  -  <channel>
;
;       returns       : <switch>
;
;       description   : selects the switch of the channel
;
;                                        written :  rgh 05/09/85
;                                        modified:  rgh 06/11/85
;
;
(defun switch.ch (channel)
  (svref channel 1))
;
; =============================================================================
;
; context.ch
; ----------
;
;       arguments     : channel  -  <channel>
;
;       returns       : <context>
;
;       description   : selects the context of the channel
;
;                                        written :  cpf 10/06/88
;                                        modified:  
;
;
(defun context.ch (channel)
  (svref channel 2))
;
;
; =============================================================================
;
; destination.ch
; --------------
;
;       arguments     : channel  -  <channel>
;
;       returns       : <destination>
;
;       description   : selects the destination of the channel
;
;                                        written :  rgh 05/09/85
;                                        modified:  rgh 08/18/85
;                                        modified:  cpf 10/06/88
;
(defun destination.ch (channel)
  (svref channel 3))
;
;
; =============================================================================
;
; valve.ch
; --------
;
;       arguments     : channel  -  <channel>
;
;       returns       : <valve>
;
;       description   : selects the valve of the channel
;
;                                        written :  rgh 08/19/85
;                                        modified:  cpf 10/06/88
;
;
(defun valve.ch (channel)
  (svref channel 4))

(defsetf valve.ch (channel) (newvalvevalue)
  `(setf (svref ,channel 4) ,newvalvevalue))
;
;
; =============================================================================
;
; is.ch
; -----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <channel>,
;                               "false" otherwise
;
;                                        written :  rgh  7/29/85
;                                        modified:  rgh  8/19/85
;                                                   rgh  3/08/86
;                                                   cpf 10/06/88
(defun is.ch (u)
  (and (vectorp u)
       (is.filter (filter.ch u))
       (is.switch (switch.ch u))
       (sneps:is.ct (context.ch u))
       (is.dest (destination.ch u))
       (is.valve (valve.ch u))))
;
;
; =============================================================================
;
; isopen.ch
; ---------
;
;       arguments     : channel - <channel>
;
;       returns       : <boolean>
;
;       description   : returns "true" if the <valve> of "channel" is open
;
;                                        written :  rgh 08/19/85
;                                        modified:
;
;
(defun isopen.ch (channel)
  (eq (valve.ch channel) 'OPEN))
;
;
; =============================================================================
;
; isclosed.ch
; -----------
;
;       arguments     : channel - <channel>
;
;       returns       : <boolean>
;
;       description   : returns "true" if the <valve> of "channel" is closed
;
;                                        written :  rgh 08/19/85
;                                        modified:
;
;
(defun isclosed.ch (channel)
  (eq (valve.ch channel) 'CLOSED))
;
;
; =============================================================================
;
; equivalent.ch
; -------------
;
;       arguments     : ch1 - <channel>
;                       ch2 - <channel>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "ch1" and "ch2" are equivalent
;                       in that their destinations and contexts are equal,
;                       and their filters and switches define equivalent
;                       restrictions.
;                       Note that their valves are not tested, so that a
;                       closed channel may match an otherwise equivalent
;                       open one.
;
;                                        written :  rgh 10/06/85
;                                        modified:  rgh  3/22/86
;                                        modified:  cpf 10/06/88
;
(defun equivalent.ch (ch1 ch2)
  (and (iseq.dest (destination.ch ch1) (destination.ch ch2))
       (equivalent.restr
	(make.restr (filter.ch ch1)) 
	(make.restr (filter.ch ch2)))
       (equivalent.restr
	(make.restr (switch.ch ch1))
	(make.restr (switch.ch ch2)))
       (sneps:iseq.ct (context.ch ch1) (context.ch ch2))))
;
;
; =============================================================================
;
; open.ch
; -------
;
;       arguments     : channel - <channel>
;
;       description   : destructively modifies "channel" so that its <valve>
;                       is set to OPEN
;
;                                        written :  rgh 08/19/85
;                                        modified:  rgh 11/30/85
;                                        modified:  cpf 10/06/88
;
(defun open.ch (channel)
  (setf (valve.ch channel) 'OPEN))
;
;
; =============================================================================
;
; close.ch
; --------
;
;       arguments     : channel - <channel>
;
;       description   : destructively modifies "channel" so that its <valve>
;                       is set to CLOSED
;
;                                        written :  rgh 08/19/85
;                                        modified:  rgh 11/30/85
;                                        modified:  cpf 10/07/88
;
(defun close.ch (channel)
  (setf (valve.ch channel) 'CLOSED))
;
;
; =============================================================================



    
    




