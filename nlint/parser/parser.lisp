;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PARSER; Base: 10 -*-

;; Copyright (C) 1984--2011
;; Research Foundation of State University of New York

;; Version: $Id: parser.lisp,v 1.1 2011/05/24 17:59:37 mwk3 Exp $

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


(in-package :parser)

(defvar *config* nil "Global configuration variable.")

(defvar *state* 's "Default starting state in grammar.") 

(defvar *trace-level* 0 "Default trace level.")

(defvar *all-parses* nil "If t returns all parses, otherwise first one only.")

(defvar *parse-trees* nil "If nil, all structures stored in registers are flattened.")

(defvar *atn-arcs-hashtable* nil  "Hashtable for the atn arcs.")

(defvar *terminating-punctuation-flag* nil "Terminating punctuation flag.")

;; The following variables control how an input sentence is parsed into
;; a list of individual tokens by the function convertline. Each variable
;; represents a set of characters that can be supplied by either a string
;; or a list of character constants.

(defvar *punct-chars* ",:;.!?()[]{}'`/#^|"
  "The set of punctuation characters. Punctuation characters terminate words
and additionally create a token containing the punctuation character. To allow
proper handling of special commands and comments ; and ^ should always be
defined as punctuation characters.")

(defvar *white-space* '(#\space #\tab #\newline)
  "The set of characters that constitute white space. White space terminates
words and gets skipped and thrown away.")

(defvar *quote-chars* "\""
  "The set of quotation characters. Between quotation characters all special
characters such as punctuation characters or white space loose their special
meaning. Everything between quotation characters gets collected into a single
token. A quotation can contain quotation characters that were preceded
by an escape character.")

(defvar *escape-chars* "\\"
  "The set of escape characters. An escape character removes any special
meaning of the next character.")


(defvar *input-redirect-stream* nil "Stream for redirected input, used by sneps::demo.")

(defvar englex::*lexentry* nil "Special variable used to keep word sense entries, for use in CAT arcs.")

(defun peval (form)
  "Disables the Explorer-specific macro-expansion facility. All calls to 'cl::eval' in this code use it."
#+explorer (let ((si:*inhibit-displacing-flag* t))
	     (declare (special si:*inhibit-displacing-flag*))
	     (eval form))
#-explorer (eval form)
)

;;;
;;; Macros for getting portions of the ATN arc, for all types of arcs. General format is:
;;; <ARCTYPE>-<PART>, for example, "jump-state" gets the state portion of an ATN jump arc.
;;; 

(defmacro jump-state (arc)
  `(cadr ,arc))

(defmacro jump-test (arc)
  `(caddr ,arc))

(defmacro jump-actions (arc)
  `(cdddr ,arc))

(defmacro pop-form (arc)
  `(cadr ,arc))

(defmacro pop-test (arc)
  `(caddr ,arc))

(defmacro pop-actions (arc)
  `(cdddr ,arc))

(defmacro push-state (arc)
  `(cadr ,arc))

(defmacro push-test (arc)
  `(caddr ,arc))

(defmacro push-actions (arc)
  `(cdddr ,arc))

(defmacro vir-category (arc)
  `(cadr ,arc))

(defmacro vir-test (arc)
  `(caddr ,arc))

(defmacro vir-actions (arc)
  `(cdddr ,arc))

(defmacro tst-label (arc)
  `(cadr ,arc))

(defmacro tst-test (arc)
  `(caddr ,arc))

(defmacro tst-actions (arc)
  `(cdddr ,arc))

(defmacro wrd-word-list (arc)
  `(cadr ,arc))

(defmacro wrd-test (arc)
  `(caddr ,arc))

(defmacro wrd-actions (arc)
  `(cdddr ,arc))

(defmacro to-state (arc)
  `(caadr ,arc))

(defmacro to-form (arc)
  `(cadadr ,arc))

(defmacro to-test (arc)
  `(caddr ,arc))

(defmacro to-actions (arc)
  `(cdddr ,arc))

(defmacro cat-category (arc)
  `(second ,arc))

(defmacro cat-test (arc)
  `(third ,arc))

(defmacro cat-actions (arc)
  `(cdddr ,arc))

(defmacro group-arcs (arc)
  `(cdr ,arc))

(defmacro call-state (arc)
  `(second ,arc))

(defmacro call-form (arc)
  `(third ,arc))

(defmacro call-test (arc)
  `(fourth ,arc))

(defmacro call-actions (arc)
  `(cddddr ,arc))

(defmacro rcall-state (arc)
  `(second ,arc))

(defmacro rcall-form (arc)
  `(third ,arc))

(defmacro rcall-test (arc)
  `(fourth ,arc))

(defmacro rcall-actions (arc)
  `(cddddr ,arc))
;;;
;;; Configuration that defines a state of the GATN machine.
;;; 
(defstruct (configuration (:print-function print-configuration))
  state				  ; State of the ATN.
  string			  ; Current state of the input string.
  registers			  ; Current register values (an assoc list).
  hold-register			  ; List of hold pairs (category form) pairs (an assoc list).
  level				  ; Current level of the grammar.
  pop-config			  ; Back link to configuration PUSHed, CALLed, or RCALLed from.
  push-acts			  ; Post acts used when poping from a PUSH, CALL, or RCALL.
  sendr-actions			  ; Assoc list of (register value) to be set on entry to this configuration.
  liftr-actions)		  ; Assoc list of (register value) to be set on exit from this configuration.

;;;
;;; The following macros, define operators on the configuration ADT.
;;; All references to the configuration data type in this program
;;; use these. Hopefully, changes will be easy.
;;; 

(defmacro set-state (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-state ,configuration) ,value)
      `(setf (configuration-state *config*) ,value)))

(defmacro get-state (&optional (configuration nil))
  (if configuration
      `(configuration-state ,configuration)
      `(configuration-state *config*)))

(defmacro set-string (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-string ,configuration) ,value)
      `(setf (configuration-string *config*) ,value)))

(defmacro get-string (&optional (configuration nil))
  (if configuration
      `(configuration-string ,configuration)
      `(configuration-string *config*)))

(defmacro set-registers (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-registers ,configuration) ,value)
      `(setf (configuration-registers *config*) ,value)))

(defmacro get-registers (&optional (configuration nil))
  (if configuration
      `(configuration-registers ,configuration)
      `(configuration-registers *config*)))

(defmacro set-hold-register (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-hold-register ,configuration) ,value)
      `(setf (configuration-hold-register *config*) ,value)))

(defmacro get-hold-register (&optional (configuration nil))
  (if configuration
      `(configuration-hold-register ,configuration)
      `(configuration-hold-register *config*)))

(defmacro set-level (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-level ,configuration) ,value)
      `(setf (configuration-level *config*) ,value)))

(defmacro get-level (&optional (configuration nil))
  (if configuration
      `(configuration-level ,configuration)
      `(configuration-level *config*)))

(defmacro set-pop-config (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-pop-config ,configuration) ,value)
      `(setf (configuration-pop-config *config*) ,value)))

(defmacro get-pop-config (&optional (configuration nil))
  (if configuration
      `(configuration-pop-config ,configuration)
      `(configuration-pop-config *config*)))

(defmacro set-push-acts (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-push-acts ,configuration) ,value)
      `(setf (configuration-push-acts *config*) ,value)))

(defmacro get-push-acts (&optional (configuration nil))
  (if configuration
      `(configuration-push-acts ,configuration)
      `(configuration-push-acts *config*)))

(defmacro set-sendr-actions (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-sendr-actions ,configuration) ,value)
      `(setf (configuration-sendr-actions *config*) ,value)))

(defmacro get-sendr-actions (&optional (configuration nil))
  (if configuration
      `(configuration-sendr-actions ,configuration)
      `(configuration-sendr-actions *config*)))

(defmacro set-liftr-actions (value &optional (configuration nil))
  (if configuration
      `(setf (configuration-liftr-actions ,configuration) ,value)
      `(setf (configuration-liftr-actions *config*) ,value)))

(defmacro get-liftr-actions (&optional (configuration nil))
  (if configuration
      `(configuration-liftr-actions ,configuration)
      `(configuration-liftr-actions *config*)))

(defun print-configuration (config stream level &optional (tab 0))
  (format stream "~%~5TLevel: ~A~20TState: ~S~45TString: ~S"
	  (get-level config) (get-state config) (get-string config))
  (if (get-registers config)
      (format stream "~%~5TRegisters: ~20T ~{~<~%~20T ~1:; ~S~>~^,~}" (get-registers config)))
  (when (cl::>= *trace-level* 5)
    (if (get-hold-register config)
	(format stream "~%~5THolds: ~20T ~{~<~%~20T ~1:; ~S~>~^,~}" (get-hold-register config)))
    (if (get-sendr-actions config)
	(format stream "~%~5TSendrs: ~20T ~{~<~%~20T ~1:; ~S~>~^,~}" (get-sendr-actions config)))
    (if (get-liftr-actions config)
	(format stream "~%~5TLiftrs: ~20T ~{~<~%~20T ~1:; ~S~>~^,~}" (get-liftr-actions config)))
    )
  (when (cl:>= *trace-level* 9)
    (if (get-push-acts config)
	(format stream "~%~5TPush Acts:~%~S" (get-push-acts config))))
  (when (and (cl::>= *trace-level* 8) (get-pop-config config))
    (format stream "~%~5TParent configuration:")
    (print-configuration (get-pop-config config) stream level (cl::+ tab 5)))
  (format stream "~%"))
;;;
;;; End of ADT operator macros.
;;; 


(defmacro putarc (state arc)
  "Adds an arc to the hashtable entry of state (cons's it on the list)"
  `(setf (gethash ,state  *atn-arcs-hashtable*)
	 (append ,arc (gethash ,state *atn-arcs-hashtable*)))
)

(defmacro getarcs (state)
  "Gets the arcs associated with a state from the hashtable."
  `(gethash ,state *atn-arcs-hashtable*)
)

(defmacro overlap (o1 o2)
  "ATN version of EQUAL"
  `(let ((a1 (flistify (evaluate-form ',o1)))
	 (a2 (flistify (evaluate-form ',o2))))
     (intersection a1 a2 :test 'packageless-equal)))

(defmacro disjoint (o1 o2)
  "ATN version of not EQUAL"
  `(null (overlap ,o1 ,o2)))

(defun endofsentence ()
  "Returns t if at the end of a sentence or last puntation."
  (declare (special *config*))
  (or
    (null (configuration-string *config*))
    (and
      (member (car (configuration-string *config*)) '("." "!" "?") :test 'packageless-equal)
      (null (cdr (configuration-string *config*))))))

(defmacro buildq (&rest buildargs)
  "Handles the ATN 'buildq' directive."
  `(let ((frags (cdr ',buildargs)))
    (declare (special frags) (special *config*))
    (bldq (car ',buildargs))))

(defun tcopy (l)
  "Copies the list without getting uninterned symbols."
  (cond ((atom l) l)
	(t (copy-list l))  ; was: (cons (car l) (tcopy (cdr l)))
	))

(defun bldq (exp)
  "Does the instantiation of registers and special atoms required by BUILD."
  (declare (special frags) (special *config*))
  (cond ((packageless-equal exp '+)
	   (setq exp (evaluate-form (car frags)) frags (cdr frags))
	   exp)
	((packageless-equal exp '*)
	   (internal-getr '*))
	((and (listp exp)
	      (packageless-equal (car exp) '@))
	   (mapcan #'tcopy (bldq (cdr exp))))
	((atom exp)
	   exp)
	(t (cons (bldq (car exp)) (bldq (cdr exp))))
	))

(defun coerce-into-package-symbols (list &optional (pkg *package*))
  "Translate all symbols in LIST to their equivalents in PACKAGE."
  (mapcar #'(lambda (item)
	      (typecase item
		(keyword item)
		(symbol (if (eq (symbol-package item) pkg)
			    item
			    (intern (string item) pkg)))
		(t item)))
	  list))

(defmacro talk (level formatstr &rest args)
  "Generates tracing output. All tracing output should go through it."
  `(cond
    ((>= *trace-level* ,level) (parser-print ,formatstr ,@args))
  ))

(defun parser-print (formatstr &rest args)
  "ATN output routine. May re-direct output."
  (format t "~?" formatstr args))

(defun flat* (l)
  "Flattens a list so that all atoms are at the top level."
  (cond ((null l) nil)
	((atom l) (list l))
	((sneps::is.n l) (list l))
	(t (nconc (flat* (car l))
		  (flat* (cdr l))))
	))

(defun flatten (s)
  "Flattens its argument list. I. e., makes all components top-level, and returns the list 
  if it contains at least two elements, its 'car' otherwise."
  (cond ((cdr (setq s (flat* s))) s)
	(t (car s))
	))


(defun pconcat (&rest catl)
  "Evaluates, copies, and listifies a list of s-expressions."
  (mapcan #'(lambda (x) (flistify (tcopy x)))
	  catl))

(defun packageless-equal (x y)
  "Compares two types of objects, which may be strings, symbols, nodes, lists, etc. Probably not
the best code but PRINC-TO-STRING is computationaly expensive, so its the base case, rather
than the only case."
      (if (and (symbolp x) (symbolp y))
	  (string-equal (symbol-name x) (symbol-name y))
	  (if (or
		(or (listp x) (listp y))
		(and (stringp x) (stringp y)))
	      (equal x y)
	      (if (stringp x)
		  (string-equal x (symbol-name y))
		  (if (stringp y)
		      (string-equal y (symbol-name x))
		      (equal (princ-to-string x) (princ-to-string y)))))))

(defmacro getf (feature &optional (word-form nil have-word-form))
   "Returns the given feature of the word-form, which defaults to the current one."
   (if have-word-form
	`(englex::lookup-lexical-feature ',feature (evaluate-form ',word-form))
	`(if englex::*lexentry*
	     (if (englex::get-lexical-feature ',feature englex::*lexentry*)
		 (englex::get-lexical-feature ',feature englex::*lexentry*)
		 (englex::lookup-lexical-feature ',feature (car (get-string))))
	     (englex::lookup-lexical-feature ',feature (car (get-string))))))

(defun internal-getf (feature &optional (word-form nil have-word-form))
   "Returns the given feature of the word-form, which defaults to the current one."
   (declare (special englex::*lexentry*))
   (if have-word-form
	(englex::lookup-lexical-feature feature (evaluate-form word-form))
	(if englex::*lexentry*
	     (if (englex::get-lexical-feature feature englex::*lexentry*)
		 (englex::get-lexical-feature feature englex::*lexentry*)
		 (englex::lookup-lexical-feature feature (internal-getr '*)))
	     (englex::lookup-lexical-feature feature (internal-getr '*)))
	))

(defmacro geta (arc &optional (node '*))
  "Get the nodes at the head of ARC from NODE (default: *)."
  `(internal-geta ',arc ',node))

(defun internal-geta (rel nde)
  "Get the nodes at the head of the REL arc from either a given node, 
or the node in *."
  (let ((nde (flatten (flistify (evaluate-form nde)))))
    (cond ((sneps:is.n nde)
           (sneps:nodeset.n nde rel))
	  ((sneps::is.ns nde)
           (let ((result (sneps:new.ns)))
             (sneps:do.ns (node nde result)
               (setq result
                 (sneps:union.ns (sneps:nodeset.n node rel) result)))))
	  ((sneps:node nde)
           (sneps:nodeset.n (sneps:node nde) rel)))))

;;;
;;; The following macros define all the ATN actions and forms.
;;; 

(defun setup-forms (forms)
  (if (or (atom forms) (typep forms 'sneps::node))
      forms
      (if (cdr forms)
	  (if *parse-trees*
	      forms
	      (flatten forms))
	  (if *parse-trees*
	      (car forms)
	      (flatten (car forms))))))

(defmacro addr (register &rest forms)
  `(let* ((regs (get-registers))
	  (forms ',(setup-forms (mapcar #'evaluate-form forms)))
	  (found nil)
	  (newregs nil))
     (dolist (r regs)
       (when (packageless-equal ',register (car r))
	 (setq r (if (null (cdr r))
		     (cons (car r) forms)
		     (cons (car r) (append (flistify (cdr r)) (flistify forms))))
	       found t))
       (cl::push r newregs))
     (if (not found)
	 (cl::push (cons ',register forms) newregs))
     (set-registers newregs)))

(defmacro addl (register &rest forms)
  `(let* ((regs (get-registers))
	  (forms ',(setup-forms (mapcar #'evaluate-form forms)))
	  (found nil)
	  (newregs nil))
     (dolist (r regs)
       (when (packageless-equal ',register (car r))
	 (setq r (if (null (cdr r))
		     (cons (car r) forms)
		     (cons (car r) (append (flistify forms) (flistify (cdr r)))))
	       found t))
       (cl::push r newregs))
     (if (not found)
	 (cl::push (cons ',register forms) newregs))
     (set-registers newregs)))

(defmacro setr (register &rest forms)
  `(set-registers 
     (cons (cons ',register ',(setup-forms (mapcar #'evaluate-form forms)))
	   (remove-if #'(lambda (x) (equal ',register (car x)))
		      (get-registers)))))

(defun internal-setr (register &rest forms)
  (progn
     (locally (declare (special *config*)))
     (set-registers
       (cons
	 (cons register (setup-forms forms))
	 (remove-if #'(lambda (x) (equal register (car x))) (get-registers))))))

(defmacro sendr (register &rest forms)
  (if forms
      `(set-sendr-actions
	 (cons (cons ',register ',(setup-forms (mapcar #'evaluate-form forms)))
	       (remove-if #'(lambda (x) (equal ',register (car x)))
			  (get-sendr-actions))))
      `(set-sendr-actions 
	 (cons (cons ',register ',(internal-getr register)) 
	       (remove-if #'(lambda (x) (equal ',register (car x)))
			  (get-sendr-actions))))))

(defun internal-sendr (register &rest forms)
  (if forms
      (set-sendr-actions
	(cons (cons register (setup-forms (mapcar #'evaluate-form forms)))
	      (remove-if #'(lambda (x) (equal register (car x)))
			 (get-sendr-actions))))
      (set-sendr-actions
	(cons (cons register (internal-getr register)) 
	      (remove-if #'(lambda (x) (equal register (car x)))
			 (get-sendr-actions))))))

(defmacro liftr (register &rest form)
  (if form
      `(set-liftr-actions
	 (cons (cons ',register ',(setup-forms (mapcar #'evaluate-form form)))
	       (remove-if #'(lambda (x) (equal ',register (car x)))
			  (get-liftr-actions))))
      `(set-liftr-actions
	 (cons (cons ',register ',(internal-getr register))
	       (remove-if #'(lambda (x) (equal ',register (car x)))
			  (get-liftr-actions))))))

(defmacro getr (register &optional level)
  (if level
      `(if (cl::>= ,level (get-level))
	   (talk 0 "~%~%Error, getr was called with a level less than or equal to the current level.~%")
	   (let ((tconfig *config*))
	     (dotimes (n (cl::- ,level (get-level)))
	       (setq tconfig (get-pop-config tconfig)))
	     (cdr (assoc ',register (get-registers tconfig) :test #'packageless-equal))))
      `(cdr (assoc ',register (get-registers) :test #'packageless-equal))))

(defun internal-getr (register)
  (cdr (assoc register (get-registers) :test #'packageless-equal)))

(defmacro nullr (register)
  `(not (cdr (assoc ',register (get-registers)))))

(defmacro hold (category form)
  `(set-hold-register
     (cons (list ,category
		 ',(flistify (setup-forms (evaluate-form form)))
		 (get-level))
	   (get-hold-register))))

(defun any-holds-at-this-level? ()
  "Returns t iff any HOLDs at this level."
  (declare (special *config*))
  (let ((holdr (get-hold-register))
	(level (get-level)))
    (if (null holdr)
	nil
	(some #'(lambda (x) (equal (caddr x) level)) holdr))))

(defmacro to (state &optional (form nil))
  (if form
      `(let ((temp ',(evaluate-form form)))
	 (declare (special *config*))
	 (if temp
	     (set-string
	       (cons temp (cdr (get-string))))
	     (set-string (cdr (get-string))))
	 ',state)
      `(progn
	 (set-string
	   (cdr (get-string)))
	 ',state)))

(defmacro jump (state)
  `',state)

(defun get-senses (ctgy string)
  "Given a category name and an input string,
        it returns a list of triples consisting of the length of the word (in words),
           its lexical entry (sense),
           and full word (which may be a multi-word lexeme),
        for all senses of the same category.
    It is required on CAT arcs where all possible senses of an input prefix must be considered."
  (do* ((senses nil)
	(lex-entries (or
		      (englex:lookup (car string))
		      (englex::geth (car string))) (cdr lex-entries))
	(lex-entry (car lex-entries) (car lex-entries))
	(wrd-ctgy (cdr (assoc 'englex:ctgy lex-entry
			      :test #'packageless-equal))
		  (cdr (assoc 'englex:ctgy lex-entry
			      :test #'packageless-equal)))
	)
      ((null lex-entries) senses)
    (if (packageless-equal ctgy wrd-ctgy)
	(cl:push (list 1 lex-entry (car string)) senses)
      (if (packageless-equal wrd-ctgy 'englex:multi-start)
	  (do* ((len 1 (1+ len))
		(full-word (car string)
			   (concatenate 'string full-word " " (car str)))
		(str (cdr string) (cdr str))
		(multi-rest (cdr (assoc 'englex:multi-rest lex-entry
					:test #'packageless-equal))
			    (cdr multi-rest)))
	      ((null multi-rest)
	       (mapcar
		#'(lambda (x)
		    (if (packageless-equal
			 (cdr (assoc 'englex:ctgy x
				     :test #'packageless-equal)) ctgy)
			(cl:push (list len x full-word) senses)))
		(englex::geth full-word))
	       )
	    (if (or (not (packageless-equal (car str) (car multi-rest)))
		    (null str))
		(return nil)))))))

(defun evaluate-actions (acts)
  "Evaluates a list of ATN actions."
  (declare (special *config*))
  (mapcar #'peval acts))

(defun evaluate-form (s-exp)
  "Evaluates an ATN form."
  (declare (special *config*))
  (cond
    ((numberp s-exp) s-exp)
    ((stringp s-exp) s-exp)
    ((internal-getr s-exp) (internal-getr s-exp))
    ((equal s-exp '*) (internal-getr '*))
    ((listp s-exp) (peval s-exp))
    ((sneps::is.n s-exp) s-exp)
    ((boundp s-exp) (symbol-value s-exp))
    (t nil)))

(defmacro evaluate-form-in-context (config form)
  "Evaluates an ATN form in the context of a specified configuration."
  `(let ((*config* ,config))
     (declare (special *config*))
     (evaluate-form ,form)))

(defmacro evaluate-form-in-context* (config form)
  "Evaluates an ATN form in the context of a specified configuration."
  `(let ((*config* ,config)
	 (registers (get-registers ,config))
	 res)
     (declare (special *config*))
     (internal-setr '* (car (get-string)))
     (setq res (evaluate-form ,form))
     (set-registers registers)
     res))

(defvar *atn-readtable*
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\# #'sneps:hash-reader)
    (set-macro-character #\? #'sneps:question-reader)
    (set-macro-character #\! #'sneps:bang-reader)
    (set-macro-character #\= #'sneps:equal-reader)
    *readtable*)
  "Readtable used to read in ATNs")

(defsnepscom atnin ((filename) (top) t)
  "Reads in the ATN grammar from a file."
  (let ((statlist  nil)
	(eof	   (gensym))
	(*illarcs* nil)
	(*undefs*  nil)
	(*arctype* '(cat wrd tst vir push pop jump call rcall transr to group))
	(*package* (find-package 'snepsul))
	(*use-reader-macro-characters* t))
    (declare (optimize speed)
	     (special *illarcs* *undefs* *arctype* *atn-arcs-hashtable*
		       *use-reader-macro-characters*))
        (if (and (boundp '*atn-arcs-hashtable*)
	     (hash-table-p *atn-arcs-hashtable*))
	(clrhash *atn-arcs-hashtable*)
	(setq *atn-arcs-hashtable* (make-hash-table)))
    (with-open-file (inunit (cl-user:sneps-translate filename) :direction :input)
      (let ((*readtable* *atn-readtable*))
	(do ((obj (read inunit nil eof) (read inunit nil eof)))
	    ((eq obj eof))
	  (cl:push (definenet obj) statlist))))
    (maphash #'check-arc-syntax *atn-arcs-hashtable*)
    (parser-print "~%~% Atnin read in states: ~A ~%~%" statlist)
    (values)))

(defun definenet (arcsets)
  "Defines the ATN network."
  (declare (optimize speed))
  (declare (special *illarcs* *undefs* *arctype*))
  (when (listp arcsets)
    (let ((arcstate (car arcsets))
	  (arcset (cdr arcsets)))
      (cond ((eq arcstate 'snepsul::^^)
	       (peval arcset)
	       nil)
	    (t (putarc arcstate arcset)
	       (dolist (arc arcset)
		 (if (not (member (car arc) *arctype*))
		     (cl::push (list arcstate (car arc)) *illarcs*))
		 (findstats arcstate arc))
	       (format t "~&State ~S processed.~%" arcstate)
	       arcstate)
	    ))))

(defun findstats (arcstate arc)
  "Checks for undefined to/push targets (?)"
  (declare (optimize speed))
  (declare (special *undefs*))
  (cond
    ((packageless-equal (car arc) 'group)
       (dolist (each-arc (cdr arc))
         (findstats arcstate each-arc)))
    (t (dolist (each-arc arc)
         (when (and (consp each-arc)
		    (consp (car each-arc))
		    (overlap (caar each-arc) '(to push)))
	   (cl::pushnew
	     (list arcstate (cadar each-arc))
	     *undefs*
	     :test-not #'(lambda (a1 a2)
			   (eq (cadr a1) (cadr a2)))))
	 ))
    ))

(defun nl-tell (sentence)
  "Given a string sentence. Invokes the parser and returns a string
   result. Useful for agents that need to listen, and produce a response."
  (let ((*config* (make-configuration))
	sneps:outunit
	(sneps:inunit *standard-input*)
	*state* converted-sentence)
    (if (not (sneps:initialized-p englex:*lexicon*))
	(talk 0 "~%~% Warning: No lexicon is loaded. You should load a ~
                      lexicon via ~%  (lexin \"<lexicon-filename>\") ~
                      before running the ATN parser.~%"))
    (if (not (sneps:initialized-p parser:*atn-arcs-hashtable*))
	(error "No ATN grammar is loaded. You must load a grammar via ~
              ~%  (atnin \"<atn-grammar-filename>\") ~
                  before running the ATN parser.~%"))
    
    (setq *state* 'S *trace-level* 0)
    
    (if (ifsneps)
	(setq *parse-trees* nil)
      (setq *all-parses* nil))

    (setf converted-sentence (convertline sentence))
    
   (with-output-to-string (stream)
      (setf sneps:outunit stream)
    
      (if (and (upper-case-p (elt (first converted-sentence) 0))
	       (null (englex::lexic (first converted-sentence)))
	       (englex::lookup
		(string-downcase (first converted-sentence) :end 1)))
	  (setq converted-sentence
	    (cons (string-downcase (first converted-sentence) :end 1)
		  (rest converted-sentence))))
      (internal-parse converted-sentence))))



(defun parse (&rest args)
  "Top level ATN parser. Initializes and invokes the parser suitably. Takes
trace level and start state as arguments."
  (let ((*config* (make-configuration))
	sentence *state* result btime)

    (if (not (sneps:initialized-p englex:*lexicon*))
	(talk 0 "~%~% Warning: No lexicon is loaded. You should load a ~
                      lexicon via ~%  (lexin \"<lexicon-filename>\") ~
                      before running the ATN parser.~%"))
    (if (not (sneps:initialized-p parser:*atn-arcs-hashtable*))
	(error "No ATN grammar is loaded. You must load a grammar via ~
              ~%  (atnin \"<atn-grammar-filename>\") ~
                  before running the ATN parser.~%"))

    (setq *state* 'S *trace-level* 0)
    (parser-print "~% ATN parser initialization... ~%")
    
    ;; Get arguments (if any).
    (dolist (arg args)
      (typecase arg
	(number (setq *trace-level* arg))
	(atom (setq *state* arg))))
    
    (talk 0 "~% Trace level = ~D.~%" *trace-level*)
    (talk 0 "~% Beginning at state '~S'.~%" *state*)
    
    (if (ifsneps)
	(setq *parse-trees* nil)
	(setq *all-parses* nil))
    
    (parser-print
     "~%~% Input sentences in normal English orthographic convention. ~
        ~% Sentences may go beyond a line by having a space followed by a <CR>~
        ~% To exit the parser, write ^end.~%")

    #+(or kcl akcl)
    ;; AKCL and KCL need an additional read-line because there is a newline
    ;; in the input buffer that does not get cleared out. However, this
    ;; solution has not been tested and might need some adaptation.
    (read-line)

    ;; Start parsing loop:
    (sneps:with-demo-control (sneps:inunit
			      ((atn-read-sentence nil)
			       (sneps:pseudolisp-read nil))
			      )
      (loop
       (parser-print "~%~% : ")
       (setq sentence (atn-read-sentence))

       (cond ((null sentence)
	      ;; This case can only occur during a demo which does not
	      ;; exit the parser
	      :do-nothing)
	     (;; Handle special commands (this assumes that ^ is a punct-char):
	      (string-equal (first sentence) "^")
	      (cond ((string-equal (second sentence) "lisp")
		     (parser-print "~% Please exit the parser via ^end.~%"))
		    ((string-equal (second sentence) "end")
		     (parser-print "~%~% ATN Parser exits... ~%~%")
		     (return (values)))
		    ((string-equal (second sentence) "^")
		     (parser-print "~% Enter Lisp Read/Eval/Print loop. Type ^^ to continue~%")
		     (sneps:pseudolisp))
		    (t (parser-print "~% Illegal command.~%"))))
	     ;; Otherwise try to parse the sentence:
	     (t (talk 4 "~% Input sentence: ~S ~%" sentence)
		(if (and (upper-case-p (elt (first sentence) 0))
			 (null (englex::lexic (first sentence)))
			 (englex::lookup
			  (string-downcase (first sentence) :end 1)))
		    (setq sentence
			  (cons (string-downcase (first sentence) :end 1)
				(rest sentence))))
		(setq btime (runtime))
		(setq result (internal-parse sentence))
		(talk -1 "~% Time (sec.): ~F"
		      (/ (float (cl::- (runtime) btime))
			 internal-time-units-per-second))
		;; printing is now done in final pops inside `mainloop':
		;;(if (and result (>= *trace-level* 0))
		;;    (print-parse result))
		result))))
    ))

(defun print-parse (parse)
  ;; Prints a single resulting PARSE (a node or list).
  (format t "~%Resulting parse:")
  (cond ((sneps:node-p parse)
	 (format t "~%")
	 #!((describe ~parse)))
	(t (pprint (if *parse-trees*
		       parse
		     (flatten parse))
		   *standard-output*)
	   (format t "~%"))))

(defun internal-parse (string &optional (state *state*))
   "Internal top-level parse call. Creates initial configuration and
calls MAINLOOP. Returns a single parse tree if `*all-parses*' is nil,
and a list of parse trees otherwise."
   (let* ((cfg (make-configuration
 	       :state state
 	       :string string
 	       :registers nil
 	       :hold-register nil
 	       :pop-config (make-configuration
 			    :state "End of Parse."
 			    :string nil
 			    :registers nil
 			    :hold-register nil
 			    :pop-config nil
 			    :level 0)
 	       :level 1))
 	 *config*)
     (declare (special *config*))
     (setq *config* (catch 'found-parse (mainloop cfg)))
     (cond (*all-parses*
 	   ;; with `*all-parses*' set we always get a list of
 	   ;; configurations (this list might be of length 1):
 	   (mapcar #'(lambda (config)
 		       (when (setq *config* config)
 			 (if *parse-trees*
 			     (internal-getr '*)
 			   (flatten (internal-getr '*)))))
 		   *config*))
	   ;; otherwise we get a single result configuration:
	   (t (when *config*
 	       (if *parse-trees*
 		   (internal-getr '*)
 		 (flatten (internal-getr '*))))))))

(defun commented-line-p (tokenized-line)
  "Non-NIL if the first token of TOKENIZED-LINE is a comment character"
  (equal (first tokenized-line) ";"))

(defun atn-read-sentence ()
  "Reads a sentence from the current input stream and returns it in tokenized
form (see function convertline). A sentence is terminated by either an end
of file condition on the input stream, or by a line that does not have a space
as its last character if *terminating-punctuation-flag* is NIL, or if it is
not NIL by a line that ends in one of the termination tokens defined by it.
Lines starting with ; are assumed to be commented out and will be skipped."
  (declare (special sneps:inunit))
  (let ((inunit (or (sneps:initialized-p sneps:inunit)
		    *standard-input*))
	(term-tokens (if (sneps:initialized-p *terminating-punctuation-flag*)
		       *terminating-punctuation-flag*
		       nil))
	current-line tokenized-current-line tokenized-line)
    (loop
     (setq current-line (read-line inunit nil :eof)
	   tokenized-current-line nil)
     (when (stringp current-line) 
       (setq tokenized-current-line (convertline current-line))
       ;; skip commented lines
       (unless (commented-line-p tokenized-current-line)
	 (setq tokenized-line (append tokenized-line tokenized-current-line))))
     ;; Check whether we have reached the end of a sentence, and
     ;; if so return the tokenized line:
     (when (and
	    ;; skip commented lines
	    (not (commented-line-p tokenized-current-line))
	    (or ;; End of file?
	       (eq current-line :eof)
	       ;; No terminating punctuation tokens defined and the
	       ;; last character of the line is not a space
	       (and (null term-tokens)
		    (not (eql (1- (length current-line))
			      (position #\space current-line :from-end t))))
	       ;; Terminating punctuation tokens defined and the
	       ;; last token of the line is one
	       (and term-tokens
		    (member (car (last tokenized-line))
			    term-tokens
			    :test #'string-equal))))
       (return-from atn-read-sentence tokenized-line)))))


;; New version that does not use type-coercion and is a bit more
;; straight forward than the original one. There were problems 
;; in Lucid-CL in some special cases. (hc, Feb-27-90)
;; Now also handles quotation characters. (hc, Jun-10-90)
(defun convertline (line &key (punct-chars *punct-chars*)
		              (white-space *white-space*)
			      (quote-chars *quote-chars*)
			      (escape-chars *escape-chars*)
			      (item-type 'string))
  "Converts LINE (a string) into a list of strings (or atoms enclosed in |'s
if ITEM-TYPE is atom), where each item corresponds to a word or a punctuation
character. Words in LINE are delimited by WHITE-SPACE or PUNCT-CHARS.
Within QUOTE-CHARS all characters are treated like normal characters.
ESCAPE-CHARS escape the special meaning of the next character."
    (mapcar
     (case item-type
       (string #'symbol-name)
       (t #'identity))
     (read-from-string
      (with-output-to-string (s)
	(let ((state :white-space)
	      (escape nil))
	  (format s "(")
	  (map nil #'(lambda (char &aux echar)
		       ;; Deal with special case that char = #\|
		       (cond ((char-equal char #\|) (setq echar "\\|"))
			     (t (setq echar char)))
		       (cond (escape
			      (format s "~a" echar)
			      (setq escape nil))
			     ((cl:find char escape-chars :test #'char-equal)
			      (setq escape t))
			     ((cl:find char quote-chars :test #'char-equal)
			      (case state
				((:white-space :punctuation)
				 (format s " |~a| |" echar)
				 (setq state :quotation))
				(:word (format s "| |~a| |" echar)
				       (setq state :quotation))
				(:quotation (format s "| |~a|" echar)
					    (setq state :white-space)))
			      )
			     ((cl:find char punct-chars :test #'char-equal)
			      (case state
				((:white-space :punctuation)
				 (format s " |~a|" echar)
				 (setq state :punctuation))
				(:word (format s "| |~a|" echar)
				       (setq state :punctuation))
				(:quotation (format s "~a" echar))))
			     ((cl:find char white-space :test #'char-equal)
			      (case state
				(:whitespace :do-nothing)
				(:word (format s "| ")
				       (setq state :white-space))
				(:punctuation (format s " ")
					      (setq state :white-space))
				(:quotation (format s "~a" echar))))
			     (t (case state
				  ((:white-space :punctuation)
				   (format s " |~a" echar)
				   (setq state :word))
				  (:word (format s "~a" echar))
				  (:quotation (format s "~a" echar))))))
				
	       (format nil "~a " line))
	  (format s ")"))))))

(defun list-to-type (type list)
  (case type
    (string (coerce list type))
    (list list)
    (atom (read-from-string (coerce (append (cons #\| list) '(#\|)) 'string)))))

(defun runtime ()
  "Returns the runtime."
  (get-internal-run-time))

;;;
;;; Dummy definitions
;;;


(defun ifsneps ()
  (let ((f (find-package 'sneps)))
    (and f
	 (fboundp (find-symbol "SNEPS" f))
	 )))

;;;
;;; Main loop call.
;;; 

(defun mainloop (entry-config &optional (grp-arcs nil in-group-arcs?))
  "Mainloop takes a configuration and returns all possible final configurations from the entry configuration."
  (declare (special *config*))
  (let* ((arcs (getarcs (get-state entry-config)))
	 (sendrs (get-sendr-actions entry-config))
	 (any-input-left? (flatten (get-string entry-config)))
	 (all-configs nil))

    (if in-group-arcs?
	(setq arcs grp-arcs)
	(progn

	  (talk 5 "~%Entering MAINLOOP with a configuration: ~S~%~%" entry-config)

	  (setq entry-config (let ((*config* entry-config))
			       (internal-setr 'lex (car (get-string)))
			       (eval-sendr sendrs)
			       *config*))

	  (set-sendr-actions nil entry-config)))

    (dolist (arc arcs)
      
      (talk 6  "~%Doing arc: ~%~%~A~%" arc)
      
      (case (car arc)
	
	(group
	 (let ((*config* (copy-configuration entry-config)))
	   (eval-sendr sendrs)
	   (set-sendr-actions nil)

	   (talk 4 "~%GROUPing with arcs:~%~5TArcs: ~20T ~{~<~%~20T ~1:; ~S~>~^,~}~%With Configuration: ~S~%"
		 (group-arcs arc)
		 *config*)
	   (setq all-configs (append all-configs (mainloop *config* (group-arcs arc)))))
	 )


	(jump				   ; JUMP atn action
	 (if (evaluate-form-in-context* entry-config (jump-test arc))
	     (let* ((*config* (copy-configuration entry-config)))
	       (internal-setr '* (car (get-string)))
	       (evaluate-actions (jump-actions arc))
	       (set-state (jump-state arc))
	       (talk 4 "~%JUMPing to state: ~A~%With Configuration: ~S~%"
		     (jump-state arc)
		     *config*)
	       (setq all-configs (append all-configs (mainloop *config*)))
	       (if in-group-arcs?
		   (return)))
	     (talk 6  "~%Blocked at arc: ~A~%~%" arc))
	 )

	(to
	 (if (and any-input-left?
		  (evaluate-form-in-context* entry-config (to-test arc)))
	     (let* ((*config* (copy-configuration entry-config)))
	       (internal-setr '* (car (get-string)))
	       (evaluate-actions (to-actions arc))
	       (set-string  (cdr (get-string))) 
	       (if (to-form arc)
		   (set-string (append (flistify (evaluate-form (to-form arc))) (get-string))))
	       (set-state (to-state arc))
	       (talk 4 "~%TOing to state: ~A~%With Configuration: ~S~%"
		     (to-state arc)
		     *config*)
	       (setq all-configs (append all-configs (mainloop *config*)))
	       (if in-group-arcs?
		   (return)))
	     (talk 6  "~%Blocked at arc: ~A~%~%" arc))
	 )

	(tst
	 (if (and any-input-left?
		  (evaluate-form-in-context* entry-config (tst-test arc)))
	       (let* ((*config* (copy-configuration entry-config)))
		 (internal-setr '* (car (get-string)))
		 (set-state (car (last (evaluate-actions (tst-actions arc)))))
		 (talk 4"~%Taking TST labeled ~A to state: ~A~%With Configuration: ~S~%"
		       (tst-label arc)
		       (get-state)
		       *config*)
		 (setq all-configs (append all-configs (mainloop *config*)))
		 (if in-group-arcs?
		   (return)))
	       (talk 6  "~%Blocked at arc: ~A~%~%" arc))
	 )

	(pop
	 (if (evaluate-form-in-context entry-config (pop-test arc))
	     (let* ((*config* (copy-configuration entry-config))
		    (string (get-string))
		    liftrs
		    presult)
	       (setq liftrs (get-liftr-actions))
	       (if (and
		    (eql (get-level) 1)
		    (null (get-string))
		    (null (get-hold-register)))
		   (progn
		     (evaluate-actions (pop-actions arc))
		     (setq presult (evaluate-form (pop-form arc)))
		     (talk 4 "~%POPing in state: ~A, a FINAL result: ~S~
                              ~%With Configuration: ~S~%"
			   (get-state) presult *config*)
		     (setq *config* (copy-configuration (get-pop-config)))
		     (internal-setr '* presult)
		     (set-string string)
		     (set-liftr-actions liftrs)
		     (setq all-configs (append all-configs (list *config*)))
		     (if (and presult (>= *trace-level* 0))
			 (print-parse presult))
		     (if *all-parses*
			 (if (not (y-or-n-p "~%Try next parse? "))
			     (throw 'found-parse all-configs))
		       (throw 'found-parse *config*)))
		 (if (and
		      (not (any-holds-at-this-level?))
		      (not (eql (get-level) 1)))
		     (progn
		       (evaluate-actions (pop-actions arc))
		       (setq presult (evaluate-form (pop-form arc)))
		       (talk 4 "~%POPing in state: ~A, a result: ~S~
                                ~%With Configuration: ~S~%"
			     (get-state) presult *config*)
		       (setq all-configs
			 (append all-configs
				 (mainloop
				  (do-acts presult *config*
					   (get-pop-config)
					   (get-push-acts))))))
		   (talk 6  "~%Blocked at arc: ~A~%~%" arc)))
	       (when in-group-arcs?
		 (return)))
	   (talk 6  "~%Blocked at arc: ~A~%~%" arc))
	 )	 
	(push
	 (if (and any-input-left?
		  (evaluate-form-in-context* entry-config (push-test arc)))
	     (let* ((*config* (copy-configuration entry-config))
		    acts)
	       (internal-setr '* (car (get-string)))
	       (setq acts (do* ((preacts (push-actions arc) (cdr preacts))
				(preact (car preacts) (car preacts)))
			       ((not (packageless-equal (car preact) 'sendr))
				(cons '(push *) preacts))
			    (evaluate-form preact)))
	       (set-state (push-state arc))
	       (set-level (1+ (get-level)))
	       (set-pop-config entry-config)
	       (set-push-acts acts)
	       (set-registers nil)
	       (set-liftr-actions nil)
	       (talk 4 "~%PUSHing to state: ~A,~%With Configuration: ~S~%"
		     (push-state arc) *config*)
	       (setq all-configs (append all-configs (mainloop *config*)))
	       (if in-group-arcs?
		   (return)))
	     (talk 6  "~%Blocked at arc: ~A~%~%" arc)))

	(call
	 (if (and any-input-left?
		  (evaluate-form-in-context* entry-config (call-test arc)))
	     (let* ((*config* (copy-configuration entry-config))
		    acts)
		   (internal-setr '* (car (get-string)))
		   (setq acts (do* ((preacts (call-actions arc) (cdr preacts))
				    (preact (car preacts) (car preacts)))
				   ((atom preact)
				    (cons (list 'call preact (internal-getr '*)) (cdr preacts)))
				   (if (packageless-equal (car preact) 'sendr)
				       (evaluate-form preact)
				       (let ((*config* entry-config))
					 (evaluate-form preact)))
				   ))
		   (set-string 
		     (append (flistify (evaluate-form (call-form arc)))
			     (cdr (get-string))))
		   (set-state (call-state arc))
		   (set-level (1+ (get-level)))
		   (set-pop-config entry-config)
		   (set-push-acts acts)
		   (set-liftr-actions nil)
		   (set-registers nil)
		   (talk 4 "~%CALLing to state: ~A,~%With Configuration: ~S~%"
			 (call-state arc) *config*)
		   (setq all-configs (append all-configs (mainloop *config*)))
		   (if in-group-arcs?
		       (return)))
	     (talk 6  "~%Blocked at arc: ~A~%~%" arc)))
	(rcall
	 (if (and any-input-left?
		  (evaluate-form-in-context* entry-config (rcall-test arc)))
	     (let* ((*config* (copy-configuration entry-config))
		    acts)
		   (internal-setr '* (car (get-string)))
		   (setq acts (do* ((preacts (rcall-actions arc) (cdr preacts))
				    (preact (car preacts) (car preacts)))
				   ((atom preact)
				    (cons (list 'rcall preact (internal-getr '*)) (cdr preacts)))
				   (if (packageless-equal (car preact) 'sendr)
				       (evaluate-form preact)
				       (let ((*config* entry-config))
					 (evaluate-form preact)))
				   ))
		   (set-string (flistify (evaluate-form (rcall-form arc))))
		   (set-state (rcall-state arc))
		   (set-level (1+ (get-level)))
		   (set-pop-config entry-config)
		   (set-push-acts acts)
		   (set-registers nil)
		   (set-liftr-actions nil)
		   (talk 4 "~%RCALLing to state: ~A,~%With Configuration: ~S~%"
			 (rcall-state arc) *config*)
		   (setq all-configs (append all-configs (mainloop *config*)))
		   (if in-group-arcs?
		       (return)))
	     (talk 6  "~%Blocked at arc: ~A~%~%" arc)))
	(vir
	 (if (and
	       (evaluate-form-in-context entry-config (vir-test arc))
	       (assoc (vir-category arc) (get-hold-register entry-config) :test #'packageless-equal))
	     (let* ((*config* (copy-configuration entry-config)))
	       (dolist (hold-item (remove-if-not #'(lambda (x) (packageless-equal (car x) (vir-category arc)))
						 (get-hold-register)))
		 (set-string (cons (cadr hold-item) (get-string)))
		 (set-hold-register (remove hold-item (get-hold-register) :test #'packageless-equal))
		 (internal-setr '* (cadr hold-item))
		 (set-state (car (last (evaluate-actions (vir-actions arc)))))
		 (talk 4 "~%Taking VIR arc on ~A, to state ~A.~%With Configuration: ~S~%"
		       (cadr arc) (get-state) *config*)
		 (setq all-configs (append all-configs (mainloop *config*))))
	       (if in-group-arcs?
		   (return)))
	     (talk 6  "~%Blocked at arc: ~A~%~%" arc))
	 )

	(cat
	 (if any-input-left?
	     (let* ((wrd-senses (get-senses (cat-category arc) (get-string entry-config))))
	       (if wrd-senses
		   (progn
		     (dolist (trans wrd-senses)
		       (let ((englex::*lexentry* (cadr trans))
			     (len (car trans))
			     (fword (caddr trans))
			     (*config* (copy-configuration entry-config)))
			 (declare (special englex::*lexentry*))
			 (internal-setr '*
					(or (internal-getf 'root)
					    fword))
			 ;(if (cl::> len 1)
			     (set-string (cons (internal-getr '*) (nthcdr len (get-string))));)
			 (if (not (evaluate-form (cat-test arc)))
			     (talk 6  "~%Blocked at arc: ~A~%~%" arc)
			     (progn
			       (set-state (car (last (evaluate-actions (cat-actions arc)))))
			       (setq englex::*lexentry* nil)
			       (talk 4 "~%Taking CAT arc on ~A, to state ~A.~%With Configuration: ~S~%"
				     (cadr arc) (get-state) *config*)
			       (setq all-configs (append all-configs (mainloop *config*)))))
			 (setq englex::*lexentry* nil)))
		     (if in-group-arcs?
			 (return)))
		   (talk 6  "~%Blocked at arc: ~A~%~%" arc)))
	     (talk 6  "~%Blocked at arc: ~A~%~%" arc)))

	(wrd
	 (if (and any-input-left?
		  (evaluate-form-in-context* entry-config (wrd-test arc)))
	     (if (member (car (get-string entry-config))
			      (flistify (wrd-word-list arc))
			      :test #'packageless-equal)
		 (let ((*config* (copy-configuration entry-config)))
		   (internal-setr '* (car (get-string)))
		   (set-state (car (last (evaluate-actions (wrd-actions arc)))))
		   (talk 4 "~%Taking WRD to state: ~A~%With Configuration: ~S~%"
			 (get-state) *config*)
		   (setq all-configs (append all-configs (mainloop *config*)))
		   (if in-group-arcs?
		       (return)))
		 (talk 6  "~%Blocked at arc: ~A~%~%" arc))
	     (talk 6  "~%Blocked at arc: ~A~%~%" arc))
	 )

	))
    all-configs))

(defun do-acts (pop-result *config* push-config acts)
  "Executes the acts associated with PUSH/CALL/RCALL when they are POPed back."
  (declare (special *config*))
  (let* ((push-type (first (first acts)))
	 (register (second (first acts)))
	 (old-star-reg (third (first acts))))
    (set-registers (get-registers push-config))
    (eval-liftr (get-liftr-actions))
    (set-level (get-level push-config))
    (set-pop-config (get-pop-config push-config))
    (set-sendr-actions (get-sendr-actions push-config))
    (set-liftr-actions (get-liftr-actions push-config))
    (set-push-acts (get-push-acts push-config))
    (internal-setr register pop-result)
    (setq acts (cdr acts))
    (case push-type
      (push
       (set-string (cons pop-result (get-string))))
      (call
       (if (packageless-equal register '*)
	   (set-string (cons pop-result (get-string)))
	   (set-string (cons (first (get-string push-config)) (get-string)))))
      (rcall
       (if (packageless-equal register '*)
	   (set-string (cons pop-result (get-string push-config)))
	   (set-string (get-string push-config)))))
    (if (and
	  (member push-type '(call rcall))
	  (not (equal register '*)))
	(internal-setr '* old-star-reg))
    (set-state (car (last (evaluate-actions acts))))
    *config*))

(defun eval-liftr (liftrs)
  "Sets registers to values that have been LIFTRed."
  (mapcar #'(lambda (l)
	      (apply #'internal-setr
		     (cons (car l)
			   (cond ((listp (cdr l))
				  ;; either a list or nil
				  (cdr l))
				 (t (list (cdr l)))))))
	  liftrs))

(defun eval-sendr (sendrs)
  "Sets registers to values that have been SENDRed."
  (mapcar #'(lambda (l)
	      (apply #'internal-setr
		     (cons (car l)
			   (cond ((listp (cdr l))
				  ;; either a list or nil
				  (cdr l))
				 (t (list (cdr l)))))))
	  sendrs))

(defun get-parser-state (state)
  "Makes a serious attempt to establish the actual parser state, that is, which package the specified state is in.
   This is non-trivial, because symbols may be interned in a variety of packages. This routine takes a state,
   presumed to be a ATN state, and returns the actual symbol used in the grammar."
  (let ((any-arcs? (getarcs state)))
    (if any-arcs?
	state
	(or (first
	      (remove-if #'(lambda (x) (or (null x) (null (getarcs x))))
			 (mapcar #'(lambda (pkg) (find-symbol (symbol-name state) pkg))
				 (list-all-packages))))
	    state))))

(defun wrap-break-around-test (test break-message)
  "Takes a TEST form and wraps arc-break around it such that the break
will be performed before the test has been evaluated. The result of the
test will be returned by the wrapped test."
  (let ((test-form (if (and (atom test)
			    (not (eq test t)))
		       `(getr ,test)
		       test))
	(break-form `(arc-break ,@break-message)))
    `(progn ,break-form ,test-form)))

(defun is-wrapped (test)
  "Checkes whether TEST form has a break wrapped around it"
  (and (consp test)
       (eq (first test) 'progn)
       (consp (second test))
       (eq (first (second test)) 'arc-break)))

(defun unwrap-test (wrapped-test)
  "Unwraps WRAPPED-TEST if it is wrapped with arc-break. NoOp otherwise."
  (if (is-wrapped wrapped-test)
      (third wrapped-test)
      wrapped-test))
  
(defun break-arc (st arcno &rest break-message)
  "Wraps a call to `arc-break' around the condition part of the ATN arc specified."
  (let* ((state (get-parser-state st))
	 (arcs (getarcs state))
	 (arc (car (nthcdr (1- arcno) arcs))))
    (if (null arcs)
	(talk 0 "~%State: ~S, has no arcs emanating.~%" state)
	(if (null arc)
	    (talk 0 "~%State: ~S, has no ~:R arc.~%" state arcno)
	    (progn
	      (setf (gethash state *atn-arcs-hashtable*)
		    (replace-item
		      arcno
		      arcs
		      (case (car arc)
			((cat jump pop push to tst vir wrd)
			 (replace-item 3 arc
				       (wrap-break-around-test
					 (third arc) break-message)))
			((call rcall)
			 (replace-item 4 arc
				       (wrap-break-around-test
					 (fourth arc) break-message))))))
	      (format t "~%~%Break installed on state: ~S, in arc:~%~S~%~%" state arc))))
    (values)))

(defun unbreak-arc (st arcno)
  "Removes the call to `arc-break' around the condition part of the ATN arc specified."
  (let* ((state (get-parser-state st))
	 (arcs (getarcs state))
	 (arc (car (nthcdr (1- arcno) arcs))))
    (remhash state *atn-arcs-hashtable*)
    (if (null arcs)
	(talk 0 "~%State: ~S, has no arcs emanating.~%" state)
	(if (null arc)
	    (talk 0 "~%State: ~S, has no ~:R arc.~%" state arcno)
	    (progn
	      (putarc
		state
		(replace-item
		  arcno
		  arcs
		  (case (car arc)
		    ((cat jump pop push to tst vir wrd)
		     (replace-item 3 arc (unwrap-test (third arc))))
		    ((call rcall)
		     (replace-item 4 arc (unwrap-test (fourth arc)))))))
	      (format t "~%~%Break removed on state: ~S, in arc:~%~S~%~%" state arc))))
    (values)))

(defun current-configuration ()
  (format t "~%~% Current configuration: ~%~S~%~%" *config*)
  (values))

(defmacro arc-break (&rest message)
  `(progn
     (parser-print ,@(or message '("")))
     (sneps:pseudolisp "break-arc> ")
     t))

(defun replace-item (pos list item)
  (setq list (copy-list list))
  (unless (zerop pos)
    (setf (nth (1- pos) list) item))
  list)

(defun state-ok? (state arc-type)
  (if state
      (if (getarcs state)
	  t
	  (talk 0 "~%State: ~S, which is the target of a ~S arc, has no outgoing arcs.~%It will always block."
		state arc-type))))

(defun terminal-action-ok? (arc)
  (let ((term-act (first (last arc))))
    (if (member (car term-act) '(jump to) :test #'packageless-equal)
	(if (state-ok? (second term-act) (car term-act))
	    t))))

(defun preactions-before-actions-ok? (acts)
  (let ((act-found nil))
    (dolist (act acts t)
      (if (packageless-equal (first act) 'sendr)
	  (if act-found
	      (return nil))
	  (setq act-found t)))))

(defun check-arc-syntax (state arcs)
  (dolist (arc arcs)
    (case (car arc)
      (cat
       (if (atom (cat-category arc))
	   (if (cat-test arc)
	       (if (terminal-action-ok? arc)
		   arc
		   (talk 0 "~%Bad terminal action in state ~S on arc:~%~S~%" state arc))
	       (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%Non-atomic category specified in state ~S on arc:~%~S~%" state arc)))
      (call
       (if (state-ok? (call-state arc) (first arc))
	   (if (call-form arc)
	       (if (call-test arc)
		   (if (do ((acts (call-actions arc) (cdr acts)))
			   ((null acts) nil)
			 (if (atom (first acts))
			     (return t)))
		       (if (terminal-action-ok? arc)
			   arc
			   (talk 0 "~%Bad terminal action in state ~S on arc:~%~S~%" state arc))
		       (talk 0 "~%Missing register in state ~S on arc:~%~S~%" state arc))
		   (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	       (talk 0 "~%Missing (or NIL) form in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%Badly specified state in state ~S on arc:~%~S~%" state arc)))
      (group
       (check-arc-syntax state (rest arc)))
      (jump
       (if (state-ok? (jump-state arc) (first arc))
	   (if (jump-test arc)
	       arc
	       (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%Badly specified state in state ~S on arc:~%~S~%" state arc)))
      (pop
       (if (pop-form arc)
	   (if (pop-test arc)
	       arc
	       (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%Missing (or NIL) form in state ~S on arc:~%~S~%" state arc)))
      (push
       (if (state-ok? (push-state arc) (first arc))
	   (if (push-test arc)
	       (if (preactions-before-actions-ok? (push-actions arc))
		   (if (terminal-action-ok? arc)
		       arc
		       (talk 0 "~%Bad terminal action in state ~S on arc:~%~S~%" state arc))
		   (talk 0 "~%Bad ordering of preactions and actions in state ~S on arc:~%~S~%" state arc))
	       (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%Badly specified state in state ~S on arc:~%~S~%" state arc)))
      (to
       (if (consp (second arc))
	 (if (state-ok? (to-state arc) (first arc))
	     (if (to-test arc)
		 arc
		 (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	     (talk 0 "~%Badly specified state in state ~S on arc:~%~S~%" state arc))
	 (talk 0 "~%Badly formed TO target (must be a list) in state ~S on arc:~%~S~%" state arc)))
      (rcall
       (if (state-ok? (rcall-state arc) (first arc))
	   (if (rcall-form arc)
	       (if (rcall-test arc)
		   (if (do ((acts (rcall-actions arc) (cdr acts)))
			   ((null acts) nil)
			 (if (atom (first acts))
			     (return t)))
		       (if (terminal-action-ok? arc)
			   arc
			   (talk 0 "~%Bad terminal action in state ~S on arc:~%~S~%" state arc))
		       (talk 0 "~%Missing register in state ~S on arc:~%~S~%" state arc))
		   (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	       (talk 0 "~%Missing (or NIL) form in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%Badly specified state in state ~S on arc:~%~S~%" state arc)))
      (tst
       (if (atom (tst-label arc))
	   (if (tst-test arc)
	       (if (terminal-action-ok? arc)
		   arc
		   (talk 0 "~%Bad terminal action in state ~S on arc:~%~S~%" state arc))
	       (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%TST label must be atomic in state ~S on arc:~%~S~%" state arc)))
      (vir
       (if (atom (vir-category arc))
	   (if (vir-test arc)
	       (if (terminal-action-ok? arc)
		   arc
		   (talk 0 "~%Bad terminal action in state ~S on arc:~%~S~%" state arc))
	       (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%Non-atomic category specified in state ~S on arc:~%~S~%" state arc)))
      (wrd
       (if (wrd-word-list arc)
	   (if (wrd-test arc)
	       (if (terminal-action-ok? arc)
		   arc
		   (talk 0 "~%Bad terminal action in state ~S on arc:~%~S~%" state arc))
	       (talk 0 "~%Missing (or NIL) test in state ~S on arc:~%~S~%" state arc))
	   (talk 0 "~%Missing word (or word list) in state ~S on arc:~%~S~%" state arc)))
      (t
       (talk 0 "~%Undefined ATN arc in state ~S on arc:~%~S~%" state arc))
      )))



    
    




