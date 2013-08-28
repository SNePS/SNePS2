;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: system-utils.lisp,v 1.2 2013/08/28 19:07:21 shapiro Exp $

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




;; altered 7-1-2002 for ACL6  (FLJ)  


(in-package :cl-user)


;; Logical pathname support functions:

;; The following functions are used by SNePS to translate namestrings
;; of logical pathnames into their physical counterparts:

;;    sneps-is-logical-pathname
;;    sneps-parse-and-translate-namestring

;; The following functions are used throughout SNePS to handle or
;; translate namestrings of logical pathnames before they are passed
;; on to actual functions such as `load', etc.:

;;    sneps-source-pathname
;;    sneps-binary-pathname
;;    sneps-translate
;;    sneps-load
;;    sneps-probe-file

;; The rationale behind this setup is this:

;; + Native implementations of logical pathnames often have quirks, and
;;   it is very common to encounter non-standard behavior.
;; + The LPMK implementation will probably fail under some operating
;;   system conditions.
;; + Only 3 functions that take pathnames as arguments are guaranteed to
;;   work with namestrings of logical pathnames.

;; If a problem related to logical pathnames occurs, it should be possible
;; to fix it by changing this small set of functions.  If all else fails,
;; the translation into physical pathnames can be done "by hand" by directly
;; manipulating the namestrings of logical pathnames.

(defun sneps-is-logical-pathname (thing)
  ;; Returns non-NIL if THING is a logical pathname or the namestring of one.
  (if (stringp thing)
      ;; There is no standard CLtL-II way to test whether a string
      ;; is the namestring of a logical pathname.  Hence this kludge
      ;; (simply testing for a #\: won't work on Macs):
      (member (subseq thing 0 (or (position #\: thing) 0))
              *sneps-logical-hosts*
              :test #'string-equal)
    (typep thing
           #+lpmk 'lp::logical-pathname
           #-lpmk 'logical-pathname)))

#+abcl
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  ;; From the Common Lisp Cookbook: 
  ;; http://cl-cookbook.sourceforge.net/strings.html
  ;; License: http://cl-cookbook.sourceforge.net/license.html
  (with-output-to-string (out)
    (loop with part-length = (length part)
	for old-pos = 0 then (+ pos part-length)
	for pos = (search part string
			  :start2 old-pos
			  :test test)
	do (write-string string out
			 :start old-pos
			 :end (or pos (length string)))
	when pos do (write-string replacement out)
	while pos))) 

(defun sneps-parse-and-translate-namestring (namestring)
  ;; Parses and translates NAMESTRING into a physical pathname.
  (let* ((pathname
	  ;; this will return an object of type `cl:pathname',
	  ;; `cl:logical-pathname' or `lp::logical-pathname', depending on
	  ;; whether the LPMK implementation of `logical-pathname' was used:
          (if (sneps-is-logical-pathname namestring)
              (lp::logical-pathname namestring)
            (parse-namestring namestring)))
	 (translated-pathname
	  ;; because the LPMK implementation of `translate-logical-pathname'
	  ;; only accepts logical pathnames we have to test what kind we have:
	  (if (sneps-is-logical-pathname pathname)
	      (lp::translate-logical-pathname pathname)
	    pathname))
         (translated-namestring
          ;; the LPMK implementation of `translate-logical-pathname' returns a
          ;; string rather than a pathname, thus we can use that right away:
          (if (stringp translated-pathname)
              translated-pathname
            (namestring translated-pathname))))
    #+clisp
    ;; Fix a CLISP (4/4/95) bug: It leaves a final "." in the
    ;; translation of pathnames without an extension:
    (if (eql (char translated-namestring
                   (1- (length translated-namestring)))
             #\.)
        (setq translated-namestring
          (subseq translated-namestring
                  0 (1- (length translated-namestring)))))
    #+abcl
    ;;; VERY temporary fix of an ABCL logical pathnames bug
    ;;; Bug info here: http://trac.common-lisp.net/armedbear/ticket/133
    (setq translated-namestring (replace-all
				 translated-namestring
				 (string-downcase *sneps-directory*)
				 *sneps-directory*))

    ;; Finally, make a physical pathname out of it:
    (parse-namestring translated-namestring)))

;; Create pathnames with forced extensions (I used to do this with
;; `(make-pathname :type "lisp" :defaults pathname)' but the Explorers
;; insisted on upcasing it to "LISP" - sigh).

(defun sneps-source-pathname (namestring)
  ;; Generates a source pathname from NAMESTRING.
  ;; If NAMESTRING does already have an extension it will simply be
  ;; translated into a physical pathname. If not, an appropriate LISP
  ;; extension will be appended to NAMESTRING and the resulting NAMESTRING
  ;; will get translated into a physical pathname.
  (let ((translated-pathname
	 (sneps-parse-and-translate-namestring namestring)))
    (cond ((pathname-type translated-pathname)
	   translated-pathname)
	  ((sneps-is-logical-pathname namestring)
	   ;; LISP is a canonical source file extension for logical pathnames.
	   ;; Since the translations of the particular logical host might map
	   ;; LISP extensions on something else we have to retranslate the
	   ;; logical pathname after .LISP got appended:
	   (sneps-parse-and-translate-namestring
	    (concatenate 'string namestring ".LISP")))
	  ;; Here we have a physical pathname without an extension, simply
	  ;; make a new pathname that uses `*sneps-default-lisp-extension*':
	  (t (make-pathname
	      :host (pathname-host translated-pathname)
	      :device (pathname-device translated-pathname)
	      :directory (pathname-directory translated-pathname)
	      :name (pathname-name translated-pathname)
	      :type *sneps-default-lisp-extension*)))))

(defun sneps-binary-pathname (namestring)
  ;; Converts NAMESTRING into a physical pathname with a binary extension.
  ;; This is a poor man's version of CLtL-II's `compile-file-pathname'.
  ;; Given any NAMESTRING it should produce the pathname generated by
  ;; `(compile-file (sneps-source-pathname NAMESTRING))'.
  (let ((translated-pathname
	 (sneps-parse-and-translate-namestring namestring)))
    (make-pathname
     :host (pathname-host translated-pathname)
     :device (pathname-device translated-pathname)
     :directory (pathname-directory translated-pathname)
     :name (pathname-name translated-pathname)
     :type *sneps-binary-extension*)))

;; The main exported interface:
(defun sneps-translate (pathname-or-namestring)
  (if (pathnamep pathname-or-namestring)
      pathname-or-namestring
    (sneps-parse-and-translate-namestring pathname-or-namestring)))

;; Define `sneps-load' such that it understands logical pathnames:
(defun sneps-load (pathname-or-namestring)
  (sneps-load0 (sneps-translate pathname-or-namestring)))

(defun sneps-probe-file (pathname-or-namestring)
  (probe-file (sneps-translate pathname-or-namestring)))


;;; Below is a set of utilities that allow one to build systems
;;; on various operating systems from a "simple" system definition.
;;; The portability is achieved with the use of logical pathnames.
;;; The system definition is "simple" because it cannot deal with
;;; dependencies, it simply is a list of entries that describe an 
;;; operation and a pathname. When a system is built these operations
;;; are performed in sequence. The entries have the following form:
;;;
;;;        (<operation> <logical/physical pathname-string>)
;;;
;;;  e.g., (:COMPILE "sneps:ginseng;desc.LISP")
;;;
;;; where <operation> is either :COMPILE, :COMPILE-LOAD, :LOAD or
;;;       :SNEPSLOG-ATNIN.
;;;
;;; The motivation for using this mechanism instead of one of the portable
;;; `defsystem' implementations is that the dependencies for SNePS are so
;;; complex (over the years it has lost its modularity) that it is easier
;;; to use a working compile/load sequence than to figure out the dependencies
;;; among the 200 or so files and construct a system definition from that.
;;; Believe me, we tried.


(defstruct (DESCRIPTION (:type list))
  operation
  file)

(defmacro GET-SIMPLE-SYSTEM-INFO (system-name)
  "Retrieves information associated with SYSTEM-NAME"
  `(get (intern (string ,system-name) 'cl-user) 'simple-system-info)) 

(defmacro SET-SIMPLE-SYSTEM-INFO (system-name info)
  "Stores INFOrmation associated with SYSTEM-NAME"
  `(setf (get-simple-system-info ,system-name) ,info))

(defmacro RECORD-SIMPLE-SYSTEM-ACTION (system-name action)
  "Records that a certain ACTION (a keyword) has been performed on
the system with SYSTEM-NAME."
  `(set-simple-system-info
    ,system-name
    (adjoin ,action (get-simple-system-info ,system-name))))

(defmacro FORGET-SIMPLE-SYSTEM-ACTION (system-name action)
  "Forgets that a certain ACTION (a keyword) has been performed on
the system with SYSTEM-NAME."
  `(set-simple-system-info
    ,system-name
    (remove ,action (get-simple-system-info ,system-name))))

(defmacro CHECK-SIMPLE-SYSTEM-ACTION (system-name action)
  "Checks whether a certain ACTION (a keyword) has been performed on
the system with SYSTEM-NAME."
  `(member ,action (get-simple-system-info ,system-name)))

(defun SIMPLE-SYSTEM-CREATED-P (system-name)
  "Returns non-NIL if SYSTEM-NAME has been created with a call to
make-simple-system."
  (or (check-simple-system-action system-name :load)
      (check-simple-system-action system-name :compile)
      (check-simple-system-action system-name :recompile)
      (check-simple-system-action system-name :load-uncompiled)))

(defvar *sneps-null-stream*
    (make-array 1024
                :element-type (if (fboundp 'string-char-p)
                                  'string-char
                                'character)
                :fill-pointer t))
(defun MAKE-SIMPLE-SYSTEM (system-name system-definition
			   &key (mode :load) (verbose *sneps-verbose*))
  "Makes the system SYSTEM-NAME from a simple SYSTEM-DEFINITION (a list of
descriptions). If MODE is :LOAD then :compile  descriptions will be ignored
and :compile-load descriptions will just load the (compiled) file. If MODE
is :RECOMPILE all :compile and :compile-load descriptions will execute an
unconditional compilation. If MODE is :COMPILE only :compile and :compile-load
descriptions with source files newer than the current binary file will
execute a compilation. If MODE is :LOAD-UNCOMPILED all :load and :compile-load
descriptions will load source files without any compilations taking place.
A :snepslog-atnin description will load the atn with the snepslog:atnin
function regardless of the value of MODE. 
   If VERBOSE is NIL all loading, compiling or warning messages will be
suppressed, i.e., sent to a null stream, and only the percentage of files
already loaded will be displayed in 10% steps.
   Once the system has been loaded successfully the SYSTEM-NAME will be
associated with the action specified in MODE."

  ;; First convert system-definition to description format if necessary
  (when (and (consp system-definition)
	     (consp (first system-definition)))
    (setq system-definition
	  (mapcar #'(lambda (entry)
		      (make-description
		       :operation (first entry)
		       :file (second entry)))
		  system-definition)))
  (let* ((*sneps-verbose* verbose)
	 ;; Percentage of already processed file descriptions (used in
	 ;; non-VERBOSE mode). Increases in 10% steps.
	 (percentage-processed 0)
	 ;; Temporary variable to to store the actual percentage of
	 ;; processed descriptions.
	 current-percentage
	 ;; Number of descriptions processed so far.
	 (descriptions-processed 0)
	 type-supplied-p pathname source-pathname binary-pathname)
    
    (format t "~&Loading system ~a..." system-name)
    (dolist (description system-definition)
      ;; If non-VERBOSE mode calculate percentage of descriptions
      ;; processed so far and print update if necessary.
      (unless verbose
	(incf descriptions-processed)
	;; This percentage changes every 10%
	(setq current-percentage
	      (* 10 (floor (* 10 descriptions-processed)
			   (length system-definition)))) 
	(unless (= percentage-processed current-percentage)
	  (setq percentage-processed current-percentage)
	  (format t "~d% " percentage-processed)
	  (force-output)
	  ))

      (setq pathname
	(sneps-parse-and-translate-namestring (description-file description)))
      (setq type-supplied-p (pathname-type pathname))
      (setq source-pathname
	(sneps-source-pathname (description-file description)))
      (setq binary-pathname
	(sneps-binary-pathname (description-file description)))

      ;; Reset null-stream buffer for non-VERBOSE mode:
      (setf (fill-pointer *sneps-null-stream*) 0)
      (with-output-to-string (null-stream *sneps-null-stream*)
	(let* ((*load-verbose* verbose)
	       (*terminal-io* (cond (verbose *terminal-io*)
				    (t null-stream)))
	       (*standard-output* *terminal-io*))

	  ;; First, check whether we have to compile
	  (case (description-operation description)
	    ((:compile :compile-load)
	     (when (and (not (eq mode :load-uncompiled))
			(or (eq mode :recompile)
			    (and (eq mode :compile)
				 (> (or (file-write-date source-pathname) 1)
				    (or (and (probe-file binary-pathname)
					     (file-write-date binary-pathname))
					0)))))
	       (format t "~&;; Compiling ~a" source-pathname)
	       (compile-file source-pathname))))

	  ;; Then, check whether and how we have to load (because we allow
	  ;; customization of binary file extensions we cannot rely on the
	  ;; standard source/binary mechanism provided by most `load's):
	  (case (description-operation description)
	    ((:load :compile-load)
	     (cond ((or (eq mode :load-uncompiled)
			(and type-supplied-p (eq mode :load)))
		    (sneps-load source-pathname))
		   ((probe-file binary-pathname)
		    (sneps-load binary-pathname))
		   (t (sneps-load source-pathname))))
	    (:snepslog-atnin
	     (format t "~&;; Loading ATN ~a" source-pathname)
	     ;; Don't have SNEPSLOG package when this function gets loaded:
	     (funcall (intern (symbol-name :atnin) :snepslog) source-pathname))
	    ))))

    ;; Finally, record what we did for this particular system
    (record-simple-system-action system-name mode)
    ))

(defvar *sneps-garnet-status* nil)

(defun sneps-check-garnet ()
  ;; Returns `:usable' if GARNET can be used, NIL otherwise.
  ;; If this function is too picky and returns NIL even though GARNET
  ;; can be used at your machine, you can always explicitly set
  ;; `*sneps-garnet-status*' to `:usable' to disable any checks.
  (cond (;; Did we already check before?
         (eq *sneps-garnet-status* :usable) :usable)
        ((eq *sneps-garnet-status* :unusable) nil)
        ;; GARNET already loaded?
        ((find-package 'opal)
         (setq *sneps-garnet-status* :usable))
        ;; Is GARNET installed at all?
        ((not (sneps-probe-file "garnet:garnet-loader.LISP"))
         (warn "The GARNET load file `~a' does not exist!"
               (namestring (sneps-translate "garnet:garnet-loader.LISP")))
         (setq *sneps-garnet-status* :unusable)
         nil)
        ;; Is it compiled for our Lisp?
        ((not (probe-file (sneps-binary-pathname "garnet:bin;kr;kr")))
         (warn "GARNET does not seem to be compiled for this Lisp; ~
                won't load it!")
         (setq *sneps-garnet-status* :unusable)
         nil)
        ;; We have a usable GARNET installation, check the DISPLAY:
        ((or #+unix
             (or #+cmu (cdr (assoc :DISPLAY cl::*environment-list*))
                 #+(or allegro lispworks
                       kcl ibcl akcl gcl clisp) (sys::getenv "DISPLAY")
                 #+(and lucid lcl3.0) (lucid-common-lisp:environment-variable
                                       "DISPLAY")
                 #+(and lucid (not lcl3.0)) (system:environment-variable
                                             "DISPLAY")
                 nil)
             #+apple t
             #-(or unix apple)
             (y-or-n-p "Are you running X-Windows with the DISPLAY set? ")
             nil)
         (setq *sneps-garnet-status* :usable))
        (t (warn "You don't have the DISPLAY variable set; won't load GARNET!")
           (setq *sneps-garnet-status* :unusable)
           nil)))

(defun LOAD-GARNET (&rest modules)
  "Loads all MODULES of the Garnet system. Returns T if Garnet is installed
and the loading completed successfully. The elements of MODULES can be
keywords, for example, :multifont, to specify that that module should be
loaded, or a string which is taken to be a particular Garnet file to be
loaded. First all modules and then all files will be loaded. A typical call
would be (load-garnet :kr :opal :inter :aggregadgets)."
  (when (sneps-check-garnet)
    (let ((load-kr-p nil)
	  (load-opal-p nil)
	  (load-inter-p nil)
	  (load-multifont-p nil)
	  (load-gesture-p nil)
	  (load-ps-p nil)
	  (load-aggregadgets-p nil)
	  (load-aggregraphs-p nil)
	  (load-debug-p nil)
	  (load-gadgets-p nil)
	  (load-demos-p nil)
	  (load-lapidary-p nil)
	  (load-gilt-p nil)
	  ;; launch-process-p
	  ;; Lucid-4.0 bombs during Garnet load if *print-pretty* is t
	  (*print-pretty* nil)
	  load-variable)
      (declare (special load-kr-p load-opal-p load-inter-p load-multifont-p
			load-gesture-p load-ps-p load-aggregadgets-p
			load-aggregraphs-p load-debug-p load-gadgets-p
			load-demos-p load-lapidary-p load-gilt-p
			launch-process-p))
      #+(and lucid sparc)
        (setq launch-process-p nil)
      (dolist (module modules)
	(when (and (symbolp module)
		   (eq (symbol-package module)
		       (find-package 'keyword)))
	  (setq load-variable (intern 
			       (concatenate 'string
				 (symbol-name :load-) 
				 (symbol-name module)
				 (symbol-name :-p))  ))
	  (when (and (boundp load-variable)
		     (null (eval load-variable)))
	    (set load-variable t))
	  ))
      #+(and allegro-version>= (version>= 6 0))
      (progn 
	(setf *pre-garnet-load-case-mode* excl:*current-case-mode*)
	(setf *pre-garnet-load-print-case-mode* *print-case*)
	(case *current-case-mode* 
	  (:case-sensitive-lower
	   (setf excl::*ignore-package-name-case* t)
	   (excl::convert-mixed-case-symbols t) ; nil to preserve mixed-case
	   (excl:set-case-mode :case-insensitive-lower)
	   (setf *print-case* :downcase))
	  (:case-insensitive-lower
	   (setf *print-case* :downcase)
	   (setf excl::*ignore-package-name-case* t)
	   (excl::convert-mixed-case-symbols t))
	  (:case-insensitive-upper
	   (setf *print-case* :upcase)
	   (setf excl::*ignore-package-name-case* t)
	   (excl::convert-mixed-case-symbols t))
	 ))
      (sneps-load "garnet:garnet-loader.LISP")      
      (dolist (module modules)
	(when (stringp module)
	  (sneps-load module)))
      #+(and allegro-version>= (version>= 6 0))
      (progn
	(setf *print-case* *pre-garnet-load-print-case-mode*)
	(cond ((equal *pre-garnet-load-case-mode* :case-sensitive-lower)
	       (excl:set-case-mode :case-sensitive-lower)
	       (excl::convert-mixed-case-symbols nil))))
      t)))

#-explorer
;; A poor man's version of kill-package (just rename it):
(defun KILL-PACKAGE (package)
  (let ((package (find-package package)))
    (when package
      (unuse-package (package-use-list package) package)
      (rename-package package (gentemp)))))

(defun KILL-SNEPS ()
  "Kill all SNePS related packages and global variables. This allows
proper reloading of SNePS without having to restart or reboot the
lisp environment."
  (declare (special *sneps-packages*))
  (in-package :cl-user)
  (let (;; Have to save these before they get unbound
	(sneps-packages (and (boundp '*sneps-packages*)
			       *sneps-packages*)))
    (setf (symbol-plist 'kill-sneps) nil)
    (dolist (fn '(sneps snepslog))
      (when (fboundp fn)
	(fmakunbound fn))
      (setf (symbol-plist fn) nil))
    (do-symbols (sym (find-package 'cl-user))
      (when (search "*SNEPS-" (symbol-name sym) :test #'char-equal)
	;;(format t "~&Kill symbol: ~a" sym)
	(and (boundp sym)
	     (not (constantp sym))
	     (makunbound sym))
	(setf (symbol-plist sym) nil)))
    (dolist (p sneps-packages)
      (if (find-package p)
	  (kill-package p)))))

;;;; Clisp's version of `probe-file' had a problem with non-existing
;;;; intermediate directories, but that seems to be fixed now:
;;#+(and clisp unix)
;;(defun clisp-new-probe-file (file)
;;  (let ((*error-handler* 
;;	 #'(lambda (&rest ignore)
;;	     (declare (ignore ignore))
;;	     (return-from clisp-new-probe-file nil))))
;;    (funcall #'clisp-old-probe-file file)))

;;#+(and clisp unix)
;;(unless (fboundp 'old-clisp-probe-file)
;;  (setf (symbol-function 'clisp-old-probe-file)
;;    (symbol-function 'probe-file))
;;  (setf (symbol-function 'probe-file)
;;    (symbol-function 'clisp-new-probe-file)))



    
    




