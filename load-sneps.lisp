;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

;; Copyright (C) 1984--2013
;; Research Foundation of State University of New York

;; Version: $Id: load-sneps.lisp,v 1.3 2013/08/28 19:07:21 shapiro Exp $

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





(in-package #.(if (find-package :cl-user) :cl-user :cl-user))

;; For ANSI Lisps that don't provide LISP and USER packages
;; add them as nicknames to the built-in COMMON-LISP packages:
#+sbcl(sb-ext:unlock-package "COMMON-LISP")
#+sbcl(sb-ext:unlock-package "COMMON-LISP-USER")

(if (and (or (not (find-package :cl))
             (not (find-package :lisp)))
         (find-package :common-lisp))
    (eval `(defpackage :common-lisp
             (:nicknames ,@(package-nicknames :common-lisp) :cl :lisp))))

(if (and (or (not (find-package :cl-user))
             (not (find-package :user)))
         (find-package :common-lisp-user))
    (eval `(defpackage :common-lisp-user
             (:nicknames ,@(package-nicknames :common-lisp-user) :cl-user :user))))


;;; Required files
;;; These need to be present to build functioning executables

#+allegro
(progn
  (require :srecord)
  (require :loop)
  (require :streama)
  (require :jlinker)
  (require :process)
  (require :sock)
  (require :streamc)
  (require :excl)
  (require :regexp2))

;; Key to Features:
;;
;;   unix      --      UNIX operating system
;;   lm-unix   --      Lisp-Machine using a Unix file server
;;   explorer  --      Texas Instruments Explorer
;;   symbolics --      Symbolics Lisp Machine
;;   vms       --      VAX/VMS operating system
;;   allegro   --      Allegro Common-Lisp (Franz Inc.)
;;   lucid     --      Lucid (or Sun) Common-Lisp
;;   cmu       --      CMU Common-Lisp
;;   clisp     --      CLISP by Bruno Haible and Michael Stoll
;;   multimax  --      Kyoto Common Lisp running on an Encore Multimax
;;   mcl       --      Mac Common-Lisp
;;   apple     --      Apple/Mac operating system
;;   lispworks --      Harlequin LispWorks
;;   lpmk      --      Use portable implementation of logical pathnames
;;                     by Mark Kantrowitz
;;   abcl      --      Armed Bear Common Lisp

;; Installation instructions:

;; - Edit `*sneps-config-file*' (below) to reflect the location of the sneps 
;;   configuration file (sneps_config.config). The config file has instructions
;;   on expected modifications.

;; - (Optional - Allegro user only) 
;;   Edit the JavaSnepsAPI/java_sneps_config.config file
;;   if use of the API is desired. Also edit the Jlinker/jl-config.cl file
;;   in the sneps home directory. This wfile will ask you to configure
;;   the location of your java home directory. For the JavaAPI Java 1.5 or
;;   above  is required.

;; - If you did rename all Lisp files to conform to the default extension
;;   of your Common-Lisp then edit `*sneps-lisp-extension*'  in the config file;;    accordingly
;;   (renaming Lisp files is not mandatory even if your Common-Lisp does not
;;   use `.lisp' as its default extension).
;; - Invoke your Common-Lisp and load this file.
;; - For first time installation, set *sneps-noquery* to 'nil' in the config 
;;   file 
;; - Select option `e' (the installation option) from the initial menu.
;; - Ignore all the warnings you'll get during compilation.
;; - If the compilation completed successfully then you are done with the
;;   installation, exit your Common-Lisp
;; - Optionally set `*sneps-noquery*' (config file) to T and 
;;   `*sneps-verbose*' to NIL
;; - To load SNePS simply load this file and select option `a' from the
;;   initial menu (if you set `*sneps-noquery*' to T you won't be asked).
;; - Run `(sneps)' or `(snepslog)' once SNePS is loaded.
;; - If you get errors because files can't be found during compilation or
;;   loading check whether the various `*sneps-xxx-extension*' variables
;;   have proper values for your Common-Lisp


;;; altered for  ACL 6 compatibility (FLJ)  

#+(and allegro-version>= (version>= 6 0))
(progn 
  (setf *pre-sneps-load-case-mode* excl:*current-case-mode*)
  (setf *pre-sneps-load-print-case-mode* *print-case*)
  (case  *current-case-mode*
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


;;;;;;;;;;;;;;;;;;;; User customization section ;;;;;;;;;;;;;;;;;;;;

;;; Open the sneps.config file and edit it for your system
;;; Change the variable below to point to that file.
(defvar *config-file* "/projects/snwiz/Install/Sneps-2.8.0/snerg_config.lisp"
"The location of the SNePS configuration file.")

;;;;;;;;;;;;;;;;;;;; End user custimization ;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *sneps-lisp-extension* "lisp"
  "File extension used for Lisp files of the SNePS distribution.
The default corresponds to the extension that comes with the standard
SNePS distribution. Ignore this variable unless you decided to rename all
Lisp files to conform to the default extension used by your Common-Lisp.")


(defvar *sneps-default-lisp-extension*
    (prog1
        #+unix (prog1
                   #+(or ibcl kcl akcl) "lsp"
                   #+clisp "lisp"  ;;; "lsp" -> "lisp"  FLJ 9/2/02
                   #+abcl "lisp"
		   #+gcl "lisp"  ; was "lsp"
                   "lisp")
        #+(or explorer symbolics) "LISP"
        #+vms "LSP"
        "lisp")
  "The default Lisp file extension used by your Common-Lisp.
The defaults provided should probably be ok. Ignore this variable unless
you run into problems with the loading of general Lisp files.")

(defvar *sneps-binary-extension*
    (prog1
        #+unix (prog1
                   #+(or ibcl kcl akcl gcl) "o"
                   #+clisp "fas"
                   #+abcl "abcl"
                   #+allegro "fasl"
                   #+cmu "sparcf"
                   #+lucid "sbin"
                   #+lispworks "afasl"
                   "bin")
        #+explorer "XLD"
        #+symbolics "BIN"
        #+vms "FAS"
        #+apple "fasl"
        "bin")
  "The default compiled file extension used by your Common-Lisp.
Because these files are generated by your compiler the defaults provided
will probably be ok. Ignore this variable unless you run into problems
with the loading of compiled Lisp files.")



;;; The following is only needed for the executable, which is allegro lisp

#+allegro
(defun set-and-load-config ()
  (when (member "-config-file" (sys:command-line-arguments) :test #'string=)
    (sys:with-command-line-arguments 
     (("config-file" :long config-file :required-companion)
      ("api-port" :long api-port :required-companion)
      ("api-classpath" :long api-classpath :required-companion))
     (rest :usage "This is a test" :long-abbrev t)
     (when config-file
       (setf *config-file* config-file))))
 
  (load *config-file*))

#+allegro
(set-and-load-config)

#-allegro
(load *config-file*)


(export '(*sneps-directory* *jung-directory* *xerces-directory* 
			    *colt-directory* *commons-directory* 
			    *jimi-directory* *eps-dump-directory* 
			    *use-gui-show* 
			    *force-jlinker-config*))


#+(and allegro-version>= (version>= 8 0))
(if *sneps-debug* 
    (proclaim '(optimize (safety 1) (space 1) (speed 2) (debug 3)))
   (proclaim '(optimize (safety 1) (space 0) (speed 3) (debug 0))))


;(defvar *sneps-compiler-optimization-settings*
;  #-cmu '(optimize (speed 2) (compilation-speed 1) (safety 1) (space 0))
;  #+cmu nil ;; CMUCL is way to verbose with the above setting.
;  "Optimization settings proclaimed during the compilation of SNePS.
;The default settings should be ok for most cases, but you can change 
;them to suit your needs and Lisp implementation.")

;;; added by flj on 2/12/04 to make sneps run faster
;(defvar *fast-sneps-compiler-optimization-settings*
;  #-cmu '(optimize (safety 1) (space 1) (speed 3) (debug 1))
;  #+cmu nil ;; CMUCL is way to verbose with the above setting.
;  "Optimization settings proclaimed during the compilation of SNePS.
;The default settings should be ok for most cases, but you can change 
;them to suit your needs and Lisp implementation.")


;;; New variable for better output control. Can be changed to any
;;; established stream, default is standard-output.

(defvar *default-output-stream* *standard-output*
  "Set output to standard out, unless defined already.")

(export '*default-output-stream*)

;; Deal with an old Allegro-3.x problem:
#+allegro (proclaim '(notinline LAST))

;; Symbolic version information (the revision name and the patch level -
;; the number after the `PL:') should match the CVS tag that was used to
;; tag this release):
(defparameter *sneps-version*
    (format nil "SNePS-2.8 [PL:0 ~a]"
	    (let ((date-string "$Date: 2013/08/28 19:07:21 $"))
	      (if (char-equal (aref date-string 0) #\$)
		  (subseq date-string 7 26)
		date-string))))

(defvar *sneps-make-option* :load
  "Used as a value for the MODE parameter of `make-simple-system'.")

(defvar *sneps-optional-systems*
    '(
      "sneps:demo;snere;load-core.LISP"
      "sneps:demo;snere;arcinfo;load-arc-snactor.LISP")
  "List of optional SNePS system files that need to be installed.")

(defun sneps-startup-query ()
  (format t 
	  "~%~%Do you want to load SNePS

    a) FAST (i.e., just load compiled files)
    b) by compiling source files that are newer than their 
       compiled version before they get loaded
    c) by compiling all source files before they get loaded
    d) by loading uncompiled source files only
    e) by compiling all source files before they get loaded plus
       compiling and loading all optional systems (installation option)

    Type a, b, c, d or e: ")

  ;; Reset options (might be set from previous runs):
  (setq *sneps-make-option* :load)

  (case (read *terminal-io*)
    (a (format t "~2%  Loading SNePS...~2%")
       (setq *sneps-make-option* :load))
    (b (format t "~2%  Changed source files will be compiled ~
                       before they are loaded....~2%")
       (setq *sneps-make-option* :compile))
    (c (format t "~2%  All source files will be compiled ~
                       before they are loaded....~2%")
       (setq *sneps-make-option* :recompile))
    (d (format t "~2%  All source files will be loaded uncompiled....~2%")
       (setq *sneps-make-option* :load-uncompiled))
    (e (format t "~2%  Compiling and loading SNePS and all ~
                       optional systems....~2%")
       (setq *sneps-make-option* :install))
    (t (format t "~2%  Loading SNePS....~2%")))
  )

(unless *sneps-noquery*
  (sneps-startup-query))

;; Logical Pathnames:

;; Boot the logical pathname system.  This is the only place where we have
;; to use physical pathnames in compile/load statements.  From here on
;; logical pathnames can be used in most Common-Lisp and SNePS commands that
;; take files as parameters (e.g., `open', `compile-file', `demo', etc.).
;; See the file `logical-pathnames.lisp' or refer to Guy Steele's
;; "Common Lisp: The Language" (2nd Edition), section 23.1.5 "Logical
;; Pathnames" for more information on logical pathnames.

;; Using logical pathnames is slightly complicated by the fact that not
;; all Lisps already provide an implementation for them. In case they
;; do not we use Mark Kantrowitz's implementation, but we have to make
;; some effort that that does not interfere with native implementations:

(if *sneps-use-lpmk*
    (pushnew :lpmk *features*))

;; We'll create this package regardless of whether we use a native
;; implementation of logical pathnames or not. Since this package
;; uses the LISP package, all logical pathname functions can be called
;; by qualifying them uniformly with `lp::' without having to worry
;; about what implementation is actually used.
(unless (find-package 'LOGICAL-PATHNAME)
  (make-package 'LOGICAL-PATHNAME :nicknames '("LP") :use '("LISP" "CL")))

(defun sneps-make-pathname (path name type &optional wild-inferiors)
  ;; Boot function for use until logical-pathnames are up and running.
  (format nil
	  #+(or mswindows win32)
          "~a~:[~;\\**~]\\~@[~a~]~@[.~a~]"
	  #+(or unix lm-unix)
	  "~a~:[~;/**~]/~@[~a~]~@[.~a~]"
	  #+(and explorer (not lm-unix))
	  "~a~:[~;.**~];~@[~a~]~@[.~a~]"
	  #+(and symbolics (not lm-unix))
	  "~a~:[~;>**~]>~@[~a~]~@[.~a~]"
	  #+vms
	  "~a~:[~;...~]]~@[~a~]~@[.~a~]"
	  #+apple
	  "~a~:[~;:**~]:~@[~a~]~@[.~a~]"
	  path
	  wild-inferiors
	  (and (stringp name) (not (string-equal name "")) name)
	  (and (stringp type) (not (string-equal type "")) type)
	  type))

(defun sneps-load0 (file)
  ;; Boot function for use until system-utils are up and running.
  (declare (special *load-verbose*))
  (setf *load-verbose* *sneps-verbose*)
  (let (#+allegro
	(excl::*enable-package-locked-errors* nil)
	#+allegro
	(*redefinition-warnings* *sneps-verbose*)
	#+lucid
	(*redefinition-action* *sneps-verbose*)
	#+(or explorer symbolics)
        (si:inhibit-fdefine-warnings t))
    (load file)))

(defun sneps-compile-load0 (file)
  ;; Boot function for use until system-utils are up and running.
  (let ((source-pathname
         (sneps-make-pathname
          *sneps-directory* file *sneps-lisp-extension*))
        (load-pathname
         (sneps-make-pathname *sneps-directory* file "")))
    (if (member *sneps-make-option* '(:recompile))
        (compile-file source-pathname))
    (if (member *sneps-make-option* '(:install))
       	(compile-file source-pathname))
    (if (eq *sneps-make-option* :load-uncompiled)
        (sneps-load0 source-pathname)
      (sneps-load0 load-pathname))))


;;; altered by flj on 2/12/04 to make sneps run faster
;(if *sneps-compiler-optimization-settings*
;     (cond ((member *sneps-make-option* '(:compile :recompile))
;	    (proclaim *sneps-compiler-optimization-settings*))
;	   ((member *sneps-make-option* '(:install))
;	    (proclaim *fast-sneps-compiler-optimization-settings*))))




#+lpmk
(sneps-compile-load0 "logical-pathnames")

;;; The following have been modified for compatibility with the EXE



(defvar *sneps-logical-pathname-translations* nil)
(defvar *sneps-patch-translations* nil)
(defvar *sneps-logical-hosts* '("sneps" "garnet" "sneps-p" "sneps21")
  "List of logical hosts recognized by SNePS.
Only namestrings starting with these hosts will be classified
as namestrings of logical pathnames.")

(defun set-logical-pathname-globals ()
  (setf *sneps-logical-pathname-translations*
    `(;; User translations first:
      ,@*sneps-user-translations*
;;; the next two lines are commented out for clisp compatibility (by FLJ 9/2/02)
;;;   ... because no load-before.lisp or other file exists      
;;;      ("before-sneps-load-hook" "sneps:load-before.LISP")  
;;;      ("after-sneps-load-hook" "sneps:load-after.LISP")
      ("snactor" "sneps:demo;snactor;load-snactor.LISP")
      ;; Even though `LISP' is a canonical type for Lisp source files
      ;; (CLtL-II, p.630), we have to explictly map it, since ACL 4.2
      ;; does not translate it into `lisp' the way it should.
      ("**;*.LISP" ,(sneps-make-pathname
		     *sneps-directory* "*" *sneps-lisp-extension* t))
      ("*.LISP" ,(sneps-make-pathname
		  *sneps-directory* "*" *sneps-lisp-extension* nil))
      ,@(if *sneps-binary-extension*
            `(("**;*.FASL" ,(sneps-make-pathname
			     *sneps-directory* "*"
			     *sneps-binary-extension* t))
              ("*.FASL" ,(sneps-make-pathname
			  *sneps-directory* "*"
			  *sneps-binary-extension* nil))))
      ;; CLISP (4/4/95) is very picky about exactly matching wildcards
      ;; between LHS and RHS.  It also needs to have every variation that
      ;; has an empty component (e.g., missing type) listed separately:
      ("**;*.*" ,(sneps-make-pathname *sneps-directory* "*" "*" t))
      ("**;*" ,(sneps-make-pathname *sneps-directory* "*" nil t))
      ("*.*" ,(sneps-make-pathname *sneps-directory* "*" "*" nil))
      ("*" ,(sneps-make-pathname *sneps-directory* "*" nil nil))))

  (setf *sneps-patch-translations*
    `(
      ("snactor" "sneps:demo;snactor;load-snactor.LISP")
      ;; Even though `LISP' is a canonical type for Lisp source files
      ;; (CLtL-II, p.630), we have to explictly map it, since ACL 4.2
      ;; does not translate it into `lisp' the way it should.
      ("**;*.LISP" ,(sneps-make-pathname
		     *sneps-patch-directory* "*" *sneps-lisp-extension* t))
      ("*.LISP" ,(sneps-make-pathname
		  *sneps-patch-directory* "*" *sneps-lisp-extension* nil))
      ,@(if *sneps-binary-extension*
            `(("**;*.FASL" ,(sneps-make-pathname
			     *sneps-patch-directory* "*"
			     *sneps-binary-extension* t))
              ("*.FASL" ,(sneps-make-pathname
			  *sneps-patch-directory* "*"
			  *sneps-binary-extension* nil))))
      ;; CLISP (4/4/95) is very picky about exactly matching wildcards
      ;; between LHS and RHS.  It also needs to have every variation that
      ;; has an empty component (e.g., missing type) listed separately:
      ("**;*.*" ,(sneps-make-pathname *sneps-patch-directory* "*" "*" t))
      ("**;*" ,(sneps-make-pathname *sneps-patch-directory* "*" nil t))
      ("*.*" ,(sneps-make-pathname *sneps-patch-directory* "*" "*" nil))
      ("*" ,(sneps-make-pathname *sneps-patch-directory* "*" nil nil)))
    )
  
  (setf *sneps-logical-hosts* '("sneps" "garnet" "sneps-p" "sneps21")))

(set-logical-pathname-globals)


;; Load a file that actually installs these translations in the
;; logical pathname package.  This is a separate file so that the acl
;; standalone can also reestablish the translations (which, starting
;; with version 4.3 are thrown away when a dumped image restarts.)
(load (format nil "~A/load-logical-pathnames.~A" *sneps-directory* *sneps-default-lisp-extension*))


;; End of Logical Pathnames

;; Load simple system utilities; after that we have a proper
;; version of `sneps-load' which understands logical pathnames:
(sneps-compile-load0 "system-utils")

;;; commented out by FLJ 9/2/02
;;;;; Load before hook file if it exists:
;;;(if (sneps-probe-file "sneps:before-sneps-load-hook")
;;;    (sneps-load "sneps:before-sneps-load-hook"))

;; Compile/load SNePS:
(cond ((eq *sneps-make-option* :install)
       (setq *sneps-make-option* :recompile)
       (sneps-load "sneps:system.LISP")
       (dolist (system *sneps-optional-systems*)
	 (sneps-load system)))
      (t ;; *** For temporary Sneps 2.6.2 ***
       (load (concatenate 'string *sneps-patch-directory* "/system.lisp"))))

(defun sneps ()
  (let ((*package* (find-package 'snepsul)))
    (sneps:sneps)))

(defun snepslog (&rest args)
  (apply #'sneps:snepslog args))

;;; commented out by FLJ 9/2/02
;;;;; Load after hook file if it exists:
;;;(if (sneps-probe-file "sneps:after-sneps-load-hook")
;;;    (sneps-load "sneps:after-sneps-load-hook"))

#+(and allegro-version>= (version>= 6 0))
(progn
  (setf *print-case* *pre-sneps-load-print-case-mode*)
  (cond ((equal *pre-sneps-load-case-mode* :case-sensitive-lower)
	 (excl:set-case-mode :case-sensitive-lower)
	 (excl::convert-mixed-case-symbols nil))))


;;; Only needed for the api, which is allegro only

#+allegro
(defun check-and-start-java-sneps-api ()
  "If command line parameters are passed that specify the classpath and port 
the api is to connect to, starts the JavaSnepsAPI"
(when (and (member "-api-port" (sys:command-line-arguments) :test #'string=)
	   (member "-api-classpath" (sys:command-line-arguments) 
		   :test #'string=))
  (sys:with-command-line-arguments 
     (("config-file" :long config-file :required-companion)
      ("api-port" :long api-port :required-companion)
      ("api-classpath" :long api-classpath :required-companion))
     (rest :usage "This is a test" :long-abbrev t)
     (when (and api-port api-classpath)
       (snepslog:init-java-sneps-connection
	(read-from-string api-port) api-classpath)))))

#+allegro
(check-and-start-java-sneps-api)


;;; Only needed for the exe, which is allegro only
#+allegro
(defun start-exe-top-level ()
  "Initiates the toplevel interaction, needed for the executeable."
  (set-and-load-config)
  (set-logical-pathname-globals)
  (load (format nil "~A/load-logical-pathnames.~A" *sneps-directory* *sneps-default-lisp-extension*))
  (check-and-start-java-sneps-api)
  (format *standard-output* "~&~a loaded.~
           ~%Type `(sneps)' or `(snepslog)' to get started."
	  *sneps-version*)
  (tpl:start-interactive-top-level *terminal-io* 
				   #'tpl:top-level-read-eval-print-loop
				   nil))


(format t "~&~a loaded.~
           ~%Type `(sneps)' or `(snepslog)' to get started."
	*sneps-version*)




    




