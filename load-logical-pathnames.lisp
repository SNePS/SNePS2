;; This file actually installs translations in the logical pathname
;; package.  This is a separate file so that the acl standalone can
;; also reestablish the translations (which, starting with version 4.3
;; are thrown away when a dumped image restarts.)


(setf (lp::logical-pathname-translations "sneps")
      *sneps-logical-pathname-translations*)

(setf (lp::logical-pathname-translations "sneps-p")
      *sneps-patch-translations*)

;; For backward compatibility:
(setf (lp::logical-pathname-translations "sneps21")
      *sneps-logical-pathname-translations*)



#+lpmk
(progn

  (when (eq *sneps-use-lpmk* :redefine)
    ;; Redefine standard functions such as `load' with LPMK versions
    ;; that understand logical pathnames. If your Lisp locks the LISP
    ;; package you'll need to find out how to break that lock.
    #+allegro
    (let ((*enable-package-locked-errors* nil))
      (lp:redefine-standard-functions))
    #+clisp
    (#.(if (fboundp 'appease-cerrors) 'appease-cerrors 'progn)
       (lp:redefine-standard-functions))
    #-(or allegro clisp)
    (lp:redefine-standard-functions))

  ;; Define type of the default host:
  (setf (lp:physical-host-type nil)
    #+unix :unix
    #+lm-unix :unix
    #+(and explorer (not lm-unix)) :explorer
    #+(and symbolics (not lm-unix)) :symbolics
    #+vms :vms)

  ;; In case the various physical directories contain a host itself
  ;; we must define its type; assume it is the same as for the default
  ;; host -- this assumption might be wrong!!
  (dolist (dir `(,*sneps-directory*
		 ,*sneps-patch-directory*))
    (let ((host (lp::get-host-string dir)))
      (when (and host (null (lp:physical-host-type host)))
	(setf (lp:physical-host-type host)
	  (lp:physical-host-type nil)))))

  ;; define unique canonical translations for LISP and FASL extensions:
  (lp:define-host-type :unix
      (:canonicals (:type (:lisp #.*sneps-default-lisp-extension*)
			  (:fasl #.*sneps-binary-extension*))))
  (lp:define-host-type :vms
      (:canonicals (:type (:lisp #.*sneps-default-lisp-extension*)
			  (:fasl #.*sneps-binary-extension*))))

  ) ;; progn

