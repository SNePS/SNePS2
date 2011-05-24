;;; Composition of certain paths

;;; Make Before Transitive
(define-path before (compose before (kstar (compose after- ! before))))

;;; Make After Transitive
(define-path after (compose after (kstar (compose before- ! after))))

;; If X is a member of the class Y and Y is a subclass of Z then
;;   X is a member of the class Z.
;; Example:  If Fido is a brachet and all brachets are hounds then 
;;;  Fido is a hound.
(define-path class (compose class (kstar (compose subclass- ! superclass))))

;; Make subclass transitive
(define-path subclass (compose subclass (kstar (compose superclass- ! subclass))))


