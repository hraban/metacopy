(cl:in-package #:metacopy-system)

(defmacro define-metacopy-package (package-name contextl)
  "Dynamically define a metacopy package.

Using reader macros leads to inconsistent results between ECL & SBCL. A regular
macro proves more reliable.

The arguments are evaluated before being used, as if passed to a function.
"
  `(defpackage ,(eval package-name)
     (:use #:common-lisp #:moptilities #:metacopy-system ,@(when (eval contextl) '(#:contextl)))
     (:export
      #:with-slot-copying
      #:copy-slot
      #:copy-set-slot
      #:copy-slots
      #:copy-cond-slot
      #:copyable-mixin
      #:copy-inner-class
      #:instance-made-for-copying-p
      #:copy-self copy-inner
      #:copy-top-level copy-one
      #:copy-slots-slots-to-initialize
      #:defcopy-methods
      #:*copy-assume-no-circular-lists*
      #:copy-template
      #:make-instance-from-object-initargs
      #:duplicate-set
      #:duplicate-slots
      #:duplicate-cond-slots

      #:define-copy-protocol
      #:define-copy-method

      ;; usual public interface
      #:duplicator-methods
      #:copy-thing)))

(define-metacopy-package (metacopy-system:metacopy-package) metacopy-system:*load-with-contextl*)
