(in-package common-lisp-user)

(defpackage #:metacopy
  (:use #:common-lisp #:moptilities #:metatilities)
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

   ;; usual public interface
   #:duplicator-methods
   #:copy-thing))
