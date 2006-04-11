(in-package common-lisp-user)
(defpackage :metacopy-test-system (:use #:cl #:asdf))
(in-package :metacopy-test-system)

(defsystem metacopy-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Test for metacopy"

  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "tests" :depends-on ("package"))))
               
               (:module "dev"
                        :components ((:static-file "notes.text"))))
  :depends-on (metacopy lift))

;;; ---------------------------------------------------------------------------

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'metacopy-test))))
  (values nil))