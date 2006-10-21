(cl:in-package #:metacopy-system)

(defpackage #.(metacopy-system:metacopy-test-package)
  (:use #:common-lisp #:moptilities #:lift #.(if metacopy-system:*load-with-contextl*
                                                 '#:metacopy-with-contextl
                                                 '#:metacopy)))
