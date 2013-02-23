(in-package :common-lisp-user)

(defpackage #:metacopy-system
  (:use #:cl #:asdf)
  (:export
   #:metacopy-package
   #:metacopy-test-package
   #:*load-with-contextl*))

(in-package #:metacopy-system)

;;; set up stuff we will use while loading the systems
(defparameter *load-with-contextl* nil
  "This is t when loading with contextl. You can set this var at runtime to \"switch\" packages while working with slime (currently needs a pending slime patch).")

(defun metacopy-package ()
  "This is the package in which the metacopy files are read in."
  (if *load-with-contextl*
      '#:metacopy-with-contextl
      '#:metacopy))

(defun metacopy-test-package ()
  "See metacopy-package."
  (if *load-with-contextl*
      '#:metacopy-test-with-contextl
      '#:metacopy-test))

(defsystem metacopy
  :version "0.2"
  :author "Originally by BBN, modified by EKSL and by Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Flexible Common Lisp shallow/deep copy mechanism."
  :components ((:module "dev"
                :components ((:static-file "notes.text")
                             (:file "package")
                             (:file "contextl-integration" :depends-on ("package"))
                             (:file "api" :depends-on ("contextl-integration" "package"))
                             (:file "copy" :depends-on ("api" "package"))))
               (:module "website"
                :components ((:module "source"
                              :components ((:static-file "index.lml"))))))
  :depends-on (moptilities))

(defmethod perform ((o test-op) (c (eql (find-system 'metacopy))))
  (operate 'load-op '#:metacopy-test)
  (describe
   (eval (read-from-string
          "(lift:run-tests :suite 'metacopy-test::metacopy-test)"))))

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
