(in-package :common-lisp-user)
(defpackage #:metacopy-system
  (:use #:cl #:asdf)
  (:export
   #:metacopy-package
   #:metacopy-test-package
   #:*load-with-contextl*))
(in-package #:metacopy-system)


;;; try to load asdf-system-connections
(unless (find-system 'asdf-system-connections nil)
  (when (find-package 'asdf-install)
    (eval (read-from-string "(asdf-install:install '#:asdf-system-connections)"))))

(unless (find-system 'asdf-system-connections nil)
  (error "The metacopy system requires asdf-system-connections. See http://www.cliki.net/asdf-system-connections for details and download instructions."))

(operate 'load-op 'asdf-system-connections)


;;; set up stuff we will use while loading the systems
(defparameter *load-with-contextl* nil
  "This is t when loading with contextl. You can set this var at runtime to \"switch\" packages while working with slime (currently needs a pending slime patch).")
(defun metacopy-package ()
  "This is the package in which the metacopy files are read in. It returns '#:metacopy-with-contextl when loading through the system-connection."
  (if *load-with-contextl*
      '#:metacopy-with-contextl
      '#:metacopy))
(defun metacopy-test-package ()
  "See metacopy-package."
  (if *load-with-contextl*
      '#:metacopy-test-with-contextl
      '#:metacopy-test))



;;; define the magic metacopy-file that will emit NAME.fasl and NAME-contextl.fasl
(defclass metacopy-file (cl-source-file)
  ())

(defclass metacopy-file-with-contextl (metacopy-file)
  ())

(defmacro with-contextl (&body body)
  `(let ((*load-with-contextl* t)
         (*features* (cons :with-contextl *features*)))
    ,@body))

(defmethod perform :around (o (c metacopy-file-with-contextl))
  (with-contextl
    (call-next-method)))

(defmethod output-files :around ((operation operation) (component metacopy-file-with-contextl)) 
  (with-contextl
    (let* ((paths (call-next-method))
           (file (first paths)))
      (assert (<= (length paths) 1))
      (if paths
          (list (merge-pathnames (concatenate 'string (pathname-name file) "-contextl") file))
          nil))))

(defsystem metacopy
  :version "0.2"
  :author "Originally by BBN, modified by EKSL and by Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Flexible Common Lisp shallow/deep copy mechanism."
  :default-component-class metacopy-file
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

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'metacopy))))
  nil)

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
  :default-component-class metacopy-file
  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "tests" :depends-on ("package"))))
               (:module "dev"
                        :components ((:static-file "notes.text"))))
  :depends-on (metacopy lift))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'metacopy-test))))
  nil)

;;; and the system connection that will load the entire metacopy code again into another package called :metacopy-with-contextl
(defsystem-connection metacopy-with-contextl
  :requires (metacopy contextl)
  :default-component-class metacopy-file-with-contextl
  ;; The contents here is (an unfortunate) copy-paste from the metacopy system.
  :components ((:module "dev"
                :components ((:file "package")
                             (:file "contextl-integration" :depends-on ("package"))
                             (:file "api" :depends-on ("contextl-integration" "package"))
                             (:file "copy" :depends-on ("api" "package"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'metacopy-with-contextl))))
  (operate 'load-op '#:metacopy-test-with-contextl)
  (describe
   (eval (read-from-string
          "(lift:run-tests :suite 'metacopy-test-with-contextl::metacopy-test)"))))

(defsystem-connection metacopy-test-with-contextl
  :requires (metacopy-test metacopy-with-contextl lift)
  :default-component-class metacopy-file-with-contextl
  ;; The contents here is (an unfortunate) copy-paste from the metacopy-test system.
  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "tests" :depends-on ("package"))))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'metacopy-with-contextl))))
  nil)




