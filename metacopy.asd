(in-package common-lisp-user)
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

(defmethod output-files :around ((operation operation) (component metacopy-file)) 
  (let* ((paths (call-next-method))
         (file (first paths)))
    (assert (<= (length paths) 1))
    (if paths
        (list (if *load-with-contextl*
                  (merge-pathnames (concatenate 'string (pathname-name file) "-contextl") file)
                  file))
        nil)))

(defsystem metacopy
  :version "0.2"
  :author "Originally by BBN, modified by EKSL and by Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Flexible Common Lisp shallow/deep copy mechanism."
  :components ((:module "dev"
                :components ((:static-file "notes.text")
                             (:metacopy-file "package")
                             (:metacopy-file "contextl-integration" :depends-on ("package"))
                             (:metacopy-file "api" :depends-on ("contextl-integration" "package"))
                             (:metacopy-file "copy" :depends-on ("api" "package"))))
               (:module "website"
                :components ((:module "source"
                              :components ((:static-file "index.lml"))))))
  :depends-on (moptilities))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'metacopy))))
  nil)

(defmethod operate ((o test-op) (c (eql (find-system 'metacopy))) &key &allow-other-keys)
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
                        :components ((:metacopy-file "package")
                                     (:metacopy-file "tests" :depends-on ("package"))))
               (:module "dev"
                        :components ((:static-file "notes.text"))))
  :depends-on (metacopy lift))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'metacopy-test))))
  nil)

;;; and the system connection that will load the entire metacopy code again into another package, :metacopy-with-contextl
(defsystem-connection metacopy-with-contextl
  :requires (metacopy contextl)
  ;; The contents here is (an unfortunate) copy-paste from the metacopy system.
  :components ((:module "dev"
                :components ((:metacopy-file "package")
                             (:metacopy-file "contextl-integration" :depends-on ("package"))
                             (:metacopy-file "api" :depends-on ("contextl-integration" "package"))
                             (:metacopy-file "copy" :depends-on ("api" "package"))))))

(defmethod operate ((o test-op) (c (eql (find-system 'metacopy-with-contextl))) &key &allow-other-keys)
  (operate 'load-op '#:metacopy-test-with-contextl)
  (describe
   (eval (read-from-string
          "(lift:run-tests :suite 'metacopy-test-with-contextl::metacopy-test)"))))

(defsystem-connection metacopy-test-with-contextl
  :requires (metacopy-test metacopy-with-contextl lift)
  ;; The contents here is (an unfortunate) copy-paste from the metacopy-test system.
  :components ((:module "unit-tests"
                        :components ((:metacopy-file "package")
                                     (:metacopy-file "tests" :depends-on ("package"))))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'metacopy-with-contextl))))
  nil)

(defmacro with-contextl (&body body)
  `(let ((*load-with-contextl* t)
         (*features* (cons :with-contextl *features*)))
    ,@body))

(defmethod operate :around ((o t) (c (eql (find-system 'metacopy-with-contextl))) &key &allow-other-keys)
  (with-contextl
    (call-next-method)))

(defmethod operate :around ((o t) (c (eql (find-system 'metacopy-test-with-contextl))) &key &allow-other-keys)
  (with-contextl
    (call-next-method)))



