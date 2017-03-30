(find-system :metacopy)

(in-package #:metacopy-system)

(defclass metacopy-file-with-contextl (cl-source-file)
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
      (when paths
        (setf paths (remove-if (lambda (s) (string= (asdf/lisp-build:warnings-file-type) (pathname-type s))) paths))
        (assert (<= (length paths) 1))
        (list (merge-pathnames (concatenate 'string (pathname-name file) "-contextl") file))))))

;;; and the system that will load the entire metacopy code again into another package called :metacopy-with-contextl
(defsystem "metacopy-with-contextl"
  :depends-on ("metacopy" "contextl")
  :default-component-class metacopy-file-with-contextl
  ;; The contents here is (an unfortunate) copy-paste from the metacopy system.
  :components ((:module "dev"
                :components ((:file "package")
                             (:file "contextl-integration" :depends-on ("package"))
                             (:file "api" :depends-on ("contextl-integration" "package"))
                             (:file "copy" :depends-on ("api" "package")))))
  :in-order-to ((test-op (test-op "metacopy-with-contextl/test"))))

(defsystem "metacopy-with-contextl/test"
  :depends-on ("metacopy/test" "metacopy-with-contextl" "lift")
  :default-component-class metacopy-file-with-contextl
  ;; The contents here is (an unfortunate) copy-paste from the metacopy-test system.
  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "tests" :depends-on ("package")))))
  :perform (test-op (o c)
             (describe
              (eval (read-from-string
                     "(lift:run-tests :suite 'metacopy-test-with-contextl::metacopy-test)")))))
