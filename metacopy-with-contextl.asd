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
    (let ((paths (call-next-method)))
      ;; Any output files also get -contextl added. This used to filter out any
      ;; “warnings file” (cf. uiop:*warnings-file-type*), but that check has
      ;; been removed because it seems to cause more trouble than it is
      ;; worth. If there is a problem arising from the renaming of warning
      ;; files, this is where to restore the filter.
      (mapcar (lambda (path)
                (make-pathname :name (concatenate 'string (pathname-name path) "-contextl")
                               :defaults path))
              paths))))

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
