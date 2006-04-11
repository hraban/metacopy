(in-package common-lisp-user)
(defpackage :metacopy-system (:use #:cl #:asdf))
(in-package :metacopy-system)

(defsystem metacopy
  :version "0.2"
  :author "Originally by BBN, modified by EKSL and by Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Flexible Common Lisp shallow/deep copy mechanism."
  :components ((:module "dev"
		        :components ((:static-file "notes.text")
				     
                                     (:file "package")
                                     (:file "api"
                                            :depends-on ("package"))
                                     (:file "copy"
                                            :depends-on ("api" "package"))))
               
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  :in-order-to ((test-op (load-op metacopy-test)))
  :perform (test-op :after (op c)
                    (describe
                     (funcall 
                      (intern "RUN-TESTS" '#:lift) 
                      :suite (intern 
                              "METACOPY-TEST"
                              '#:metacopy-test))))
  :depends-on (moptilities))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'metacopy))))
  (values nil))