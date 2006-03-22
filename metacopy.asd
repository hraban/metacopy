
(defpackage :metacopy-system (:use #:cl #:asdf))
(in-package :metacopy-system)

(defsystem metacopy
  :version "0.1"
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
  :depends-on (moptilities metatilities-base))

