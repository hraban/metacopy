
(defpackage :metacopy-system (:use #:cl #:asdf))
(in-package :metacopy-system)

(defsystem metacopy
  :version "0.1"
  :author "BBN and EKSL"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Common Lisp flexible shallow/deep copy mechanism"
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

