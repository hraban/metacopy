(in-package metacopy-test)

(deftestsuite metacopy-test () ())

(deftestsuite test-simples (metacopy-test)
  ()
  (:test ((ensure-same (copy-thing 2) 2)))
  (:test ((ensure-same (copy-thing #\a) #\a)))
  (:test ((ensure-same (copy-thing "what") "what")))
  (:test ((ensure-same (copy-thing 'ouch) 'ouch))))

(deftestsuite test-arrays (metacopy-test)
  ()
  (:test (simple-array
          (let ((a #(1 2 3 4)))
            (ensure-same (copy-thing a) a :test 'equalp))))
  (:test (nested-array
          (let ((a #(1 2 #(a b) 4)))
            (ensure-same (copy-thing a) #(1 2 #(a b) 4) :test 'equalp)))))


(deftestsuite test-lists (metacopy-test)
  ()
  (:test (simple-list
          (let ((a '(1 2 3)))
            (ensure (not (eq (copy-thing a) a)))
            (ensure-same (copy-thing a) a :test #'equal))))
  (:test (dotted-pair
          (let ((a '(1 . 2)))
            (ensure-same (copy-thing a) a :test #'equal)))))


;; hash-tables
;; structures
;; more lists
;; objects
