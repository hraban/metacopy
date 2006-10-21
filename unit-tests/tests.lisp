(in-package #.(metacopy-system:metacopy-test-package))

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


#+with-contextl
(progn
  (define-copy-protocol adder-copy)
  (define-copy-protocol subber-copy)
  (define-copy-protocol list-element-duplicating-copy)
  (define-copy-protocol adder-and-list-element-duplicating-copy (adder-copy list-element-duplicating-copy))

  (define-copy-method (copy-one adder-copy) ((n number) ht)
    (1+ n))
  
  (define-copy-method (copy-one subber-copy) ((n number) ht)
    (1- n))
  
  (define-copy-method (copy-one list-element-duplicating-copy) ((list cons) ht)
    (mapcan (lambda (el)
              (list (copy-one el ht) (copy-one el ht)))
            list))
  
  (deftestsuite contextual (metacopy-test)
    ()
    (:test ((ensure-same (adder-copy '(1 "foo" 2)) '(2 "foo" 3) :test #'equal)))
    (:test ((ensure-same (subber-copy '(2 "bar" 3)) '(1 "bar" 2) :test #'equal)))
    (:test ((ensure-same (list-element-duplicating-copy '(2 3)) '(2 2 3 3) :test #'equal)))
    (:test ((ensure-same (adder-and-list-element-duplicating-copy '(2 "x" 3)) '(3 3 "x" "x" 4 4) :test #'equal)))))

;; hash-tables
;; structures
;; more lists
;; objects
