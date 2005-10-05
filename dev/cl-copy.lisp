;;; -*- Syntax: Common-lisp; Package: cl-copy -*-

#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(defpackage "CL-COPY"
  (:use "COMMON-LISP" "MOPTILITIES" "METATILITIES")
  (:export
   #:with-stack-list-copy 
   #:with-slot-copying
   #:copy-slot
   #:copy-set-slot
   #:copy-slots
   #:copy-cond-slot
   #:copyable-mixin
   #:copy-inner-class 
   #:instance-made-for-copying-p
   #:copy-self copy-inner
   #:copy-top-level copy-one
   #:copy-slots-slots-to-initialize
   #:defcopy-methods
   #:*copy-assume-no-circular-lists*
   #:copy-template
   #:make-instance-from-object-initargs
   #:duplicate-set
   #:duplicate-slots
   #:duplicator-methods
   #:duplicate-cond-slots
   #:cl-copy))
(in-package cl-copy)

;;; ---------------------------------------------------------------------------

(defvar *copy-assume-no-circular-lists* nil)

;;; ---------------------------------------------------------------------------

(defgeneric copy-top-level (thing)
  (:documentation "Copy objects with aribtrarily complex substructure.
Objects are kept track of in a HashTable, so only one copy is made of each.
Things which are EQ in the original (i.e. objects, sublists, etc.) come out
EQ in the corresponding places in the copy."))

;;; ---------------------------------------------------------------------------

(defmethod copy-top-level (ORIGINAL-THING) 
  (copy-one ORIGINAL-THING (make-hash-table)))

#+second-try
(defvar *ctl-copy-hash-table* (make-hash-table))

#+second-try
(defmethod copy-top-level (ORIGINAL-THING)
    (clrhash *ctl-copy-hash-table*)
    (copy-one ORIGINAL-THING *ctl-copy-hash-table*))  
  
#+original
(let ((copy-htable (make-hash-table)))
  (defmethod copy-top-level (ORIGINAL-THING)
    (clrhash COPY-HTABLE)
    (copy-one ORIGINAL-THING COPY-HTABLE)))

;;; ---------------------------------------------------------------------------

(defgeneric copy-one (SELF COPY-HTABLE)
  (:documentation "Returns a fullfledged copy of SELF, set-up and ready to go."))


;;;********************************************************************************
;;; Some simple cases.
;;; Copies these objects are always eq to the original and have no 
;;; internal structure.  -->  So just use the objects use themselves.
;;; (I.e. no need to worry about caching them).
;;;
;;; Gary King 2003-04-01: for speed, we do this for copy-one _and_ copy-top-level
;;;
;;;********************************************************************************

(defmethod copy-top-level ((ORIGINAL-THING symbol))
  original-thing)

(defmethod copy-one ((ORIGINAL-SYMBOL symbol) COPY-HTABLE)
  (declare (ignore COPY-HTABLE))
  ORIGINAL-SYMBOL)

(defmethod copy-top-level ((ORIGINAL-THING number))
  original-thing)

(defmethod copy-one ((ORIGINAL-NUMBER number) COPY-HTABLE)
  (declare (ignore COPY-HTABLE))
  ORIGINAL-NUMBER)

(defmethod copy-top-level ((ORIGINAL-THING function))
  original-thing)

(defmethod copy-one ((ORIGINAL-FUNCTION function) COPY-HTABLE)
  (declare (ignore COPY-HTABLE))
  ORIGINAL-FUNCTION)

(defmethod copy-top-level ((ORIGINAL-THING character))
  original-thing)

(defmethod copy-one ((orig character) copy-htable)
  (declare (ignore copy-htable))
  orig)

#+MCL-WHEN-YOU-WANT-TO-KNOW
(defmethod copy-one ((ORIGINAL-FUNCTION ccl::compiled-lexical-closure) COPY-HTABLE)
  (declare (ignore COPY-HTABLE))
  (warn "Copying ~A" ORIGINAL-FUNCTION)
  ORIGINAL-FUNCTION)

;;;********************************************************************************
;;; The hairier, default case. 
;;; In general,
;;;  1] Objects can have internal structure (and, for instance, circular 
;;;     refrences) and/or
;;;  2] Copies are not eql to the original.
;;;
;;; The basic idea here is to only make one copy of the ORIGINAL-THING and 
;;; store it in the HashTable for future use.  In this way, the Copied 
;;; Object has the same "eq-connectedness" that the original had.
;;;********************************************************************************

(defgeneric copy-self (SELF)
  (:documentation "Return a new, empty version of SELF"))

(defgeneric copy-inner (SELF COPY-OBJECT COPY-HTABLE)
  (:documentation
    "Copy the relevant portions of SELF into COPY-OBJECT.
     OK if it calls COPY on sub-objects."))

(defgeneric copy-final (SELF COPY)
  (:documentation "Last pass to make sure everything is in place."))

;;; ---------------------------------------------------------------------------

;;; So, in short there are three steps (if I've not already been Copied):
;;;  1] Create a new, empty copy  (using COPY-SELF).
;;;  2] Shove it in the HashTable.
;;;  3] Setup its internal structure, as needed (using COPY-INNER).
(defmethod copy-one (ORIGINAL-THING COPY-HTABLE)
  (multiple-value-bind (VALUE FOUND?) (gethash ORIGINAL-THING COPY-HTABLE)
    (or (and FOUND? VALUE)
	(let ((COPY-THING (copy-self ORIGINAL-THING)))
          (setf (gethash ORIGINAL-THING COPY-HTABLE) COPY-THING)
	  (copy-inner ORIGINAL-THING COPY-THING COPY-HTABLE)
	  (copy-final ORIGINAL-THING COPY-THING)
	  COPY-THING))))

;;; ---------------------------------------------------------------------------

;; copies without worrying about shared structure
(defmethod copy-one (original-thing (copy-htable (eql nil)))
  (let ((copy-thing (copy-self original-thing)))
    (copy-inner original-thing copy-thing copy-htable)
    (copy-final original-thing copy-thing)
    copy-thing))

;;; ---------------------------------------------------------------------------

(defmethod copy-self (SELF)
  (error "Don't know how to copy ~S" self))

(defmethod copy-inner (SELF COPY-OBJECT COPY-HTABLE)
  "Default is to do nothing."
  (declare (ignore SELF COPY-OBJECT COPY-HTABLE))
  nil)

(defmethod copy-final ((self t) (copy t))
  "Default is to do nothing."
  nil)

;;; ---------------------------------------------------------------------------
;;; Strings

(defmethod copy-self ((ORIGINAL string))
  (subseq ORIGINAL 0 (length ORIGINAL)))

;; Define this so that array code below does not run for strings.
(defmethod copy-inner ((ORIGINAL string) new-array copy-htable)
  (declare (ignore new-array copy-htable)))

;;; ---------------------------------------------------------------------------
;;; Arrays

(defmethod copy-self ((original array))
  (let* ((array-dimensions (array-dimensions original))
         (adjustable (adjustable-array-p original))
         (type (array-element-type original))
         (fp (when (array-has-fill-pointer-p original)
               (fill-pointer original))))
    (make-array array-dimensions
                :element-type type
                :adjustable adjustable
                :fill-pointer fp)))

(defmethod copy-inner ((original array) new-array copy-htable)
  (loop for index from 0 below (apply #'* (array-dimensions original)) do
        (setf (row-major-aref new-array index) 
              (copy-one (row-major-aref original index) copy-htable))))

;;; ---------------------------------------------------------------------------
;;; Hash Tables

(defmethod copy-self ((original hash-table))
  (make-hash-table :test (hash-table-test original)))

(defmethod copy-inner ((original-table hash-table) new-table copy-htable)
  (maphash #'(lambda (key value)
               (setf (gethash 
                      (copy-one key copy-htable)
                      new-table)
                     (copy-one value copy-htable)))
           original-table))

;;;********************************************************************************
;;; Lists
;;;********************************************************************************
;;; The Old, Boring, Common-Lisp compatible Way.
#-lispm
(defmethod copy-self ((ORIGINAL-LIST list))
  (and ORIGINAL-LIST (cons nil nil)))

#-lispm
(defmethod copy-inner ((original-list list) copy-list copy-htable)
  ;; this handles circular lists, but is slower and isn't cdr coded.
  (cond ((not (null *copy-assume-no-circular-lists*))
         (setf (car copy-list) 
               (copy-one (first original-list) copy-htable)
               (cdr copy-list)
               (if (dotted-pair-p original-list)
                 (copy-one (cdr original-list) copy-htable)
                 (loop for x in (rest original-list) collect (copy-one x copy-htable)))))
        (t
         (unless (null original-list)
           (setf (car copy-list) (copy-one (car original-list) copy-htable))
           (setf (cdr copy-list) (copy-one (cdr original-list) copy-htable))))))

;;;********************************************************************************
;;; Copy-able Class objects.
;;;********************************************************************************


(defclass copyable-mixin
	  ()
    ()
  (:documentation
    "Provides method for doing COPY that creates a copy on an object.
     Each mixin should provide an COPY-INNER-CLASS method to copy its
     slots appropriately."))

(defvar *instance-for-copy?* nil)

(defgeneric copy-slots-slots-to-initialize (self)
  (:method-combination append :most-specific-last)
  (:method  append ((self standard-object))
            (values nil)))

(defmethod copy-self ((SELF copyable-mixin))
  (copy-object self))

(defun copy-object (self)
  (let ((i (allocate-instance (class-of SELF))))
    (let ((*instance-for-copy?* i))
      (shared-initialize i (copy-slots-slots-to-initialize self)))))

(defmethod instance-made-for-copying-p ((object t))
  (eq object *instance-for-copy?*))

(defgeneric copy-inner-class (SELF COPY-OBJECT COPY-HTABLE)
  (:method-combination progn :most-specific-last)
  (:documentation
    "Defined for each component class of an object with mixin COPYABLE-MIXIN.
     It should setup its slots as appropriate.
     This needs to be a seperate method (from COPY-INNER) because it has
     to be done with a PROGN Method-Combination."))

(defmethod copy-inner-class progn ((ORIGINAL-OBJECT copyable-mixin) COPY-LIST COPY-HTABLE)
  (declare (ignore COPY-LIST COPY-HTABLE))
  nil)

(defmethod copy-inner ((ORIGINAL-OBJECT standard-object) COPY-LIST COPY-HTABLE)
  (copy-inner-class ORIGINAL-OBJECT COPY-LIST COPY-HTABLE))

(defgeneric copy-final-class (SELF COPY)
  (:method-combination progn)
  (:documentation
    "Defined for each component class of an object with mixin COPYABLE-MIXIN.
     It should setup its slots as appropriate.
     This needs to be a seperate method (from COPY-FINAL) because it has
     to be done with a PROGN Method-Combination."))

(defmethod copy-final-class progn ((ORIGINAL-OBJECT copyable-mixin) (copy t))
  nil)

(defmethod copy-final ((ORIGINAL-OBJECT copyable-mixin) (copy t))
  (copy-final-class ORIGINAL-OBJECT copy))

;;;********************************************************************************
;;; Things to make using COPY-INNER-CLASS easier.
;;;********************************************************************************

(defun copy-set-slot-1 (COPY-OBJECT SLOT-NAME VALUE)
  (setf (slot-value COPY-OBJECT SLOT-NAME)
	 VALUE))

;; Maybe make this deal with unbound slots - could just ignore them which would make
;; the slot unbound in the copy as well. - Westy
;; added the slot-boundp check, GWK 20011022
(defun copy-slot-1 (COPY-OBJECT SLOT-NAME ORIGINAL-OBJECT COPY-HTABLE)
  (when (slot-boundp ORIGINAL-OBJECT SLOT-NAME)
    (copy-set-slot-1 COPY-OBJECT SLOT-NAME
                     (copy-one (slot-value ORIGINAL-OBJECT SLOT-NAME)
                               COPY-HTABLE))))

;;; (copy-set-slot (SLOT-NAME VALUE)
;;;   Set the contents of SLOT-NAME in COPY-OBJECT to VALUE.
;;; (copy-slot (SLOT-NAME) ...
;;;   Set the contents of SLOT-NAME in COPY-OBJECT to be a copyicate of the
;;;   contents of the same slot in ORIGINAL-OBJECT.
(defmacro with-slot-copying
	  ((COPY-OBJECT COPY-HTABLE &optional (ORIGINAL-OBJECT 'SELF)) &body BODY)
  `(macrolet ((copy-slot (SLOT-NAME)
		`(copy-slot-1 ,',COPY-OBJECT ',SLOT-NAME ,',ORIGINAL-OBJECT ,',COPY-HTABLE))
              (copy-cond-slot (SLOT-NAME TEST-FORM)
		`(when ,test-form
                   (copy-slot-1 ,',COPY-OBJECT ',SLOT-NAME ,',ORIGINAL-OBJECT ,',COPY-HTABLE)))
	      (copy-set-slot (SLOT-NAME VALUE)
		`(copy-set-slot-1 ,',COPY-OBJECT ',SLOT-NAME ,VALUE)))
     (macrolet ((copy-slots (&rest SLOT-NAMES)
		  `(progn
		     ,@(loop for SLOT-NAME in SLOT-NAMES
			     collecting `(copy-slot ,SLOT-NAME)))))
       ,@BODY)))

;;;
;;;
;;;
(defmacro WITH-STACK-LIST-COPY ((variable list) &body body)
  "Like `((let ((,variable (copy-list ,list))) ,@body) 
   except that the copy is consed on the stack."
  `(let ((,variable (copy-list ,list))) ,@body))

;;; ***************************************************************************
;;; DUPLICATE

(defun duplicate-class-forms-copy (SYMBOL CLASS-NAME ALL-SLOTS SLOT-FORMS)
  (let ((WITH-SLOTS-SLOTS nil)
        (COPIED-SLOTS nil))
    (let ((SLOTS-FORMS (loop for (FORM-KIND FIRST-FORM . RST) in SLOT-FORMS
			     collecting (case FORM-KIND
					  (duplicate-set
                                           (push FIRST-FORM WITH-SLOTS-SLOTS)
                                           (push FIRST-FORM COPIED-SLOTS)
                                           `(copy-set-slot ,FIRST-FORM ,@RST))
					  (duplicate-slots
                                           (push FIRST-FORM COPIED-SLOTS)
                                           (setf COPIED-SLOTS (append RST COPIED-SLOTS))
                                           `(copy-slots ,FIRST-FORM ,@RST))
                                          (duplicate-cond-slots
                                           (push FIRST-FORM COPIED-SLOTS)
					    `(copy-cond-slot ,FIRST-FORM ,@RST))
					  (otherwise
					    (error "Unknown DUPLICATE Form-kind: ~S"
						   FORM-KIND))))))
      `(progn
         ,@(let ((it (set-difference ALL-SLOTS COPIED-SLOTS)))
             (when it
               `((defmethod copy-slots-slots-to-initialize append ((,SYMBOL ,CLASS-NAME))
                   '(,@it)))))
         (defmethod copy-inner-class progn ((,SYMBOL ,CLASS-NAME)
					    ..COPY-OBJECT.. ..COPY-HTABLE..)
           (declare (ignorable ..COPY-OBJECT.. ..COPY-HTABLE..))
	   (,@(if WITH-SLOTS-SLOTS
		`(with-slots ,WITH-SLOTS-SLOTS ,symbol
                   ,@with-slots-slots ; avoid unused errors
                   )
	        '(progn))
	    (with-slot-copying (..COPY-OBJECT.. ..COPY-HTABLE..)
	      ,@SLOTS-FORMS)))))))

(defun duplicate-class-forms-final-duplicate
       (SYMBOL CLASS-NAME FINAL-DUPLICATE-FORMS COPY-FORMS?)
  (when FINAL-DUPLICATE-FORMS
    (list `(defmethod final-duplicate-class progn ((,SYMBOL ,CLASS-NAME))
	     ,@FINAL-DUPLICATE-FORMS)
	  (and COPY-FORMS?
	       `(defmethod copy-final-class progn ((,SYMBOL ,CLASS-NAME))
		  (final-duplicate-class ,SYMBOL))))))

;;;
;;; 
;;;
(defvar *DUPLICATE-CLASS-FORMS-COPY-FORMS?* t)
(defmacro duplicator-methods
	  ((CLASS-NAME
	     &key
	     (symbol 'self)
	     (COPY-FORMS? *DUPLICATE-CLASS-FORMS-COPY-FORMS?*))
	   ALL-SLOTS SLOT-FORMS &optional FINAL-DUPLICATE-FORMS)
  
  `(progn
     (remove-duplicator-methods ',class-name)
     ,(and COPY-FORMS?
	   (duplicate-class-forms-copy symbol CLASS-NAME ALL-SLOTS SLOT-FORMS))
     ,@(duplicate-class-forms-final-duplicate
	 symbol CLASS-NAME FINAL-DUPLICATE-FORMS COPY-FORMS?)))

;;; ---------------------------------------------------------------------------
;;; removing methods
;;; ---------------------------------------------------------------------------

#|

duplicate-class-forms-copy
  copy-slots-slots-to-initialize append <class>
  copy-inner-class progn <class> object hash
duplicate-class-forms-final-duplicate
  final-duplicate-class progn <class>

|#

(defparameter *potential-duplicator-methods*
  '(copy-slots-slots-to-initialize
    copy-inner-class
    final-duplicate-class))

(defun remove-duplicator-methods (class-name)
  (mopu:remove-methods-if 
   class-name
   (lambda (gf m)
     (declare (ignorable gf m))
     (find (mopu:mopu-generic-function-name gf)
           *potential-duplicator-methods*))
   :verbose? nil))



;;; ---------------------------------------------------------------------------
;;; defcopy-methods
;;; ---------------------------------------------------------------------------

(defmacro defcopy-methods (class &key copy set copy-all set-all)
  "Put a different face on duplicator-methods's syntax."
  (let ((copy-slots nil)
        (set-slots nil))
    (when copy-all
      (assert (null copy))
      (assert (null set))
      (assert (null set-all))
      (setf copy (mopu:mopu-class-slot-names class)))
    (when set-all
      (assert (null copy))
      (assert (null set))
      (setf set (mopu:mopu-class-slot-names class)
            set-all nil))
    
    (setf set-slots (loop for slot in (ensure-list set) collect
                          (if (atom slot)
                            (list slot slot)
                            slot)))
    (setf copy-slots (ensure-list copy))
    
    ;;?? make sure that set and copy don't overlap
    
    `(progn
       (defmethod copy-self ((object ,class))
         (copy-object object))
       (duplicator-methods 
        (,class)
        ,(mopu:mopu-class-slot-names class)
        (,@(when copy-slots
             `((duplicate-slots ,@copy-slots)))
         ,@(when set-slots
             (loop for slot in set-slots collect
                   `(duplicate-set ,@slot))))))))


#|

(let ((x (make-list 10000)))
  (timeit (:report t) 
          (copy-top-level x))
  (let ((*copy-assume-no-circular-lists* t))
    (timeit (:report t) 
          (copy-top-level x))))

;;; 0.2160 cpu seconds (0.2160 cpu seconds ignoring GC)
;;; 431,920 words consed
;;; 0.0030 cpu seconds (0.0030 cpu seconds ignoring GC)
;;; 80,000 words consed
|#


;;; ---------------------------------------------------------------------------
;;; copy-template
;;; suppose you have an object
;;; ? (defclass foo ()
;;;     ((test :accessor test :initform #'equal :initarg :test))) =>
;;; #<STANDARD-CLASS FOO>
;;; 
;;; ? (setf *foo* (make-instance 'foo :test #'eql))
;;;
;;; ? (test *foo*) => #'eql 
;;; 
;;; Now you want to make another instance of foo that has the test as foo.
;;; 
;;; ? (setf *new-foo* (make-instance (type-of foo)))
;;;
;;; ? (test *new-foo*) => #'equal
;;;
;;; Wait, we wanted *new-foo* to have slot test to be #'eql.  This seems trival
;;; for simple objects, but consider this from make-filtered-graph
;;;
;;; (make-graph (type-of old-graph)
;;;              :vertex-test (vertex-test old-graph) 
;;;              :vertex-key (vertex-key old-graph)
;;;              :edge-test (edge-test old-graph)
;;;              :edge-key (edge-key old-graph)
;;;              :default-edge-type (default-edge-type old-graph)
;;;              :default-edge-class (default-edge-class old-graph)
;;;              :directed-edge-class (directed-edge-class old-graph)
;;;              :undirected-edge-class (undirected-edge-class old-graph))))
;;; Yuck!
;;; 
;;; So we offer copy-template as a temporary solution
;;; ---------------------------------------------------------------------------

(defmethod copy-template ((object standard-object))
  (apply 
   #'make-instance 
   (type-of object) 
   (loop 
     with result = nil 
     for (nil name nil initargs) in (mapcar (lambda (slot-value)
                                              (mopu-class-slot-information 
                                               object slot-value))
                                            (mopu-class-slot-names object)) do
     (when initargs
       (setf result (nconc result 
                           (list (if (listp initargs)
                                   (first initargs)
                                   initargs) (slot-value object name)))))
     finally (return result))))

(defmethod make-instance-from-object-initargs ((object standard-object))
  (apply 
   #'make-instance 
   (type-of object) 
   (loop 
     with result = nil 
     for (nil name nil initargs) in (mapcar (lambda (slot-value)
                                              (mopu-class-slot-information 
                                               object slot-value))
                                            (mopu-class-slot-names object)) do
     (when initargs
       (setf result (nconc result 
                           (list (if (listp initargs)
                                   (first initargs)
                                   initargs) (slot-value object name)))))
     finally (return result))))

;;;********************************************************************************
;;; EOF
