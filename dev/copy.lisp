;;; -*- Syntax: Common-lisp; Package: metacopy -*-

(in-package #.(metacopy-system:metacopy-package))

;;; ---------------------------------------------------------------------------

(defvar *copy-assume-no-circular-lists* nil)

;;; ---------------------------------------------------------------------------

(define-copy-method copy-thing (original-thing) 
  (copy-one original-thing (make-hash-table)))

;; TODO delme, this is not thread safe this way
#+nil(let ((copy-htable (make-hash-table)))
  (defmethod copy-thing (original-thing)
    (clrhash copy-htable)
    (copy-one original-thing copy-htable)))




;;;********************************************************************************
;;; Some simple cases.
;;; Copies these objects are always eq to the original and have no 
;;; internal structure.  -->  So just use the objects use themselves.
;;; (I.e. no need to worry about caching them).
;;;
;;; Gary King 2003-04-01: for speed, we do this for copy-one _and_ copy-thing
;;;
;;;********************************************************************************

(define-copy-method copy-thing ((original-thing symbol))
  original-thing)

(define-copy-method copy-one ((original-symbol symbol) copy-htable)
  (declare (ignore copy-htable))
  original-symbol)

(define-copy-method copy-thing ((original-thing number))
  original-thing)

(define-copy-method copy-one ((original-number number) copy-htable)
  (declare (ignore copy-htable))
  original-number)

(define-copy-method copy-thing ((original-thing function))
  original-thing)

(define-copy-method copy-one ((original-function function) copy-htable)
  (declare (ignore copy-htable))
  original-function)

(define-copy-method copy-thing ((original-thing character))
  original-thing)

(define-copy-method copy-one ((orig character) copy-htable)
  (declare (ignore copy-htable))
  orig)

#+mcl-when-you-want-to-know
(define-copy-method copy-one ((original-function ccl::compiled-lexical-closure) copy-htable)
  (declare (ignore copy-htable))
  (warn "copying ~a" original-function)
  original-function)

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


;;; So, in short there are three steps (if I've not already been Copied):
;;;  1] Create a new, empty copy  (using COPY-SELF).
;;;  2] Shove it in the HashTable.
;;;  3] Setup its internal structure, as needed (using COPY-INNER).
(define-copy-method copy-one (original-thing copy-htable)
  (multiple-value-bind (value found?) (gethash original-thing copy-htable)
    (or (and found? value)
	(let ((copy-thing (copy-self original-thing)))
          (setf (gethash original-thing copy-htable) copy-thing)
	  (copy-inner original-thing copy-thing copy-htable)
	  (copy-final original-thing copy-thing)
	  copy-thing))))

;;; ---------------------------------------------------------------------------

;; copies without worrying about shared structure
(define-copy-method copy-one (original-thing (copy-htable (eql nil)))
  (let ((copy-thing (copy-self original-thing)))
    (copy-inner original-thing copy-thing copy-htable)
    (copy-final original-thing copy-thing)
    copy-thing))

;;; ---------------------------------------------------------------------------

(define-copy-method copy-self (self)
  (error "don't know how to copy ~s" self))

;;; ---------------------------------------------------------------------------

(define-copy-method copy-inner (self copy-object copy-htable)
  "default is to do nothing."
  (declare (ignore self copy-object copy-htable))
  nil)

;;; ---------------------------------------------------------------------------

(define-copy-method copy-final ((self t) (copy t))
  "Default is to do nothing."
  nil)

;;; ---------------------------------------------------------------------------
;;; Strings

(define-copy-method copy-self ((original string))
  (subseq original 0 (length original)))

;; define this so that array code below does not run for strings.
(define-copy-method copy-inner ((original string) new-array copy-htable)
  (declare (ignore new-array copy-htable)))

;;; ---------------------------------------------------------------------------
;;; arrays

(define-copy-method copy-self ((original array))
  (let* ((array-dimensions (array-dimensions original))
         (adjustable (adjustable-array-p original))
         (type (array-element-type original))
         (fp (when (array-has-fill-pointer-p original)
               (fill-pointer original))))
    (make-array array-dimensions
                :element-type type
                :adjustable adjustable
                :fill-pointer fp)))

(define-copy-method copy-inner ((original array) new-array copy-htable)
  (loop for index from 0 below (apply #'* (array-dimensions original)) do
        (setf (row-major-aref new-array index) 
              (copy-one (row-major-aref original index) copy-htable))))

;;; ---------------------------------------------------------------------------
;;; hash tables

(define-copy-method copy-self ((original hash-table))
  (make-hash-table :test (hash-table-test original)))

(define-copy-method copy-inner ((original-table hash-table) new-table copy-htable)
  (maphash #'(lambda (key value)
               (setf (gethash 
                      (copy-one key copy-htable)
                      new-table)
                     (copy-one value copy-htable)))
           original-table))

;;;********************************************************************************
;;; Lists
;;;********************************************************************************

(define-copy-method copy-self ((original-list list))
  (and original-list (cons nil nil)))

(define-copy-method copy-inner ((original-list list) copy-list copy-htable)
  ;; this handles circular lists, but is slower and isn't cdr coded.
  (flet ((dotted-pair-p (putative-pair)
           (and (consp putative-pair)
                (cdr putative-pair)
                (not (consp (cdr putative-pair))))))
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
             (setf (cdr copy-list) (copy-one (cdr original-list) copy-htable)))))))

;;;********************************************************************************
;;; copy-able class objects.
;;;********************************************************************************


(defclass copyable-mixin ()
    ()
  (:documentation
    "provides method for doing copy that creates a copy on an object.
     each mixin should provide an copy-inner-class method to copy its
     slots appropriately."))

(defvar *instance-for-copy?* nil)

(define-copy-method copy-self ((self copyable-mixin))
  (copy-object self))

(defun copy-object (self)
  (let ((i (allocate-instance (class-of self))))
    (let ((*instance-for-copy?* i))
      (shared-initialize i (copy-slots-slots-to-initialize self)))))

(defmethod instance-made-for-copying-p ((object t))
  (eq object *instance-for-copy?*))

(define-copy-method copy-inner-class progn ((original-object copyable-mixin) copy-list copy-htable)
  (declare (ignore copy-list copy-htable))
  nil)

(define-copy-method copy-inner ((original-object standard-object) copy-list copy-htable)
  (copy-inner-class original-object copy-list copy-htable))

(define-copy-method copy-final-class progn ((original-object copyable-mixin) (copy t))
  nil)

(define-copy-method copy-final ((original-object copyable-mixin) (copy t))
  (copy-final-class original-object copy))

;;;********************************************************************************
;;; Things to make using COPY-INNER-CLASS easier.
;;;********************************************************************************

(defun copy-set-slot-1 (copy-object slot-name value)
  (setf (slot-value copy-object slot-name)
	 value))

;; Maybe make this deal with unbound slots - could just ignore them which would make
;; the slot unbound in the copy as well. - Westy
;; added the slot-boundp check, GWK 20011022
(defun copy-slot-1 (copy-object slot-name original-object copy-htable)
  (when (slot-boundp original-object slot-name)
    (copy-set-slot-1 copy-object slot-name
                     (copy-one (slot-value original-object slot-name)
                               copy-htable))))

;;; (copy-set-slot (SLOT-NAME VALUE)
;;;   Set the contents of SLOT-NAME in COPY-OBJECT to VALUE.
;;; (copy-slot (SLOT-NAME) ...
;;;   Set the contents of SLOT-NAME in COPY-OBJECT to be a copyicate of the
;;;   contents of the same slot in ORIGINAL-OBJECT.
(defmacro with-slot-copying
	  ((copy-object copy-htable &optional (original-object 'self)) &body body)
  `(macrolet ((copy-slot (slot-name)
		`(copy-slot-1 ,',copy-object ',slot-name ,',original-object ,',copy-htable))
              (copy-cond-slot (slot-name test-form)
		`(when ,test-form
                   (copy-slot-1 ,',copy-object ',slot-name ,',original-object ,',copy-htable)))
	      (copy-set-slot (slot-name value)
		`(copy-set-slot-1 ,',copy-object ',slot-name ,value)))
     (macrolet ((copy-slots (&rest slot-names)
		  `(progn
		     ,@(loop for slot-name in slot-names
			     collecting `(copy-slot ,slot-name)))))
       ,@body)))


;;; ***************************************************************************
;;; DUPLICATE

(defun duplicate-class-forms-copy (symbol class-name all-slots slot-forms)
  (let ((with-slots-slots nil)
        (copied-slots nil))
    (let ((slots-forms (loop for (form-kind first-form . rst) in slot-forms
			     collecting (case form-kind
					  (duplicate-set
                                           ;; sanity check, make sure we have something 
                                           ;; to duplicate
                                           (when first-form
                                             (push first-form with-slots-slots)
                                             (push first-form copied-slots)
                                             `(copy-set-slot ,first-form ,@rst)))
					  (duplicate-slots
                                           (push first-form copied-slots)
                                           (setf copied-slots (append rst copied-slots))
                                           `(copy-slots ,first-form ,@rst))
                                          (duplicate-cond-slots
                                           (push first-form copied-slots)
					    `(copy-cond-slot ,first-form ,@rst))
					  (otherwise
					    (error "unknown duplicate form-kind: ~s"
						   form-kind))))))
      `(progn
         ,@(let ((it (set-difference all-slots copied-slots)))
             (when it
               `((defmethod copy-slots-slots-to-initialize append ((,symbol ,class-name))
                   '(,@it)))))
         (defmethod copy-inner-class progn ((,symbol ,class-name)
					    ..copy-object.. ..copy-htable..)
           (declare (ignorable ..copy-object.. ..copy-htable..))
	   (,@(if with-slots-slots
		`(with-slots ,with-slots-slots ,symbol
                   ,@with-slots-slots ; avoid unused errors
                   )
	        '(progn))
	    (with-slot-copying (..copy-object.. ..copy-htable..)
	      ,@slots-forms)))))))

(defun duplicate-class-forms-final-duplicate
       (symbol class-name final-duplicate-forms copy-forms?)
  (when final-duplicate-forms
    (list `(defmethod final-duplicate-class progn ((,symbol ,class-name))
	     ,@final-duplicate-forms)
	  (and copy-forms?
	       `(defmethod copy-final-class progn ((,symbol ,class-name))
		  (final-duplicate-class ,symbol))))))

;;;
;;; 
;;;
(defvar *duplicate-class-forms-copy-forms?* t)
(defmacro duplicator-methods
	  ((class-name
	     &key
	     (symbol 'self)
	     (copy-forms? *duplicate-class-forms-copy-forms?*))
	   all-slots slot-forms &optional final-duplicate-forms)
  
  `(progn
     (remove-duplicator-methods ',class-name)
     ,(and copy-forms?
	   (duplicate-class-forms-copy symbol class-name all-slots slot-forms))
     ,@(duplicate-class-forms-final-duplicate
	 symbol class-name final-duplicate-forms copy-forms?)))

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
     (find (generic-function-name gf)
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
      (setf copy (slot-names class)))
    (when set-all
      (assert (null copy))
      (assert (null set))
      (setf set (slot-names class)
            set-all nil))
    
    (setf set-slots (loop for slot in (if (consp set) set (list set)) collect
                          (if (atom slot)
                            (list slot slot)
                            slot)))
    (setf copy-slots (if (consp copy) copy (list copy)))
    
    ;;?? make sure that set and copy don't overlap
    
    `(progn
       (define-copy-method copy-self ((object ,class))
         (copy-object object))
       (duplicator-methods 
        (,class)
        ,(slot-names class)
        (,@(when copy-slots
             `((duplicate-slots ,@copy-slots)))
         ,@(when set-slots
             (loop for slot in set-slots collect
                   `(duplicate-set ,@slot))))))))


#|

(let ((x (make-list 10000)))
  (timeit (:report t) 
          (copy-thing x))
  (let ((*copy-assume-no-circular-lists* t))
    (timeit (:report t) 
          (copy-thing x))))

;;; 0.2160 cpu seconds (0.2160 cpu seconds ignoring GC)
;;; 431,920 words consed
;;; 0.0030 cpu seconds (0.0030 cpu seconds ignoring GC)
;;; 80,000 words consed
|#

