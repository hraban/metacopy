(in-package #.(metacopy-system:metacopy-package))

;;; ---------------------------------------------------------------------------

(define-copy-function copy-thing (thing)
  (:documentation "Copy objects with aribtrarily complex substructure.
Objects are kept track of in a HashTable, so only one copy is made of each.
Things which are EQ in the original (i.e. objects, sublists, etc.) come out
EQ in the corresponding places in the copy."))

;;; ---------------------------------------------------------------------------

(define-copy-function copy-one (self copy-htable)
  (:documentation "returns a fullfledged copy of self, set-up and ready to go."))

;;; ---------------------------------------------------------------------------

(define-copy-function copy-self (self)
  (:documentation "return a new, empty version of self"))

;;; ---------------------------------------------------------------------------

(define-copy-function copy-inner (self copy-object copy-htable)
  (:documentation
    "copy the relevant portions of self into copy-object.
     ok if it calls copy on sub-objects."))

;;; ---------------------------------------------------------------------------

(define-copy-function copy-final (self copy)
  (:documentation "Last pass to make sure everything is in place."))

(define-copy-function copy-inner-class (self copy-object copy-htable)
  (:method-combination progn :most-specific-last)
  (:documentation
    "Defined for each component class of an object with mixin COPYABLE-MIXIN.
     It should setup its slots as appropriate.
     This needs to be a seperate method (from COPY-INNER) because it has
     to be done with a PROGN Method-Combination."))

(define-copy-function copy-slots-slots-to-initialize (self)
  (:method-combination append :most-specific-last)
  (:method  append ((self standard-object))
            (values nil)))

(define-copy-function copy-final-class (self copy)
  (:method-combination progn)
  (:documentation
    "defined for each component class of an object with mixin copyable-mixin.
     it should setup its slots as appropriate.
     this needs to be a seperate method (from copy-final) because it has
     to be done with a progn method-combination."))