;;; -*- Syntax: Common-lisp; Package: metacopy -*-

(in-package #.(metacopy-system:metacopy-package))

#+with-contextl
(progn

  (defparameter *copy-protocols* nil "Holds a list of copy protocol names")

  (defun calculate-layer-name-from-protocol-name (name)
    (intern (concatenate 'string "%CPL-" (string name))))

  (defmacro define-copy-protocol (name &optional super-protocols &rest options)
    "Define a copy protocol, which directly maps to a ContextL layer."
    `(progn
      (pushnew ',name *copy-protocols*)
      (deflayer ,(calculate-layer-name-from-protocol-name name)
          ,(mapcar #'calculate-layer-name-from-protocol-name super-protocols)
        ,@options)
      (defun ,name (thing)
        (with-copy-protocol ,name
          (copy-thing thing)))))

  (defmacro define-copy-function (name args &rest options)
    "A defgeneric, with or without contextl."
    `(define-layered-function ,name ,args
      ,@options))

  (defmacro define-copy-method (name &rest body)
    "A defmethod, with or without contextl."
    (let ((protocol)
          (qualifiers)
          (args))
      (when (consp name)
        (assert (= (length name) 2))
        (setf protocol (calculate-layer-name-from-protocol-name (second name)))
        (setf name (first name)))
      (loop for el :in body
            until (consp el) do
            (push (pop body) qualifiers))
      (setf args (pop body))
      `(define-layered-method ,name ,@(when protocol `(:in-layer ,protocol)) ,@qualifiers ,args ,@body)))

  (defmacro with-copy-protocol (name &body body)
    (setf name (calculate-layer-name-from-protocol-name name))
    `(with-active-layers (,name)
      ,@body)))

#-with-contextl
(progn

  (defmacro define-copy-function (name args &rest options)
    "A defgeneric, without contextl."
    `(defgeneric ,name ,args
      ,@options))

  (defmacro define-copy-method (name args &rest options)
    "A defmethod, without contextl."
    (assert (not (consp name)) (name) "You can only define layered copy-methods when using the METACOPY-WITH-CONTEXTL package")
    `(defmethod ,name ,args
      ,@options)))

