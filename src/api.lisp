(mgl-pax-minimal:define-package #:docs-builder
  (:use #:cl)
  (:nicknames #:docs-builder/api)
  (:export #:make-builder
           #:build
           #:def-docbuilder-guesser))
(in-package docs-builder/api)


(defvar *builder-factories* nil)


(defgeneric make-builder (system)
  (:documentation "Returns a object which can be passed to the BUILD method along with system."))


(defgeneric build (builder system)
  (:documentation "Builds documentation of a system and returns absolute path to the dir with docs."))


(defmacro def-docbuilder-guesser (name (system) &body body)
  `(progn
     (defun ,name (,system)
       ,@body)
     (pushnew ',name *builder-factories*)))
