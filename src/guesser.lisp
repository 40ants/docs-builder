(mgl-pax-minimal:define-package #:docs-builder/guesser
  (:use #:cl)
  (:export #:guess-builder
           #:defguesser))
(in-package docs-builder/guesser)


(defvar *guessers* nil)


(defgeneric guess-builder (system)
  (:documentation "Returns a builder object which can be passed to the BUILD method along with system.

The builder type is guessed using different euristics which depends on a documentation system.

If you want to add support for a new documentation generator, use DEFGUESSER macro."))


(defmacro defguesser (name (system) &body body)
  `(progn
     (defun ,name (,system)
       ,@body)
     (pushnew ',name *guessers*)))


(defmethod guess-builder ((system symbol))
  (guess-builder (asdf:find-system system)))


(defmethod guess-builder ((system string))
  (guess-builder (asdf:find-system system)))


(defmethod guess-builder ((system asdf:system))
  (loop for guesser in *guessers*
          thereis (funcall guesser
                           system)
        finally (error "Unable to create documentation builder for system ~S"
                       system)))


