(uiop:define-package #:docs-builder/guesser
  (:use #:cl)
  (:export #:guess-builder
           #:defguesser))
(in-package docs-builder/guesser)


(defvar *guessers* nil)

(defparameter *system-to-builder*
  (make-hash-table))


(defgeneric guess-builder (system)
  (:documentation "Returns a builder object which can be passed to the DOCS-BUILDER/BUILDER:BUILD generic-function along with system.

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
  (or (let ((builder (gethash system *system-to-builder*)))
        (log:info "Using builder" builder)
        builder)
      (progn
        (log:info "Guessing builder")

        (setf (gethash system *system-to-builder*)
              (loop for guesser in *guessers*
                    thereis (progn
                              (log:info "Running guesser" guesser)
                              (funcall guesser
                                       system))
                    finally (error "Unable to create documentation builder for system ~S"
                                   system))))))


