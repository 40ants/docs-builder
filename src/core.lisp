(defpackage #:docs-builder
  (:use #:cl)
  (:nicknames #:docs-builder/core)
  (:import-from #:docs-builder/builder)
  (:import-from #:docs-builder/guesser)
  (:import-from #:log4cl)
  (:export #:build
           #:@index))
(in-package docs-builder)


(define-condition documentation-has-problems (error)
  ((num-of-warnings :initarg :num-of-warnings
                    :reader num-of-warnings))
  (:report (lambda (c stream)
             (format stream
                     "There was ~A warnings during documentation build."
                     (num-of-warnings c)))))


(defun build (system &key (error-on-warnings t))
  (let ((builder (docs-builder/guesser:guess-builder system)))
    (unless builder
      (error "Unable to guess documentation builder for ASDF system ~S"
             system))
    (let ((warnings 0))
      (handler-bind ((warning (lambda (c)
                                (declare (ignore c))
                                (incf warnings))))
        (docs-builder/builder:build builder system))
      (when (and error-on-warnings
                 (not (zerop warnings)))
        (cerror "Ignore Warnings"
                'documentation-has-problems
                :num-of-warnings warnings)))))
