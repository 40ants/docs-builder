(defpackage #:docs-builder
  (:use #:cl)
  (:nicknames #:docs-builder/core)
  (:import-from #:docs-builder/builder)
  (:import-from #:docs-builder/guesser)
  (:import-from #:log4cl)
  (:export #:build))
(in-package docs-builder)


(defun build (system)
  (let ((builder (docs-builder/guesser:guess-builder system)))
    (unless builder
      (error "Unable to guess documentation builder for ASDF system ~S"
             system))
    (docs-builder/builder:build builder system)))
