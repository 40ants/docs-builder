(defpackage #:docs-builder/builders/geneva/guesser
  (:use #:cl)
  (:import-from #:docs-builder/api))
(in-package docs-builder/builders/geneva/guesser)


(docs-builder/api:def-docbuilder-guesser geneva (system)
  (when (probe-file
         (asdf:system-relative-pathname system
                                        "docs/source/index.mk2"))
    (ql:quickload :docs-builder/builders/geneva/builder
                  :silent t)
    (make-instance (intern "BUILDER" "DOCS-BUILDER/BUILDERS/GENEVA/BUILDER"))))
