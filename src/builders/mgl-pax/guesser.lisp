(defpackage #:docs-builder/builders/mgl-pax/guesser
  (:use #:cl)
  (:import-from #:docs-builder/api)
  (:import-from #:docs-builder/utils))
(in-package docs-builder/builders/mgl-pax/guesser)


(docs-builder/api:def-docbuilder-guesser mgl-pax (system)
  (when (member "mgl-pax-minimal"
                (docs-builder/utils:external-dependencies system)
                :test #'string-equal)
    (ql:quickload :docs-builder/builders/mgl-pax/builder
                  :silent t)
    (make-instance (intern "BUILDER" "DOCS-BUILDER/BUILDERS/MGL-PAX/BUILDER"))))
