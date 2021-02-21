(mgl-pax-minimal:define-package #:docs-builder/builders/mgl-pax/guesser
  (:use #:cl)
  (:import-from #:docs-builder/guesser)
  (:import-from #:docs-builder/utils)
  (:import-from #:mgl-pax-minimal
                #:defsection
                #:section
                #:macro))
(in-package docs-builder/builders/mgl-pax/guesser)


(defsection @index (:title "MGL-PAX")
  "This guesser tries to find if your system depends on MGL-PAX-MINIMAL system.
If it is, then the [MGL-PAX](https://github.com/melisgl/mgl-pax)
will be used to build documentation.

During the `BUILD` phase, the builder will try to find `THE-PACKAGE:@INDEX` symbol in a
package with the same name as the system's name. It should be a section, defined
with MGL-PAX-MINIMAL:DEFSECTION macro.

*Note*, that this builder not only renders HTML documentation, but also updates
README files in the system's root directory."
  
  (@todo section))


(defsection @todo (:title "What is next")
  "
- collect all sections from the system's packages and build pages
  for all root sections.
- build a ChangeLog.md out of changelog.lisp, if it is exists")


(docs-builder/guesser:defguesser mgl-pax (system)
  (when (member "mgl-pax-minimal"
                (docs-builder/utils:external-dependencies system)
                :test #'string-equal)
    (ql:quickload :docs-builder/builders/mgl-pax/builder
                  :silent t)
    (make-instance (intern "BUILDER" "DOCS-BUILDER/BUILDERS/MGL-PAX/BUILDER"))))
