(uiop:define-package #:docs-builder/builders/mgl-pax/guesser
  (:use #:cl)
  (:import-from #:docs-builder/guesser)
  (:import-from #:40ants-doc
                #:defsection
                #:section
                #:macro)
  (:import-from #:docs-builder/utils
                #:external-dependencies))
(in-package docs-builder/builders/mgl-pax/guesser)


(defsection @index (:title "MGL-PAX"
                    :ignore-words ("MGL-PAX:DEFSECTION"))
  "This guesser tries to find if your system depends on MGL-PAX system.
If it is, then the [MGL-PAX](https://github.com/melisgl/mgl-pax)
will be used to build documentation.

During the `build` phase, the builder will try to find MGL-PAX sections not refereced
from any other sections. For each root section, builder will create a separate HTML
page. If there are few root sections, make sure one of them is having \\@INDEX name.
Otherwise `index.html` page will not be created.

Algorithm searches section amongh all exported symbols. If you don't want it to find
some root section, just pass `:export nil` to the MGL-PAX:DEFSECTION macro.

If you want your documentation link back to the GitHub sources, make sure
you have either `:homepage` or `:source-control` in your ASDF definition:

```
(asdf:defsystem #:example
  :licence \"MIT\"
  :version \"0.0.3\"
  :author \"John Doe\"
  :mailto \"john@doe.me\"
  :homepage \"https://github.com/john-doe/example\"
  :source-control (:git \"https://github.com/john-doe/example\")
  ...)
```

*Note*, that this builder not only renders HTML documentation, but also updates
README files in the system's root directory.
")


(docs-builder/guesser:defguesser mgl-pax (system)
  (when (member "mgl-pax"
                (external-dependencies system)
                :test #'string-equal)
    (ql:quickload :docs-builder/builders/mgl-pax/builder
                  :silent t)
    (make-instance (intern "BUILDER" "DOCS-BUILDER/BUILDERS/MGL-PAX/BUILDER"))))
