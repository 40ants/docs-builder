(uiop:define-package #:docs-builder/builders/40ants-doc/guesser
  (:use #:cl)
  (:import-from #:docs-builder/guesser)
  (:import-from #:40ants-doc
                #:defsection
                #:section
                #:macro)
  (:import-from #:docs-builder/utils
                #:external-dependencies))
(in-package docs-builder/builders/40ants-doc/guesser)


(defsection @index (:title "40ANTS-DOC")
  "This guesser tries to find if your system depends on 40ANTS-DOC system.
If it is, then the [40ANTS-DOC](https://github.com/40ants/doc)
will be used to build documentation.

During the `BUILD` phase, the builder will try to find documentation sections not refereced
from any other sections. For each root section, builder will create a separate HTML
page. If there are few root sections, make sure one of them is having \\@INDEX name.
Otherwise `index.html` page will not be created.

Algorithm searches section amongh all exported symbols. If you don't want it to find
some root section, just pass `:export nil` to the 40ANTS-DOC:DEFSECTION.

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
"
  
  (@todo section))


(defsection @todo (:title "What is next")
  "
- build a ChangeLog.md out of changelog.lisp, if it is exists")


(docs-builder/guesser:defguesser 40ants-doc (system)
  (when (or (member "40ants-doc"
                    (external-dependencies system)
                    :test #'string-equal)
            ;; Documentation for 40ANTS-DOC is written
            ;; in 40ANTS-DOC :)))
            (string-equal (asdf:component-name system)
                          "40ants-doc"))
    (ql:quickload :docs-builder/builders/40ants-doc/builder
                  :silent t)
    (make-instance (intern "BUILDER" "DOCS-BUILDER/BUILDERS/40ANTS-DOC/BUILDER"))))
