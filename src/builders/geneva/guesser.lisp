(uiop:define-package #:docs-builder/builders/geneva/guesser
  (:use #:cl)
  (:import-from #:docs-builder/guesser)
  (:import-from #:40ants-doc
                #:defsection
                #:section
                #:macro))
(in-package docs-builder/builders/geneva/guesser)


(defsection @index (:title "Geneva")
  "This guesser tries to find a file `docs/sources/index.mk2` and if it exists
then [Geneva](https://github.com/eugeneia/geneva) documentation generator will be used.
"
  (@todo section))


(defsection @todo (:title "What is next")
  "
- make builder to process all `*.mk2` files in the `docs/sources/` dir.
- build API reference pages for all packages created by the system.")


(docs-builder/guesser:defguesser geneva (system)
  (when (probe-file
         (asdf:system-relative-pathname system
                                        "docs/source/index.mk2"))
    (uiop:symbol-call :ql :quickload :docs-builder/builders/geneva/builder
                          :silent t)
    (make-instance (intern "BUILDER" "DOCS-BUILDER/BUILDERS/GENEVA/BUILDER"))))
