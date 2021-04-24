(defsystem "docs-builder" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "A meta documentation builder for Common Lisp projects."
  :long-description
    "This system is a generic documentation builder for Common Lisp Systems.
It able to generate HTML documentation for specified ASDF system.

The idea is to use `docs-builder` as an universal HTML documentation builders
which can be used in a continuous integration pipeline. For example, it is
used inside [build-docs](https://40ants.com/build-docs) GitHub action, which can be
used to build docs update gh-pages for any Common Lisp library (if it is uses
documentation generator supported by `docs-builder`).

Currently Docs Builder supports only [MGL-PAX](https://github.com/melisgl/mgl-pax)
can be extended to support other documentation builders, covered by examples in here:
[cl-doc-systems.github.io](https://cl-doc-systems.github.io/)."
  :homepage "https://40ants.com/docs-builder"
  :bug-tracker "https://github.com/40ants/docs-builder/issues"
  :source-control (:git "https://github.com/40ants/docs-builder")
  :depends-on ("docs-builder/core"
               "docs-builder/builders/40ants-doc/guesser"
               "docs-builder/builders/mgl-pax/guesser"
               "docs-builder/builders/geneva/guesser"
               "docs-builder/docs"))


(register-system-packages "geneva-html" '(#:geneva.html))
(register-system-packages "geneva-mk2" '(#:geneva.mk2))
(register-system-packages "geneva-cl" '(#:geneva.cl))
