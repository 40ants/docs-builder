(defsystem "docs-builder" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "A meta documentation builder for Common Lisp projects."
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
