(defsystem "docs-builder" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description ""
  :depends-on ("docs-builder/core"
               "docs-builder/builders/40ants-doc/guesser"
               "docs-builder/builders/mgl-pax/guesser"
               "docs-builder/builders/geneva/guesser"
               "docs-builder/docs"))


(register-system-packages "geneva-html" '(#:geneva.html))
(register-system-packages "geneva-mk2" '(#:geneva.mk2))
(register-system-packages "geneva-cl" '(#:geneva.cl))
