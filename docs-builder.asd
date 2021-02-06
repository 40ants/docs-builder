(defsystem "docs-builder" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description ""
  :defsystem-depends-on ("mgl-pax-minimal")
  :depends-on ("docs-builder/core"
               "docs-builder/builders/mgl-pax/guesser"))
