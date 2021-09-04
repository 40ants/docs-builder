(defsystem "docs-config"
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "A protocol for defining configuration for DOCS-BUILDER system."
  :homepage "https://40ants.com/docs-builder"
  :bug-tracker "https://github.com/40ants/docs-builder/issues"
  :source-control (:git "https://github.com/40ants/docs-builder")
  :depends-on ("docs-config/config"))
