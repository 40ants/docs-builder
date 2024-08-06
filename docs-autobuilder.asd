(defsystem "docs-autobuilder" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "A wrapper which watches on filesystem and runs DOCS-BUILDER automatically."
  :homepage "https://40ants.com/docs-builder"
  :bug-tracker "https://github.com/40ants/docs-builder/issues"
  :source-control (:git "https://github.com/40ants/docs-builder")
  :depends-on ("docs-autobuilder/autobuilder"))


(asdf:register-system-packages "lack-app-file" '("LACK.APP.FILE"))
(asdf:register-system-packages "lack" '("LACK.COMPONENT"))
