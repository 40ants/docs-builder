(defpackage #:docs-builder/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package docs-builder/changelog)


(defchangelog (:ignore-words ("HTML"
                              "MGL-PAX"
                              "ERROR-ON-WARNINGS") )
  (0.7.1 2021-09-11
         "Fixed building documentation with 40ANTS-DOC if changelog section is absent.")
  (0.7.0 2021-09-04
         "Added DOCS-CONFIG:DOCS-CONFIG generic-function to allow define
          a special config for documented systems.")
  (0.6.2 2021-08-31
         "Speedup documentation builder guessing.")
  (0.6.1 2021-08-28
         "Fixed argument passing to the builder. Previously
          error `Unknown &KEY argument: :ERROR-ON-WARNINGS` has happened. ")
  (0.6.0 2021-08-28
         "Support new refactored 40ANTS-DOC system.")
  (0.5.3 2021-05-08
         "Roswell script was fixed to work with latest DEFMAIN system.")
  (0.5.2 2021-04-25
         "Fixed the case when 40ANTS-DOC or MGL-PAX
          builder found the same section twice. This could
          happen for a package-inferred system where
          some package has the same nickname as as a
          primary system.")
  (0.5.1 2021-04-16
         "Fixed returning of the path to resulting docs.")
  (0.5.0 2021-04-15
         "Now DOCS-BUILDER:BUILD has a special argument ERROR-ON-WARNINGS
          which is T by default. This flag causes a DOCS-BUILDER:DOCUMENTATION-HAS-PROBLEMS
          continuable error signaled in case if there were some warnings
          during building the documentation.")
  (0.4.2 2021-04-05
         "Fixed dependency on `log4cl-extras`.")
  (0.4.1 2021-04-05
         "Now utility will dump traceback
          in case of errors during the build.")
  (0.4.0 2021-04-05
         "* Added support for 40ANTS-DOC documentation builder.
          * Switched MGL-PAX builder from my fork to original.")
  (0.3.0 2021-02-22
         "Now MGL-PAX builder is able to discover all root sections and to build
          a multipage HTML doc.")
  (0.2.0 2021-02-21
         "Added basic support for Geneva documentation generator.")
  (0.1.0 2021-02-04
         "Initial version."))
