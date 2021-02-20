(mgl-pax-minimal:define-package #:docs-builder
  (:use #:cl)
  (:nicknames #:docs-builder/api)
  (:import-from #:mgl-pax-minimal
                #:defsection
                #:section)
  (:export #:make-builder
           #:build
           #:def-docbuilder-guesser))
(in-package docs-builder/api)


(defsection @index (:title "Docs Builder")
  "
This system is a generic documentation builder for Common Lisp Systems.
It able to generate HTML documentation for specified ASDF system.

The idea is to use `docs-builder` as an universal HTML documentation builders
which can be used in a continuous integration pipeline. For example, it is
used inside [build-docs](https://40ants.com/build-docs) GitHub action, which can be
used to build docs update gh-pages for any Common Lisp library (if it is uses
documentation generator supported by `docs-builder`).

Currently Docs Builder supports only [MGL-PAX](https://github.com/melisgl/mgl-pax)
can be extended to support other documentation builders, covered by examples in here:
[cl-doc-systems.github.io](https://cl-doc-systems.github.io/)."

  (@usage section)
  (@roadmap section))


(defsection @usage (:title "Usage")
  "Documentation can be built in a few ways: from the lisp REPL, command-line and
using the [GitHub action](https://40ants.com/build-docs)."

  (@repl-usage section)
  (@command-line-usage section)
  (@github-action-usage section))


(defsection @repl-usage (:title "REPL")
  "From the REPL, you need first to create a builder for the system using this generic function:"

  (make-builder generic-function)

  "
Then you need to pass the object returned by MAKE-BUILDER to the BUILD function:"

  (make-builder generic-function)

  "Here is an example how to build documentation for `:docs-builder` ASDF system:

```lisp
CL-USER> (docs-builder:make-builder :docs-builder)
#<DOCS-BUILDER/BUILDERS/MGL-PAX/BUILDER::BUILDER {1006652DB3}>

CL-USER> (docs-builder:build * :docs-builder)
 <INFO> [02:12:00] docs-builder/core core.lisp (build :before system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM \"docs-builder\"> found at /Users/art/projects/docs-builder/
 <INFO> [02:12:00] docs-builder/builders/mgl-pax/builder builder.lisp (build builder system) -
  Building docs in \"/Users/art/projects/docs-builder/docs/build/\" dir
#P\"/Users/art/projects/docs-builder/docs/build/\"
```")


(defsection @command-line-usage (:title "Command-line")
  "You can use builder from command-line. To do this, first install it using [Roswell](https://github.com/roswell/roswell):

```bash
# Note, we need to install this patched mgl-pax
# first, to be able to load docs-builder.
# This step in necessary until this pull
# will be merged:
# https://github.com/melisgl/mgl-pax/pull/8

$ ros install svetlyak40wt/mgl-pax

$ ros install 40ants/docs-builder
```

Here we call it to build documentation for \"docs-builder\" ASDF system:

$ build-docs docs-builder
 <INFO> [02:26:32] docs-builder/main main.lisp (main) -
  Quickloading system \"docs-builder\"
 <INFO> [02:26:34] docs-builder/core core.lisp (build :before system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM \"docs-builder\"> found at /Users/art/projects/docs-builder/
 <INFO> [02:26:34] docs-builder/builders/mgl-pax/builder builder.lisp (build builder system) -
  Building docs in \"/Users/art/projects/docs-builder/docs/build/\" dir
Scan was called 2146 times.

")


(defsection @github-action-usage (:title "GitHub Action")
  "If you host your project on the GitHub, then the most easy way to build and host documentation
would be to use [Github Pages](https://pages.github.com/).

To build docs and update the site, create a file `.github/workflows/docs.yml` with a content like this:

```yaml
name: 'Docs'

on:
  # This will run tests on pushes
  # to master branch and every monday:
  push:
    branches:
      - 'main'
      - 'master'
  schedule:
    - cron:  '0 10 * * 1'

jobs:
  build-docs:
    runs-on: ubuntu-latest
    
    env:
      LISP: sbcl-bin

    steps:
      - uses: actions/checkout@v1
      - uses: 40ants/setup-lisp@v1
        with:
          asdf-system: cl-info
          qlfile-template: |
            github mgl-pax svetlyak40wt/mgl-pax :branch mgl-pax-minimal
      
      - uses: 40ants/build-docs@v1
        with:
          asdf-system: cl-info
```

You'll find more info in [the action's documentation](https://40ants.com/build-docs).
")


(defsection @roadmap (:title "Roadmap")
  "
* Use [eazy-documentation](https://guicho271828.github.io/eazy-documentation/) as default fallback
  when no other builder was guessed.
* Support other documentation generators, collected at https://cl-doc-systems.github.io/
* Add ability to put a configuration file into the reporitory, for fine-tunning the builder.")



(defvar *builder-factories* nil)


(defgeneric make-builder (system)
  (:documentation "Returns a builder object which can be passed to the BUILD method along with system.

The builder type is guessed using different euristics which depends on a documentation system.

If you want to add support for a new documentation generator, use DEF-DOCBUILDER-GUESSER macro."))


(defgeneric build (builder system)
  (:documentation "Builds HTML documentation for ASDF system and returns absolute path to the dir with docs."))


(defmacro def-docbuilder-guesser (name (system) &body body)
  `(progn
     (defun ,name (,system)
       ,@body)
     (pushnew ',name *builder-factories*)))
