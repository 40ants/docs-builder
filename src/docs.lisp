(mgl-pax-minimal:define-package #:docs-builder/docs
  (:use #:cl)
  (:import-from #:mgl-pax-minimal
                #:defsection
                #:section
                #:macro)
  (:import-from #:docs-builder/builders/mgl-pax/guesser)
  (:import-from #:docs-builder/builders/geneva/guesser)
  (:import-from #:docs-builder/utils))
(in-package docs-builder/docs)


(defsection docs-builder:@index (:title "Common Lisp Docs Builder")
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
  (@supported-builders section)
  (docs-builder/utils:@utils section)
  (@roadmap section))


(defsection @usage (:title "Usage")
  "Documentation can be built in a few ways: from the lisp REPL, command-line and
using the [GitHub action](https://40ants.com/build-docs)."

  (@repl-usage section)
  (@command-line-usage section)
  (@github-action-usage section)
  (@extending section))


(defsection @repl-usage (:title "REPL")
  "From the REPL, you need first to call a BUILD fuction:"
  
  (docs-builder:build function)

  "Inside, it will try to guess which documentation builder should be used:"
  
  (docs-builder/guesser:guess-builder generic-function)

  "
Then it will pass the builder object and ASDF system to the DOCS-BUILDER/BUILDER:BUILD function:"

  (docs-builder/builder:build generic-function)

  "Here is an example how to build documentation for `:docs-builder` ASDF system:

```lisp
CL-USER> (docs-builder:build :docs-builder)
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


(defsection @extending (:title "Extending")
  "
Docs builder was made to be extensible and here we see how to add support for a new
documentation generator. A documentation builder we'll use is a [Geneva](https://inters.co/geneva/open-geneva.html).

Some time ago I've wrote [a template system](https://github.com/cl-doc-systems/geneva) to demonstrate how to
document ASDF system using Geneva. Lets try to automate docs building and add Geneva support to the DOCS-BUILDER!

First, we need a way to detect if the system uses the Geneva. This is done using \"guesser\".
Let's define a guesser which will consider we are working with Geneva, if there is a `docs/source/index.mk2`
file. Files with `*.mk2` extensions are special markup used by Geneva.
"
  (@adding-a-builder-class section)
  (@adding-a-guesser section)
  (@adding-a-build-method section))


(defsection @adding-a-builder-class (:title "Add a Builder Class")
  "
First thing we need to do is to create a builder class.

Create a `src/builders/geneva/builder.lisp` file with
following content:

```
(defclass builder ()
  ())
```
")


(defsection @adding-a-guesser (:title "Guessing a Doc Generator")
  "
DOCS-BUILDER uses a number of euristics to determine the CLOS class
to be used for documentation generation. Euristics are incapulated in
\"guessers\". Let's create a `src/builders/geneva/guesser.lisp` file
and define a simple guesser, which will return the `builder` class defined
in the previous section if a file `docs/sources/index.mk2` exists.

To define a guesser, we'll be using DOCS-BUILDER/GUESSER:DEFGUESSER macro:

"
  (docs-builder/guesser:defguesser macro)

  "
```
(docs-builder/guesser:defguesser geneva (system)
  (when (probe-file
         (asdf:system-relative-pathname system
                                        \"docs/source/index.mk2\"))
    (ql:quickload :docs-builder/builders/geneva/builder
                  :silent t)
    (make-instance (intern \"BUILDER\" \"DOCS-BUILDER/BUILDERS/GENEVA/BUILDER\"))))
```

When all rules matched, guesser loads the builder package and all its dependencies.
In our case, `Geneva` system will be loaded.

Add this guesser file to the `docs-builder.asd` file:

```
(defsystem \"docs-builder\" 
  :class :package-inferred-system
  :author \"Alexander Artemenko\"
  :license \"Unlicense\"
  :pathname \"src\"
  :description \"\"
  :defsystem-depends-on (\"mgl-pax-minimal\")
  :depends-on (\"docs-builder/core\"
               \"docs-builder/builders/mgl-pax/guesser\"
               \"docs-builder/builders/geneva/guesser\"))
```

This way, it will be loaded along with the primary system while
`geneva/builder` and it's dependencies will be loaded only
if the system we are building documentation for is using Geneva.

Now we can call MAKE-BUILDER to create a builder for example
system:

```
CL-USER> (docs-builder:make-builder :example)
#<DOCS-BUILDER/BUILDERS/GENEVA/BUILDER::BUILDER {1003D89313}>
```
")


(defsection @adding-a-build-method (:title "Add a Build Method")
  "
Now open a `src/builders/geneva/builder.lisp` file again and
add DOCS-BUILDER/BUILDER:BUILD method. The method should build HTML
documentation and return a path to the folder.

```
(defmethod docs-builder/builder:build ((builder builder) (system asdf:system))
  (let* ((docs-source-dir
           (asdf:system-relative-pathname system
                                          \"docs/source/\"))
         (docs-output-dir
           (asdf:system-relative-pathname system
                                          \"docs/build/\"))
         (index-input-filename
           (uiop:merge-pathnames* docs-source-dir
                                  \"index.mk2\"))
         (index-output-filename
           (uiop:merge-pathnames* docs-output-dir
                                  \"index.html\"))
         (document (with-open-file (s index-input-filename)
                     (geneva.mk2:read-mk2 s))))
    (ensure-directories-exist docs-output-dir)
    
    (uiop:with-output-file (s index-output-filename
                              :if-exists :supersede)
      (geneva.html:render-html document :stream s)
      ;; Return a directory with resulting docs:
      docs-output-dir)))
```

Finally, we can build our documentation:

```
CL-USER> (docs-builder:build :example)
 <INFO> [23:53:48] docs-builder/builder builder.lisp (build :around system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM \"example\"> found at /Users/art/cl-doc-systems/geneva/
#P\"/Users/art/cl-doc-systems/geneva/docs/build/\"
```

Of cause, in reality this method could be a more complex. It should process all `*.mk2` files
and build API reference for the primary system and all package inferred subsystems.

")


(defsection @supported-builders (:title "Supported Docs Generators")
  (docs-builder/builders/mgl-pax/guesser:@index section)
  (docs-builder/builders/geneva/guesser:@index section))
