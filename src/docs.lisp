(uiop:define-package #:docs-builder/docs
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:quicklisp)
  (:import-from #:docs-builder/builders/mgl-pax/guesser)
  (:import-from #:docs-builder/builders/geneva/guesser)
  (:import-from #:docs-builder/utils)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:docs-builder/core))
(in-package docs-builder/docs)


(defmethod docs-config ((system (eql (asdf:find-system "docs-builder"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (ql:quickload :40ants-doc-theme-40ants)
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ignore-words*
    '("HTML"
      "README"
      "PACKAGE-INFERRED-SYSTEM"
      "GIT"
      "REPL"
      "ASDF"
      "MGL-PAX"
      "API"
      "CLOS"))

  (defparameter *introduction*
    "This system is a generic documentation builder for Common Lisp Systems.
It able to generate HTML documentation for specified ASDF system.

The idea is to use `docs-builder` as an universal HTML documentation builders
which can be used in a continuous integration pipeline. For example, it is
used inside [build-docs](https://40ants.com/build-docs) GitHub action, which can be
used to build docs update gh-pages for any Common Lisp library (if it is uses
documentation generator supported by `docs-builder`).

Currently Docs Builder supports only 40ANTS-DOC, [MGL-PAX](https://github.com/melisgl/mgl-pax)
and [Geneva](https://inters.co/geneva/open-geneva.html), but it
can be extended to support other documentation builders, covered by examples in here:
[cl-doc-systems.github.io](https://cl-doc-systems.github.io/).")

  (defparameter *badges*
    "[![](https://github-actions.40ants.com/40ants/docs-builder/matrix.svg)](https://github.com/40ants/docs-builder/actions)"))


(defsection docs-builder:@index (:title "Common Lisp Docs Builder"
                                 :ignore-words *ignore-words*
                                 :external-docs ("https://40ants.com/doc/"
                                                 "https://40ants.com/defmain/"))
  *badges*
  
  (docs-builder system)

  *introduction*
  
  (@usage section)
  (@extending section)
  (@supported-builders section)
  (@api section)
  (docs-builder/utils::@utils section)
  (@roadmap section))


(defsection docs-builder:@readme (:title "Common Lisp Docs Builder"
                                  :ignore-words *ignore-words*)
  *badges*
  (docs-builder system)
  *introduction*
  (@usage section)
  (@roadmap section))


(defsection @usage (:title "Usage")
  "Documentation can be built in a few ways: from the lisp REPL, command-line and
using the [GitHub action](https://40ants.com/build-docs)."

  (@repl-usage section)
  (@command-line-usage section)
  (@github-action-usage section)
  (@additional-params section))


(defsection @repl-usage (:title "REPL")
  "From the REPL, you need first to call a DOCS-BUILDER:BUILD function:"
  
  (docs-builder:build function)

  "Inside, it will try to guess which documentation builder should be used:"
  
  (docs-builder/guesser:guess-builder generic-function)

  "
Then it will pass the builder object and ASDF system to the DOCS-BUILDER/BUILDER:BUILD generic-function:"

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

$ ros install 40ants/doc

$ ros install 40ants/docs-builder
```

Here we call it to build documentation for \"docs-builder\" ASDF system:

```
$ build-docs docs-builder
 <INFO> [02:26:32] docs-builder/main main.lisp (main) -
  Quickloading system \"docs-builder\"
 <INFO> [02:26:34] docs-builder/core core.lisp (build :before system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM \"docs-builder\"> found at /Users/art/projects/docs-builder/
 <INFO> [02:26:34] docs-builder/builders/mgl-pax/builder builder.lisp (build builder system) -
  Building docs in \"/Users/art/projects/docs-builder/docs/build/\" dir
Scan was called 2146 times.
```

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
      - uses: 40ants/build-docs@v1
        with:
          asdf-system: cl-info
```

You'll find more info in [the action's documentation](https://40ants.com/build-docs).
")


(defsection @additional-params (:title "Additional Params")
  "You can customize a builder by defining a method for this generic function:"
  (docs-config:docs-config generic-function))


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
  :defsystem-depends-on (\"40ants-doc\")
  :depends-on (\"docs-builder/core\"
               \"docs-builder/builders/40ants-doc/guesser\"
               \"docs-builder/builders/mgl-pax/guesser\"
               \"docs-builder/builders/geneva/guesser\"))
```

This way, it will be loaded along with the primary system while
`geneva/builder` and it's dependencies will be loaded only
if the system we are building documentation for is using Geneva.

Now we can call DOCS-BUILDER/GUESSER:GUESS-BUILDER generic-function to create a builder for example
system:

```
CL-USER> (docs-builder/guesser:guess-builder :example)
#<DOCS-BUILDER/BUILDERS/GENEVA/BUILDER::BUILDER {1003D89313}>
```
")


(defsection @adding-a-build-method (:title "Add a Build Method")
  "
Now open a `src/builders/geneva/builder.lisp` file again and
add a method to the DOCS-BUILDER/BUILDER:BUILD generic-function.
The method should build HTML documentation and return a path to the folder.

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


(defsection @api (:title "API")
  (docs-builder/core:documentation-has-problems condition)
  (docs-builder/core:num-of-warnings (reader docs-builder:documentation-has-problems)))


(defsection @supported-builders (:title "Supported Docs Generators")
  (docs-builder/builders/40ants-doc/guesser::@index section)
  (docs-builder/builders/mgl-pax/guesser::@index section)
  (docs-builder/builders/geneva/guesser::@index section))
