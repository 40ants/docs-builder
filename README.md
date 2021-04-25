<a id='x-28DOCS-BUILDER-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# Common Lisp Docs Builder

## Table of Contents

- [1 docs-builder ASDF System Details][52d8]
- [2 Usage][16b5]
    - [2.1 REPL][76f2]
    - [2.2 Command-line][03bf]
    - [2.3 GitHub Action][6577]
    - [2.4 Extending][81d2]
        - [2.4.1 Add a Builder Class][c798]
        - [2.4.2 Guessing a Doc Generator][4ba2]
        - [2.4.3 Add a Build Method][a8f9]
- [3 Supported Docs Generators][1999]
    - [3.1 40ANTS-DOC][6ad1]
        - [3.1.1 What is next][22e3]
    - [3.2 MGL-PAX][9a66]
    - [3.3 Geneva][3583]
        - [3.3.1 What is next][3227]
- [4 Utilities][8bb5]
- [5 Roadmap][4bf7]

###### \[in package DOCS-BUILDER/DOCS\]
[![](https://github-actions.40ants.com/40ants/docs-builder/matrix.svg)](https://github.com/40ants/docs-builder/actions)

<a id='x-28-23A-28-2812-29-20BASE-CHAR-20-2E-20-22docs-builder-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 docs-builder ASDF System Details

- Description: A meta documentation builder for Common Lisp projects.
- Licence: Unlicense
- Author: Alexander Artemenko
- Homepage: [https://40ants.com/docs-builder](https://40ants.com/docs-builder)
- Bug tracker: [https://github.com/40ants/docs-builder/issues](https://github.com/40ants/docs-builder/issues)
- Source control: [GIT](https://github.com/40ants/docs-builder)

This system is a generic documentation builder for Common Lisp Systems.
It able to generate HTML documentation for specified `ASDF` system.

The idea is to use [`docs-builder`][52d8] as an universal HTML documentation builders
which can be used in a continuous integration pipeline. For example, it is
used inside [build-docs](https://40ants.com/build-docs) GitHub action, which can be
used to build docs update gh-pages for any Common Lisp library (if it is uses
documentation generator supported by [`docs-builder`][52d8]).

Currently Docs Builder supports only [MGL-PAX](https://github.com/melisgl/mgl-pax)
can be extended to support other documentation builders, covered by examples in here:
[cl-doc-systems.github.io](https://cl-doc-systems.github.io/).

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 2 Usage

Documentation can be built in a few ways: from the lisp REPL, command-line and
using the [GitHub action](https://40ants.com/build-docs).

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40REPL-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

### 2.1 REPL

From the REPL, you need first to call a [`DOCS-BUILDER:BUILD`][036a] function:

<a id='x-28DOCS-BUILDER-3ABUILD-20FUNCTION-29'></a>

- [function] **DOCS-BUILDER:BUILD** *SYSTEM &KEY (ERROR-ON-WARNINGS T)*

    Builds HTML documentation for `ASDF` system and returns absolute path to the dir with docs.

Inside, it will try to guess which documentation builder should be used:

<a id='x-28DOCS-BUILDER-2FGUESSER-3AGUESS-BUILDER-20GENERIC-FUNCTION-29'></a>

- [generic-function] **DOCS-BUILDER/GUESSER:GUESS-BUILDER** *SYSTEM*

    Returns a builder object which can be passed to the [`DOCS-BUILDER/BUILDER:BUILD`][0169] generic-function along with system.
    
    The builder type is guessed using different euristics which depends on a documentation system.
    
    If you want to add support for a new documentation generator, use [`DEFGUESSER`][c83f] macro.

Then it will pass the builder object and `ASDF` system to the [`DOCS-BUILDER/BUILDER:BUILD`][0169] generic-function:

<a id='x-28DOCS-BUILDER-2FBUILDER-3ABUILD-20GENERIC-FUNCTION-29'></a>

- [generic-function] **DOCS-BUILDER/BUILDER:BUILD** *BUILDER SYSTEM*

    Builds HTML documentation for `ASDF` system and returns absolute path to the dir with docs.

Here is an example how to build documentation for [`:docs-builder`][52d8] `ASDF` system:

```lisp
CL-USER> (docs-builder:build :docs-builder)
 <INFO> [02:12:00] docs-builder/core core.lisp (build :before system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM "docs-builder"> found at /Users/art/projects/docs-builder/
 <INFO> [02:12:00] docs-builder/builders/mgl-pax/builder builder.lisp (build builder system) -
  Building docs in "/Users/art/projects/docs-builder/docs/build/" dir
#P"/Users/art/projects/docs-builder/docs/build/"
```


<a id='x-28DOCS-BUILDER-2FDOCS-3A-40COMMAND-LINE-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

### 2.2 Command-line

You can use builder from command-line. To do this, first install it using [Roswell](https://github.com/roswell/roswell):

```bash
# Note, we need to install this patched mgl-pax
# first, to be able to load docs-builder.
# This step in necessary until this pull
# will be merged:
# https://github.com/melisgl/mgl-pax/pull/8

$ ros install 40ants/doc

$ ros install 40ants/docs-builder
```

Here we call it to build documentation for "docs-builder" `ASDF` system:

```
$ build-docs docs-builder
 <INFO> [02:26:32] docs-builder/main main.lisp (main) -
  Quickloading system "docs-builder"
 <INFO> [02:26:34] docs-builder/core core.lisp (build :before system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM "docs-builder"> found at /Users/art/projects/docs-builder/
 <INFO> [02:26:34] docs-builder/builders/mgl-pax/builder builder.lisp (build builder system) -
  Building docs in "/Users/art/projects/docs-builder/docs/build/" dir
Scan was called 2146 times.
```


<a id='x-28DOCS-BUILDER-2FDOCS-3A-40GITHUB-ACTION-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

### 2.3 GitHub Action

If you host your project on the GitHub, then the most easy way to build and host documentation
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

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40EXTENDING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

### 2.4 Extending

Docs builder was made to be extensible and here we see how to add support for a new
documentation generator. A documentation builder we'll use is a [Geneva](https://inters.co/geneva/open-geneva.html).

Some time ago I've wrote [a template system](https://github.com/cl-doc-systems/geneva) to demonstrate how to
document `ASDF` system using Geneva. Lets try to automate docs building and add Geneva support to the DOCS-BUILDER!

First, we need a way to detect if the system uses the Geneva. This is done using "guesser".
Let's define a guesser which will consider we are working with Geneva, if there is a `docs/source/index.mk2`
file. Files with `*.mk2` extensions are special markup used by Geneva.

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-BUILDER-CLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

#### 2.4.1 Add a Builder Class

First thing we need to do is to create a builder class.

Create a `src/builders/geneva/builder.lisp` file with
following content:

```
(defclass builder ()
  ())
```


<a id='x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-GUESSER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

#### 2.4.2 Guessing a Doc Generator

[`DOCS-BUILDER`][52d8] uses a number of euristics to determine the CLOS class
to be used for documentation generation. Euristics are incapulated in
"guessers". Let's create a `src/builders/geneva/guesser.lisp` file
and define a simple guesser, which will return the `builder` class defined
in the previous section if a file `docs/sources/index.mk2` exists.

To define a guesser, we'll be using [`DOCS-BUILDER/GUESSER:DEFGUESSER`][c83f] macro:

<a id='x-28DOCS-BUILDER-2FGUESSER-3ADEFGUESSER-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29'></a>

- [macro] **DOCS-BUILDER/GUESSER:DEFGUESSER** *NAME (SYSTEM) &BODY BODY*

```
(docs-builder/guesser:defguesser geneva (system)
  (when (probe-file
         (asdf:system-relative-pathname system
                                        "docs/source/index.mk2"))
    (ql:quickload :docs-builder/builders/geneva/builder
                  :silent t)
    (make-instance (intern "BUILDER" "DOCS-BUILDER/BUILDERS/GENEVA/BUILDER"))))
```

When all rules matched, guesser loads the builder package and all its dependencies.
In our case, `Geneva` system will be loaded.

Add this guesser file to the `docs-builder.asd` file:

```
(defsystem "docs-builder" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description ""
  :defsystem-depends-on ("40ants-doc")
  :depends-on ("docs-builder/core"
               "docs-builder/builders/40ants-doc/guesser"
               "docs-builder/builders/mgl-pax/guesser"
               "docs-builder/builders/geneva/guesser"))
```

This way, it will be loaded along with the primary system while
`geneva/builder` and it's dependencies will be loaded only
if the system we are building documentation for is using Geneva.

Now we can call [`DOCS-BUILDER/GUESSER:GUESS-BUILDER`][0577] generic-function to create a builder for example
system:

```
CL-USER> (docs-builder/guesser:guess-builder :example)
#<DOCS-BUILDER/BUILDERS/GENEVA/BUILDER::BUILDER {1003D89313}>
```


<a id='x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-BUILD-METHOD-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

#### 2.4.3 Add a Build Method

Now open a `src/builders/geneva/builder.lisp` file again and
add a method to the [`DOCS-BUILDER/BUILDER:BUILD`][0169] generic-function.
The method should build HTML documentation and return a path to the folder.

```
(defmethod docs-builder/builder:build ((builder builder) (system asdf:system))
  (let* ((docs-source-dir
           (asdf:system-relative-pathname system
                                          "docs/source/"))
         (docs-output-dir
           (asdf:system-relative-pathname system
                                          "docs/build/"))
         (index-input-filename
           (uiop:merge-pathnames* docs-source-dir
                                  "index.mk2"))
         (index-output-filename
           (uiop:merge-pathnames* docs-output-dir
                                  "index.html"))
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
  Building docs for system #<PACKAGE-INFERRED-SYSTEM "example"> found at /Users/art/cl-doc-systems/geneva/
#P"/Users/art/cl-doc-systems/geneva/docs/build/"
```

Of cause, in reality this method could be a more complex. It should process all `*.mk2` files
and build API reference for the primary system and all package inferred subsystems.

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40SUPPORTED-BUILDERS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 3 Supported Docs Generators

<a id='x-28DOCS-BUILDER-2FBUILDERS-2F40ANTS-DOC-2FGUESSER-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

### 3.1 40ANTS-DOC

###### \[in package DOCS-BUILDER/BUILDERS/40ANTS-DOC/GUESSER\]
This guesser tries to find if your system depends on `40ANTS-DOC` system.
If it is, then the [40ANTS-DOC](https://github.com/40ants/doc)
will be used to build documentation.

During the `build` phase, the builder will try to find documentation sections not refereced
from any other sections. For each root section, builder will create a separate HTML
page. If there are few root sections, make sure one of them is having @INDEX name.
Otherwise `index.html` page will not be created.

Algorithm searches section amongh all exported symbols. If you don't want it to find
some root section, just pass `:export nil` to the `40ANTS-DOC:DEFSECTION` macro.

If you want your documentation link back to the GitHub sources, make sure
you have either `:homepage` or `:source-control` in your `ASDF` definition:

```
(asdf:defsystem #:example
  :licence "MIT"
  :version "0.0.3"
  :author "John Doe"
  :mailto "john@doe.me"
  :homepage "https://github.com/john-doe/example"
  :source-control (:git "https://github.com/john-doe/example")
  ...)
```

*Note*, that this builder not only renders HTML documentation, but also updates
README files in the system's root directory.

<a id='x-28DOCS-BUILDER-2FBUILDERS-2F40ANTS-DOC-2FGUESSER-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

#### 3.1.1 What is next

- build a ChangeLog.md out of changelog.lisp, if it is exists


<a id='x-28DOCS-BUILDER-2FBUILDERS-2FMGL-PAX-2FGUESSER-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

### 3.2 MGL-PAX

###### \[in package DOCS-BUILDER/BUILDERS/MGL-PAX/GUESSER\]
This guesser tries to find if your system depends on `MGL-PAX` system.
If it is, then the [MGL-PAX](https://github.com/melisgl/mgl-pax)
will be used to build documentation.

During the `build` phase, the builder will try to find `MGL-PAX` sections not refereced
from any other sections. For each root section, builder will create a separate HTML
page. If there are few root sections, make sure one of them is having @INDEX name.
Otherwise `index.html` page will not be created.

Algorithm searches section amongh all exported symbols. If you don't want it to find
some root section, just pass `:export nil` to the `MGL-PAX:DEFSECTION` macro.

If you want your documentation link back to the GitHub sources, make sure
you have either `:homepage` or `:source-control` in your `ASDF` definition:

```
(asdf:defsystem #:example
  :licence "MIT"
  :version "0.0.3"
  :author "John Doe"
  :mailto "john@doe.me"
  :homepage "https://github.com/john-doe/example"
  :source-control (:git "https://github.com/john-doe/example")
  ...)
```

*Note*, that this builder not only renders HTML documentation, but also updates
README files in the system's root directory.

<a id='x-28DOCS-BUILDER-2FBUILDERS-2FGENEVA-2FGUESSER-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

### 3.3 Geneva

###### \[in package DOCS-BUILDER/BUILDERS/GENEVA/GUESSER\]
This guesser tries to find a file `docs/sources/index.mk2` and if it exists
then [Geneva](https://github.com/eugeneia/geneva) documentation generator will be used.

<a id='x-28DOCS-BUILDER-2FBUILDERS-2FGENEVA-2FGUESSER-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

#### 3.3.1 What is next

- make builder to process all `*.mk2` files in the `docs/sources/` dir.

- build API reference pages for all packages created by the system.


<a id='x-28DOCS-BUILDER-2FUTILS-3A-40UTILS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 4 Utilities

###### \[in package DOCS-BUILDER/UTILS\]
<a id='x-28DOCS-BUILDER-2FUTILS-3AEXTERNAL-DEPENDENCIES-20FUNCTION-29'></a>

- [function] **EXTERNAL-DEPENDENCIES** *SYSTEM &KEY ALL*

    Returns a list of string with names of external dependencies of the system.
    
    It works with package-inferred systems too, recursively collecting
    external-dependencies of all subsystems.
    
    Warning! By default, this function does not return dependencies of dependencies.
    To get them all, add `:ALL` T option.

<a id='x-28DOCS-BUILDER-2FUTILS-3ASYSTEM-PACKAGES-20GENERIC-FUNCTION-29'></a>

- [generic-function] **SYSTEM-PACKAGES** *SYSTEM*

    Returns a list of packages created by `ASDF` system.
    
    Default implementation returns a package having the same name as a system
    and all packages matched to package-inferred subsystems:
    
    ```
    CL-USER> (docs-builder/utils:system-packages :docs-builder)
    (#<PACKAGE "DOCS-BUILDER">
     #<PACKAGE "DOCS-BUILDER/UTILS">
     #<PACKAGE "DOCS-BUILDER/GUESSER">
     #<PACKAGE "DOCS-BUILDER/BUILDERS/GENEVA/GUESSER">
     #<PACKAGE "DOCS-BUILDER/BUILDER">
     #<PACKAGE "DOCS-BUILDER/BUILDERS/MGL-PAX/GUESSER">
     #<PACKAGE "DOCS-BUILDER/DOCS">
     #<PACKAGE "DOCS-BUILDER/BUILDERS/MGL-PAX/BUILDER">)
    ```
    
    This function can be used by builder to find pieces of documentation.
    For example, [MGL-PAX][9a66]
    builder uses it to find documentation sections.

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 5 Roadmap

- Use [eazy-documentation](https://guicho271828.github.io/eazy-documentation/) as default fallback
  when no other builder was guessed.

- Support other documentation generators, collected at https://cl-doc-systems.github.io/

- Add ability to put a configuration file into the reporitory, for fine-tunning the builder.


  [0169]: #x-28DOCS-BUILDER-2FBUILDER-3ABUILD-20GENERIC-FUNCTION-29 "(DOCS-BUILDER/BUILDER:BUILD GENERIC-FUNCTION)"
  [036a]: #x-28DOCS-BUILDER-3ABUILD-20FUNCTION-29 "(DOCS-BUILDER:BUILD FUNCTION)"
  [03bf]: #x-28DOCS-BUILDER-2FDOCS-3A-40COMMAND-LINE-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Command-line"
  [0577]: #x-28DOCS-BUILDER-2FGUESSER-3AGUESS-BUILDER-20GENERIC-FUNCTION-29 "(DOCS-BUILDER/GUESSER:GUESS-BUILDER GENERIC-FUNCTION)"
  [16b5]: #x-28DOCS-BUILDER-2FDOCS-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Usage"
  [1999]: #x-28DOCS-BUILDER-2FDOCS-3A-40SUPPORTED-BUILDERS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Supported Docs Generators"
  [22e3]: #x-28DOCS-BUILDER-2FBUILDERS-2F40ANTS-DOC-2FGUESSER-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "What is next"
  [3227]: #x-28DOCS-BUILDER-2FBUILDERS-2FGENEVA-2FGUESSER-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "What is next"
  [3583]: #x-28DOCS-BUILDER-2FBUILDERS-2FGENEVA-2FGUESSER-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Geneva"
  [4ba2]: #x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-GUESSER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Guessing a Doc Generator"
  [4bf7]: #x-28DOCS-BUILDER-2FDOCS-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Roadmap"
  [52d8]: #x-28-23A-28-2812-29-20BASE-CHAR-20-2E-20-22docs-builder-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29 "(#A((12) BASE-CHAR . \"docs-builder\") ASDF/SYSTEM:SYSTEM)"
  [6577]: #x-28DOCS-BUILDER-2FDOCS-3A-40GITHUB-ACTION-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "GitHub Action"
  [6ad1]: #x-28DOCS-BUILDER-2FBUILDERS-2F40ANTS-DOC-2FGUESSER-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "40ANTS-DOC"
  [76f2]: #x-28DOCS-BUILDER-2FDOCS-3A-40REPL-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "REPL"
  [81d2]: #x-28DOCS-BUILDER-2FDOCS-3A-40EXTENDING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Extending"
  [8bb5]: #x-28DOCS-BUILDER-2FUTILS-3A-40UTILS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Utilities"
  [9a66]: #x-28DOCS-BUILDER-2FBUILDERS-2FMGL-PAX-2FGUESSER-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "MGL-PAX"
  [a8f9]: #x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-BUILD-METHOD-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Add a Build Method"
  [c798]: #x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-BUILDER-CLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Add a Builder Class"
  [c83f]: #x-28DOCS-BUILDER-2FGUESSER-3ADEFGUESSER-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29 "(DOCS-BUILDER/GUESSER:DEFGUESSER (40ANTS-DOC/LOCATIVES:MACRO))"

* * *
###### \[generated by [40ANTS-DOC](https://40ants.com/doc)\]
