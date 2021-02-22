<a id='x-28DOCS-BUILDER-3A-40INDEX-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

# Docs Builder

## Table of Contents

- [1 Usage][8960]
    - [1.1 REPL][e16c]
    - [1.2 Command-line][b5c9]
    - [1.3 GitHub Action][5697]
    - [1.4 Extending][20a2]
        - [1.4.1 Add a Builder Class][ba39]
        - [1.4.2 Guessing a Doc Generator][2782]
        - [1.4.3 Add a Build Method][d527]
- [2 Supported Docs Generators][0ad6]
    - [2.1 MGL-PAX][9b0c]
        - [2.1.1 What is next][f6ff]
    - [2.2 Geneva][f2b9]
        - [2.2.1 What is next][701a]
- [3 Utilities][132a]
- [4 Roadmap][87ec]

###### \[in package DOCS-BUILDER/DOCS\]
This system is a generic documentation builder for Common Lisp Systems.
It able to generate HTML documentation for specified `ASDF` system.

The idea is to use `docs-builder` as an universal HTML documentation builders
which can be used in a continuous integration pipeline. For example, it is
used inside [build-docs](https://40ants.com/build-docs) GitHub action, which can be
used to build docs update gh-pages for any Common Lisp library (if it is uses
documentation generator supported by `docs-builder`).

Currently Docs Builder supports only [MGL-PAX](https://github.com/melisgl/mgl-pax)
can be extended to support other documentation builders, covered by examples in here:
[cl-doc-systems.github.io](https://cl-doc-systems.github.io/).

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40USAGE-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 1 Usage

Documentation can be built in a few ways: from the lisp REPL, command-line and
using the [GitHub action](https://40ants.com/build-docs).

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40REPL-USAGE-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 1.1 REPL

From the REPL, you need first to call a BUILD fuction:

<a id='x-28DOCS-BUILDER-3ABUILD-20FUNCTION-29'></a>

- [function] **DOCS-BUILDER:BUILD** *SYSTEM*

Inside, it will try to guess which documentation builder should be used:

<a id='x-28DOCS-BUILDER-2FGUESSER-3AGUESS-BUILDER-20GENERIC-FUNCTION-29'></a>

- [generic-function] **DOCS-BUILDER/GUESSER:GUESS-BUILDER** *SYSTEM*

    Returns a builder object which can be passed to the BUILD method along with system.
    
    The builder type is guessed using different euristics which depends on a documentation system.
    
    If you want to add support for a new documentation generator, use DEFGUESSER macro.

Then it will pass the builder object and `ASDF` system to the [`DOCS-BUILDER/BUILDER:BUILD`][0169] function:

<a id='x-28DOCS-BUILDER-2FBUILDER-3ABUILD-20GENERIC-FUNCTION-29'></a>

- [generic-function] **DOCS-BUILDER/BUILDER:BUILD** *BUILDER SYSTEM*

    Builds HTML documentation for `ASDF` system and returns absolute path to the dir with docs.

Here is an example how to build documentation for `:docs-builder` `ASDF` system:

```lisp
CL-USER> (docs-builder:build :docs-builder)
 <INFO> [02:12:00] docs-builder/core core.lisp (build :before system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM "docs-builder"> found at /Users/art/projects/docs-builder/
 <INFO> [02:12:00] docs-builder/builders/mgl-pax/builder builder.lisp (build builder system) -
  Building docs in "/Users/art/projects/docs-builder/docs/build/" dir
#P"/Users/art/projects/docs-builder/docs/build/"
```


<a id='x-28DOCS-BUILDER-2FDOCS-3A-40COMMAND-LINE-USAGE-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 1.2 Command-line

You can use builder from command-line. To do this, first install it using [Roswell](https://github.com/roswell/roswell):

```bash
# Note, we need to install this patched mgl-pax
# first, to be able to load docs-builder.
# This step in necessary until this pull
# will be merged:
# https://github.com/melisgl/mgl-pax/pull/8

$ ros install svetlyak40wt/mgl-pax

$ ros install 40ants/docs-builder
```

Here we call it to build documentation for "docs-builder" `ASDF` system:

$ build-docs docs-builder
 <INFO> [02:26:32][] docs-builder/main main.lisp (main) -
  Quickloading system "docs-builder"
 <INFO> [02:26:34][] docs-builder/core core.lisp (build :before system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM "docs-builder"> found at /Users/art/projects/docs-builder/
 <INFO> [02:26:34][] docs-builder/builders/mgl-pax/builder builder.lisp (build builder system) -
  Building docs in "/Users/art/projects/docs-builder/docs/build/" dir
Scan was called 2146 times.

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40GITHUB-ACTION-USAGE-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 1.3 GitHub Action

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
        with:
          asdf-system: cl-info
          qlfile-template: |
            github mgl-pax svetlyak40wt/mgl-pax :branch mgl-pax-minimal
      
      - uses: 40ants/build-docs@v1
        with:
          asdf-system: cl-info
```

You'll find more info in [the action's documentation](https://40ants.com/build-docs).

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40EXTENDING-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 1.4 Extending

Docs builder was made to be extensible and here we see how to add support for a new
documentation generator. A documentation builder we'll use is a [Geneva](https://inters.co/geneva/open-geneva.html).

Some time ago I've wrote [a template system](https://github.com/cl-doc-systems/geneva) to demonstrate how to
document `ASDF` system using Geneva. Lets try to automate docs building and add Geneva support to the DOCS-BUILDER!

First, we need a way to detect if the system uses the Geneva. This is done using "guesser".
Let's define a guesser which will consider we are working with Geneva, if there is a `docs/source/index.mk2`
file. Files with `*.mk2` extensions are special markup used by Geneva.

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-BUILDER-CLASS-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

#### 1.4.1 Add a Builder Class

First thing we need to do is to create a builder class.

Create a `src/builders/geneva/builder.lisp` file with
following content:

```
(defclass builder ()
  ())
```


<a id='x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-GUESSER-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

#### 1.4.2 Guessing a Doc Generator

`DOCS-BUILDER` uses a number of euristics to determine the CLOS class
to be used for documentation generation. Euristics are incapulated in
"guessers". Let's create a `src/builders/geneva/guesser.lisp` file
and define a simple guesser, which will return the `builder` class defined
in the previous section if a file `docs/sources/index.mk2` exists.

To define a guesser, we'll be using [`DOCS-BUILDER/GUESSER:DEFGUESSER`][3656] macro:

<a id='x-28DOCS-BUILDER-2FGUESSER-3ADEFGUESSER-20-28MGL-PAX-MINIMAL-3AMACRO-29-29'></a>

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
  :defsystem-depends-on ("mgl-pax-minimal")
  :depends-on ("docs-builder/core"
               "docs-builder/builders/mgl-pax/guesser"
               "docs-builder/builders/geneva/guesser"))
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


<a id='x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-BUILD-METHOD-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

#### 1.4.3 Add a Build Method

Now open a `src/builders/geneva/builder.lisp` file again and
add [`DOCS-BUILDER/BUILDER:BUILD`][0169] method. The method should build HTML
documentation and return a path to the folder.

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

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40SUPPORTED-BUILDERS-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 2 Supported Docs Generators

<a id='x-28DOCS-BUILDER-2FBUILDERS-2FMGL-PAX-2FGUESSER-3A-40INDEX-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 2.1 MGL-PAX

###### \[in package DOCS-BUILDER/BUILDERS/MGL-PAX/GUESSER\]
This guesser tries to find if your system depends on `MGL-PAX-MINIMAL` system.
If it is, then the [MGL-PAX](https://github.com/melisgl/mgl-pax)
will be used to build documentation.

During the `BUILD` phase, the builder will try to find `MGL-PAX` sections not refereced
from any other sections. For each root section, builder will create a separate HTML
page. If there are few root sections, make sure one of them is having @INDEX name.
Otherwise `index.html` page will not be created.

Algorithm searches section amongh all exported symbols. If you don't want it to find
some root section, just pass `:export nil` to the `MGL-PAX-MINIMAL:DEFSECTION`.

*Note*, that this builder not only renders HTML documentation, but also updates
README files in the system's root directory.

<a id='x-28DOCS-BUILDER-2FBUILDERS-2FMGL-PAX-2FGUESSER-3A-40TODO-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

#### 2.1.1 What is next

- collect all sections from the system's packages and build pages
  for all root sections.

- build a ChangeLog.md out of changelog.lisp, if it is exists


<a id='x-28DOCS-BUILDER-2FBUILDERS-2FGENEVA-2FGUESSER-3A-40INDEX-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

### 2.2 Geneva

###### \[in package DOCS-BUILDER/BUILDERS/GENEVA/GUESSER\]
This guesser tries to find a file `docs/sources/index.mk2` and if it exists
then [Geneva](https://github.com/eugeneia/geneva) documentation generator will be used.

<a id='x-28DOCS-BUILDER-2FBUILDERS-2FGENEVA-2FGUESSER-3A-40TODO-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

#### 2.2.1 What is next

- make builder to process all `*.mk2` files in the `docs/sources/` dir.

- build API reference pages for all packages created by the system.


<a id='x-28DOCS-BUILDER-2FUTILS-3A-40UTILS-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 3 Utilities

###### \[in package DOCS-BUILDER/UTILS\]
<a id='x-28DOCS-BUILDER-2FUTILS-3AEXTERNAL-DEPENDENCIES-20FUNCTION-29'></a>

- [function] **EXTERNAL-DEPENDENCIES** *SYSTEM &KEY ALL*

    Returns a list of string with names of external dependencies of the system.
    
    It works with package-inferred systems too, recursively collecting
    external-dependencies of all subsystems.
    
    Warning! By default, this function does not return dependencies of dependencies.
    To get them all, add `:ALL` `T` option.

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
    For example, [MGL-PAX][9b0c]
    builder uses it to find documentation sections.

<a id='x-28DOCS-BUILDER-2FDOCS-3A-40ROADMAP-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

## 4 Roadmap

- Use [eazy-documentation](https://guicho271828.github.io/eazy-documentation/) as default fallback
  when no other builder was guessed.

- Support other documentation generators, collected at https://cl-doc-systems.github.io/

- Add ability to put a configuration file into the reporitory, for fine-tunning the builder.


  [0169]: #x-28DOCS-BUILDER-2FBUILDER-3ABUILD-20GENERIC-FUNCTION-29 "(DOCS-BUILDER/BUILDER:BUILD GENERIC-FUNCTION)"
  [0ad6]: #x-28DOCS-BUILDER-2FDOCS-3A-40SUPPORTED-BUILDERS-20MGL-PAX-MINIMAL-3ASECTION-29 "Supported Docs Generators"
  [132a]: #x-28DOCS-BUILDER-2FUTILS-3A-40UTILS-20MGL-PAX-MINIMAL-3ASECTION-29 "Utilities"
  [20a2]: #x-28DOCS-BUILDER-2FDOCS-3A-40EXTENDING-20MGL-PAX-MINIMAL-3ASECTION-29 "Extending"
  [2782]: #x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-GUESSER-20MGL-PAX-MINIMAL-3ASECTION-29 "Guessing a Doc Generator"
  [3656]: #x-28DOCS-BUILDER-2FGUESSER-3ADEFGUESSER-20-28MGL-PAX-MINIMAL-3AMACRO-29-29 "(DOCS-BUILDER/GUESSER:DEFGUESSER (MGL-PAX-MINIMAL:MACRO))"
  [5697]: #x-28DOCS-BUILDER-2FDOCS-3A-40GITHUB-ACTION-USAGE-20MGL-PAX-MINIMAL-3ASECTION-29 "GitHub Action"
  [701a]: #x-28DOCS-BUILDER-2FBUILDERS-2FGENEVA-2FGUESSER-3A-40TODO-20MGL-PAX-MINIMAL-3ASECTION-29 "What is next"
  [87ec]: #x-28DOCS-BUILDER-2FDOCS-3A-40ROADMAP-20MGL-PAX-MINIMAL-3ASECTION-29 "Roadmap"
  [8960]: #x-28DOCS-BUILDER-2FDOCS-3A-40USAGE-20MGL-PAX-MINIMAL-3ASECTION-29 "Usage"
  [9b0c]: #x-28DOCS-BUILDER-2FBUILDERS-2FMGL-PAX-2FGUESSER-3A-40INDEX-20MGL-PAX-MINIMAL-3ASECTION-29 "MGL-PAX"
  [b5c9]: #x-28DOCS-BUILDER-2FDOCS-3A-40COMMAND-LINE-USAGE-20MGL-PAX-MINIMAL-3ASECTION-29 "Command-line"
  [ba39]: #x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-BUILDER-CLASS-20MGL-PAX-MINIMAL-3ASECTION-29 "Add a Builder Class"
  [d527]: #x-28DOCS-BUILDER-2FDOCS-3A-40ADDING-A-BUILD-METHOD-20MGL-PAX-MINIMAL-3ASECTION-29 "Add a Build Method"
  [e16c]: #x-28DOCS-BUILDER-2FDOCS-3A-40REPL-USAGE-20MGL-PAX-MINIMAL-3ASECTION-29 "REPL"
  [f2b9]: #x-28DOCS-BUILDER-2FBUILDERS-2FGENEVA-2FGUESSER-3A-40INDEX-20MGL-PAX-MINIMAL-3ASECTION-29 "Geneva"
  [f6ff]: #x-28DOCS-BUILDER-2FBUILDERS-2FMGL-PAX-2FGUESSER-3A-40TODO-20MGL-PAX-MINIMAL-3ASECTION-29 "What is next"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
