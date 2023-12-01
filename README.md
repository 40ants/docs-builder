<a id="x-28DOCS-BUILDER-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Common Lisp Docs Builder

[![](https://github-actions.40ants.com/40ants/docs-builder/matrix.svg)][100b]

<a id="docs-builder-asdf-system-details"></a>

## DOCS-BUILDER ASDF System Details

* Description: A meta documentation builder for Common Lisp projects.
* Licence: Unlicense
* Author: Alexander Artemenko
* Homepage: [https://40ants.com/docs-builder][3993]
* Bug tracker: [https://github.com/40ants/docs-builder/issues][7d71]
* Source control: [GIT][843b]
* Depends on: [40ants-doc][2c00], [alexandria][8236], [docs-config][ce67], [log4cl][7f8b], [uiop][5274]

This system is a generic documentation builder for Common Lisp Systems.
It able to generate `HTML` documentation for specified `ASDF` system.

The idea is to use `docs-builder` as an universal `HTML` documentation builders
which can be used in a continuous integration pipeline. For example, it is
used inside [build-docs][0ebc] GitHub action, which can be
used to build docs update gh-pages for any Common Lisp library (if it is uses
documentation generator supported by `docs-builder`).

Currently Docs Builder supports only [`40ants-doc`][a2c7], [MGL-PAX][7927]
and [Geneva][05ae], but it
can be extended to support other documentation builders, covered by examples in here:
[cl-doc-systems.github.io][8884].

<a id="x-28DOCS-BUILDER-2FDOCS-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

Documentation can be built in a few ways: from the lisp `REPL`, command-line and
using the [GitHub action][0ebc].

<a id="x-28DOCS-BUILDER-2FDOCS-3A-3A-40REPL-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### REPL

From the `REPL`, you need first to call a [`docs-builder:build`][febf] function:

<a id="x-28DOCS-BUILDER-3ABUILD-20FUNCTION-29"></a>

#### [function](83f8) `docs-builder:build` system &rest rest &key (error-on-warnings t) &allow-other-keys

Builds `HTML` documentation for `ASDF` system and returns absolute path to the dir with docs.

Inside, it will try to guess which documentation builder should be used:

<a id="x-28DOCS-BUILDER-2FGUESSER-3AGUESS-BUILDER-20GENERIC-FUNCTION-29"></a>

#### [generic-function](b835) `docs-builder/guesser:guess-builder` system

Returns a builder object which can be passed to the [`docs-builder/builder:build`][9de0] generic-function along with system.

The builder type is guessed using different euristics which depends on a documentation system.

If you want to add support for a new documentation generator, use [`defguesser`][3453] macro.

Then it will pass the builder object and `ASDF` system to the [`docs-builder/builder:build`][9de0] generic-function:

<a id="x-28DOCS-BUILDER-2FBUILDER-3ABUILD-20GENERIC-FUNCTION-29"></a>

#### [generic-function](5d18) `docs-builder/builder:build` builder system &key local root-sections &allow-other-keys

Builds `HTML` documentation for `ASDF` system and returns absolute path to the dir with docs.

Here is an example how to build documentation for `:docs-builder` `ASDF` system:

```lisp
CL-USER> (docs-builder:build :docs-builder)
 <INFO> [02:12:00] docs-builder/core core.lisp (build :before system) -
  Building docs for system #<PACKAGE-INFERRED-SYSTEM "docs-builder"> found at /Users/art/projects/docs-builder/
 <INFO> [02:12:00] docs-builder/builders/mgl-pax/builder builder.lisp (build builder system) -
  Building docs in "/Users/art/projects/docs-builder/docs/build/" dir
#P"/Users/art/projects/docs-builder/docs/build/"
```
<a id="x-28DOCS-BUILDER-2FDOCS-3A-3A-40COMMAND-LINE-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Command-line

You can use builder from command-line. To do this, first install it using [Roswell][795a]:

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
<a id="x-28DOCS-BUILDER-2FDOCS-3A-3A-40GITHUB-ACTION-USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### GitHub Action

If you host your project on the GitHub, then the most easy way to build and host documentation
would be to use [Github Pages][9f4c].

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
You'll find more info in [the action's documentation][0ebc].

<a id="x-28DOCS-BUILDER-2FDOCS-3A-3A-40ADDITIONAL-PARAMS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Additional Params

You can customize a builder by defining a method for this generic function:

<a id="x-28DOCS-CONFIG-3ADOCS-CONFIG-20GENERIC-FUNCTION-29"></a>

#### [generic-function](aab2) `docs-config:docs-config` asdf-system

Should return a plist which will be passed as keyword
arguments to the documentation builder when building
docs for a given asdf-system.

Implement a method, `EQL` specialized on a concrete `ASDF` system.

Here is a typical method I use for my own libraries to set
a custom theme for [`40ants-doc`][a2c7] system:

```lisp
(defmethod docs-config ((system (eql (asdf:registered-system "cl-info"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))
```
Try to load additional dependencies inside the method. This users of your
library will not download dependencies needed only for building documentation.

For some special cases it might be useful to return a special key `DYNAMIC-BINDINGS`.
This could be useful, for configuring some custom extensions like it did with
interactive demos for the [Weblocks][8d16]. Here is how
a method looks like when I configure Weblocks documentation builder:

```
(defmethod docs-config ((system (eql (asdf:registered-system "weblocks"))))
  ;; ...
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))
        :dynamic-bindings (list (cons 'weblocks/doc/example:*server-url*
                                      ;; When local examples server is running,
                                      ;; we'll be using it instead of production:
                                      (unless weblocks/doc/example::*port*
                                        "http://examples.40ants.com/"))))
```
<a id="x-28DOCS-BUILDER-2FDOCS-3A-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Roadmap

* Use [eazy-documentation][f9f7] as default fallback
  when no other builder was guessed.
* Support other documentation generators, collected at https://cl-doc-systems.github.io/
* Add ability to put a configuration file into the reporitory, for fine-tunning the builder.


[0ebc]: https://40ants.com/build-docs
[a2c7]: https://40ants.com/doc/#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[3993]: https://40ants.com/docs-builder
[9de0]: https://40ants.com/docs-builder/#x-28DOCS-BUILDER-2FBUILDER-3ABUILD-20GENERIC-FUNCTION-29
[3453]: https://40ants.com/docs-builder/#x-28DOCS-BUILDER-2FGUESSER-3ADEFGUESSER-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[febf]: https://40ants.com/docs-builder/#x-28DOCS-BUILDER-3ABUILD-20FUNCTION-29
[8d16]: https://40ants.com/weblocks/
[8884]: https://cl-doc-systems.github.io/
[843b]: https://github.com/40ants/docs-builder
[100b]: https://github.com/40ants/docs-builder/actions
[5d18]: https://github.com/40ants/docs-builder/blob/12b6827772e09510f46e2ec54b41827fc693d3f4/src/builder.lisp#L10
[aab2]: https://github.com/40ants/docs-builder/blob/12b6827772e09510f46e2ec54b41827fc693d3f4/src/config.lisp#L9
[83f8]: https://github.com/40ants/docs-builder/blob/12b6827772e09510f46e2ec54b41827fc693d3f4/src/core.lisp#L29
[b835]: https://github.com/40ants/docs-builder/blob/12b6827772e09510f46e2ec54b41827fc693d3f4/src/guesser.lisp#L14
[7d71]: https://github.com/40ants/docs-builder/issues
[7927]: https://github.com/melisgl/mgl-pax
[795a]: https://github.com/roswell/roswell
[f9f7]: https://guicho271828.github.io/eazy-documentation/
[05ae]: https://inters.co/geneva/open-geneva.html
[9f4c]: https://pages.github.com/
[2c00]: https://quickdocs.org/40ants-doc
[8236]: https://quickdocs.org/alexandria
[ce67]: https://quickdocs.org/docs-config
[7f8b]: https://quickdocs.org/log4cl
[5274]: https://quickdocs.org/uiop

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
