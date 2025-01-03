<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E12-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.12.0 (2024-12-14)

* Now `DYNAMIC_SPACE_SIZE` env variable can be used to control amount of memory available to `SBCL`, by default it is `1Gb`.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E11-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.11.1 (2024-03-11)

* Fixed initite loop when the system is unknown to `ASDF` yet.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E11-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.11.0 (2023-06-05)

* Now docs builder tries to load system using either Quicklisp client or `ASDF` if system is not already loaded.
* Also a bug was fixed - previously [`docs-builder:build`][febf] function hanged in recursion in case if asdf system wasn't found.
  Now it will show an error.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E10-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.0 (2022-11-16)

Support new refactored [`40ants-doc`][a2c7] system.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E9-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.1 (2022-10-26)

Fixed an issue with warnings from `ASDF` find-system function. Also, a build now should go faster.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E9-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.0 (2021-10-27)

Added support for `DYNAMIC-BINDINGS` key in [`docs-config:docs-config`][37c9]'s results. This allows to configure documentation builder
in case if usual arguments passing is not enough.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E8-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.0 (2021-10-21)

[`docs-builder/builder:build`][9de0] generic-function specialized on [`40ants-doc`][a2c7] system now supports `ROOT-SECTIONS` argument.
You can specify this argument to render a multipage documentation and suppress a warning about more than one root section.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E7-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.1 (2021-09-11)

Fixed building documentation with [`40ants-doc`][a2c7] if changelog section is absent.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E7-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.0 (2021-09-04)

Added [`docs-config:docs-config`][37c9] generic-function to allow define
a special config for documented systems.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E6-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.2 (2021-08-31)

Speedup documentation builder guessing.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E6-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.1 (2021-08-28)

Fixed argument passing to the builder. Previously
error `Unknown &KEY argument: :ERROR-ON-WARNINGS` has happened. 

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E6-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.0 (2021-08-28)

Support new refactored [`40ants-doc`][a2c7] system.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E5-2E3-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.3 (2021-05-08)

Roswell script was fixed to work with latest [`defmain`][dc1a] system.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E5-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.2 (2021-04-25)

Fixed the case when [`40ants-doc`][a2c7] or `MGL-PAX`
builder found the same section twice. This could
happen for a package-inferred system where
some package has the same nickname as as a
primary system.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E5-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.1 (2021-04-16)

Fixed returning of the path to resulting docs.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E5-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.0 (2021-04-15)

Now [`docs-builder:build`][febf] has a special argument `ERROR-ON-WARNINGS`
which is T by default. This flag causes a [`docs-builder:documentation-has-problems`][d28c]
continuable error signaled in case if there were some warnings
during building the documentation.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E4-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.2 (2021-04-05)

Fixed dependency on `log4cl-extras`.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E4-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.1 (2021-04-05)

Now utility will dump traceback
in case of errors during the build.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0 (2021-04-05)

* Added support for [`40ants-doc`][a2c7] documentation builder.
* Switched `MGL-PAX` builder from my fork to original.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0 (2021-02-22)

Now `MGL-PAX` builder is able to discover all root sections and to build
a multipage `HTML` doc.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2021-02-21)

Added basic support for Geneva documentation generator.

<a id="x-28DOCS-BUILDER-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0 (2021-02-04)

Initial version.


[dc1a]: https://40ants.com/defmain/#x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22defmain-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[a2c7]: https://40ants.com/doc/#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[9de0]: https://40ants.com/docs-builder/#x-28DOCS-BUILDER-2FBUILDER-3ABUILD-20GENERIC-FUNCTION-29
[febf]: https://40ants.com/docs-builder/#x-28DOCS-BUILDER-3ABUILD-20FUNCTION-29
[d28c]: https://40ants.com/docs-builder/#x-28DOCS-BUILDER-3ADOCUMENTATION-HAS-PROBLEMS-20CONDITION-29
[37c9]: https://40ants.com/docs-builder/#x-28DOCS-CONFIG-3ADOCS-CONFIG-20GENERIC-FUNCTION-29

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
