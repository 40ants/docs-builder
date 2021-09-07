(uiop:define-package #:docs-builder/utils
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:documentation "The utils for documentation builders.")
  (:export #:system-packages
           #:external-dependencies))
(in-package docs-builder/utils)


(defsection @utils (:title "Utilities")
  (external-dependencies function)
  (system-packages generic-function))


(defun ensure-supported (item)
  "Returns either a string or nil if this item should be skipped.

   ITEM can be a string or a list like:

      (:feature :windows \"winhttp\")
"
  (etypecase item
    (symbol (string-downcase
             (symbol-name item)))
    (string item)
    (list
     (ecase (first item)
       (:version
        ;; Like (:VERSION "fare-utils" "1.0.0")
        (ensure-supported (second item)))
       (:require
        ;; Like (:REQUIRE :SB-POSIX)
        (ensure-supported (second item)))
       (:feature
        (when (uiop:featurep (second item))
          (ensure-supported (third item))))))))


(defun external-dependencies (system &key all)
  "Returns a list of string with names of external dependencies of the system.

    It works with package-inferred systems too, recursively collecting
    external-dependencies of all subsystems.

    Warning! By default, this function does not return dependencies of dependencies.
    To get them all, add :ALL T option."
  (let ((visited nil)
        (results nil))
    (labels ((recurse (system)
               (typecase system
                 ((or string symbol)
                  (recurse (asdf:find-system system)))
                 
                 (asdf:system
                  (loop for item in (asdf:system-depends-on system)
                        for real-item = (ensure-supported item)
                        for seen = (when real-item
                                     (member real-item visited :test #'equal))
                        when (and real-item
                                  (not seen)
                                  (asdf/system:primary-system-p real-item))
                        do (pushnew real-item results
                                    :test #'string=)
                        when (and real-item
                                  (not seen)
                                  (or all
                                      (not (asdf/system:primary-system-p real-item))))
                        do (push real-item visited)
                           (recurse real-item))))))
      (recurse system)
      (values (sort results
                    #'string<)))))


(defgeneric system-packages (system)
  (:documentation "Returns a list of packages created by ASDF system.

Default implementation returns a package having the same name as a system
and all packages matched to package-inferred subsystems:

```
CL-USER> (docs-builder/utils:system-packages :docs-builder)
(#<PACKAGE \"DOCS-BUILDER\">
 #<PACKAGE \"DOCS-BUILDER/UTILS\">
 #<PACKAGE \"DOCS-BUILDER/GUESSER\">
 #<PACKAGE \"DOCS-BUILDER/BUILDERS/GENEVA/GUESSER\">
 #<PACKAGE \"DOCS-BUILDER/BUILDER\">
 #<PACKAGE \"DOCS-BUILDER/BUILDERS/MGL-PAX/GUESSER\">
 #<PACKAGE \"DOCS-BUILDER/DOCS\">
 #<PACKAGE \"DOCS-BUILDER/BUILDERS/MGL-PAX/BUILDER\">)
```

This function can be used by builder to find pieces of documentation.
For example, DOCS-BUILDER/BUILDERS/MGL-PAX/GUESSER:@INDEX
builder uses it to find documentation sections.
")
  (:method ((system string))
    (system-packages (asdf:find-system system)))
  (:method ((system symbol))
    (system-packages (asdf:find-system system)))
  (:method ((system asdf:system))

    (let* ((package-name (string-upcase
                          ;; If we are building documentation for the subsystem
                          ;; of a large ASDF package inferred system,
                          ;; then most probably we want to collect packages
                          ;; of the whole ASDF system.
                          (asdf:primary-system-name system)))
           (primary-package (find-package package-name)))
      (append (when primary-package
                (list primary-package))
              (loop with prefix-name = (format nil "~A/" package-name)
                    with prefix-name-length = (length prefix-name)
                    for package in (list-all-packages)
                    for package-name = (package-name package)
                    when (and (> (length package-name)
                                 prefix-name-length)
                              (string= (subseq package-name
                                               0 prefix-name-length)
                                       prefix-name))
                    collect package)))))
