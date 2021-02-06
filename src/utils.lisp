(mgl-pax-minimal:define-package #:docs-builder/utils
  (:use #:cl)
  (:import-from #:mgl-pax-minimal
                #:defsection)
  (:documentation "The utils for documentation builders."))
(in-package docs-builder/utils)


(defsection @utils (:title "Utilities")
  (external-dependencies function))


(defun ensure-supported (item)
  "Returns either a string or nil if this item should be skipped.

   ITEM can be a string or a list like:

      (:feature :windows \"winhttp\")
"
  (etypecase item
    (string item)
    (list
     (ecase (first item)
       (:feature
        (when (uiop:featurep (second item))
          (ensure-supported (third item))))))))


(defun external-dependencies (system &key all)
  "Returns a list of string with names of external dependencies of the system.

   It works with package-inferred systems too, recursively collecting
   external-dependencies of all subsystems.

   Warning! By default, this function does not return dependencies of dependencies.
   To get them all, add :ALL T option."
  (etypecase system
    ((or symbol string)
     (external-dependencies (asdf:find-system system)
                             :all all))
    (asdf:system
     (loop for item in (asdf:system-depends-on system)
           for real-item = (ensure-supported item)
           when (and real-item
                     (asdf/system:primary-system-p real-item))
             collect real-item
           when (and real-item
                     (or all
                         (not (asdf/system:primary-system-p real-item))))
             append (external-dependencies real-item
                                           :all all)))))
