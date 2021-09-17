(defpackage #:docs-config
  (:use #:cl)
  (:nicknames #:docs-config/config)
  (:export
   #:docs-config))
(in-package docs-config)


(defgeneric docs-config (asdf-system)
  (:documentation "Should return a plist which will be passed as keyword
                   arguments to the documentation builder when building
                   docs for a given asdf-system.

                   Implement a method, EQL specialized on a concrete ASDF system.

                   Here is a typical method I use for my own libraries to set
                   a custom theme for 40ANTS-DOC system:

                   ```lisp
                   (defmethod docs-config ((system (eql (asdf:find-system \"cl-info\"))))
                     ;; 40ANTS-DOC-THEME-40ANTS system will bring
                     ;; as dependency a full 40ANTS-DOC but we don't want
                     ;; unnecessary dependencies here:
                     (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
                     (list :theme
                           (find-symbol \"40ANTS-THEME\"
                                        (find-package \"40ANTS-DOC-THEME-40ANTS\"))))
                   ```

                   Try to load additional dependencies inside the method. This users of your
                   library will not download dependencies needed only for building documentation.")
  (:method ((system t))
    nil)
  (:method ((system-name symbol))
    (docs-config (asdf:find-system system-name)))
  (:method ((system-name string))
    (docs-config (asdf:find-system system-name))))
