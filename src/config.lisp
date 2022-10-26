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
                   (defmethod docs-config ((system (eql (asdf:registered-system \"cl-info\"))))
                     ;; 40ANTS-DOC-THEME-40ANTS system will bring
                     ;; as dependency a full 40ANTS-DOC but we don't want
                     ;; unnecessary dependencies here:
                     (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
                     (list :theme
                           (find-symbol \"40ANTS-THEME\"
                                        (find-package \"40ANTS-DOC-THEME-40ANTS\"))))
                   ```

                   Try to load additional dependencies inside the method. This users of your
                   library will not download dependencies needed only for building documentation.

                   For some special cases it might be useful to return a special key DYNAMIC-BINDINGS.
                   This could be useful, for configuring some custom extensions like it did with
                   interactive demos for the [Weblocks](https://40ants.com/weblocks/). Here is how
                   a method looks like when I configure Weblocks documentation builder:

                   ```
                   (defmethod docs-config ((system (eql (asdf:registered-system \"weblocks\"))))
                     ;; ...
                     (list :theme
                           (find-symbol \"40ANTS-THEME\"
                                        (find-package \"40ANTS-DOC-THEME-40ANTS\"))
                           :dynamic-bindings (list (cons 'weblocks/doc/example:*server-url*
                                                         ;; When local examples server is running,
                                                         ;; we'll be using it instead of production:
                                                         (unless weblocks/doc/example::*port*
                                                           \"http://examples.40ants.com/\"))))
                   ```
                   ")
  (:method ((system t))
    nil)
  (:method ((system-name symbol))
    (docs-config (asdf:registered-system system-name)))
  (:method ((system-name string))
    (docs-config (asdf:registered-system system-name))))
