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

                   Implement a method, EQL specialized on a concrete ASDF system.")
  (:method ((system t))
    nil)
  (:method ((system-name symbol))
    (docs-config (asdf:find-system system-name)))
  (:method ((system-name string))
    (docs-config (asdf:find-system system-name))))
