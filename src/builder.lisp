(uiop:define-package #:docs-builder/builder
  (:use #:cl)
  (:import-from :log4cl)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:build))
(in-package docs-builder/builder)


(defgeneric build (builder system &key &allow-other-keys)
  (:documentation "Builds HTML documentation for ASDF system and returns absolute path to the dir with docs."))


(defmethod build ((builder t) (system string) &rest rest &key &allow-other-keys)
  (apply #'build
         builder
         (asdf:registered-system system)
         rest))


(defmethod build ((builder t) (system symbol) &rest rest &key &allow-other-keys)
  (apply #'build
           builder
           (asdf:registered-system system)
           rest))


(defmethod build :around ((builder t) (system asdf:system) &rest rest &key &allow-other-keys)
  (log:info "Building docs for system ~A found at ~A"
            system
            (asdf:system-relative-pathname system ""))
  (let* ((primary-system
           (asdf:registered-system (asdf:primary-system-name system)))
         (special-config (or (docs-config system)
                             ;; You might be building docs for a subsystem,
                             ;; but specialize DOCS-CONFIG method on a primary system.
                             ;; Thats is why here we check both.
                             (unless (eql system primary-system)
                               (docs-config primary-system))))
         (dynamic-bindings (getf special-config :dynamic-bindings))
         (special-config (alexandria:remove-from-plist special-config
                                                       :dynamic-bindings))
         (params (append rest
                         ;; Config defined for ASDF system goes after the method
                         ;; params because explicit params from REST should have higher priority.
                         (when special-config
                           (log:info "Using config ~S" special-config)
                           special-config)))
         (result (progv
                     (mapcar #'car dynamic-bindings)
                     (mapcar #'cdr dynamic-bindings)
                   (apply #'call-next-method
                          builder
                          system
                          params))))
    (when (or (null result)
              (not (uiop:directory-exists-p result)))
      (error "BUILD method of (~S ~S) should return a pathname to a directory with HTML docs."
             builder system))
    result))
