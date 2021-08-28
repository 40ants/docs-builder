(uiop:define-package #:docs-builder/builder
  (:use #:cl)
  (:import-from :log4cl)
  (:export #:build))
(in-package docs-builder/builder)


(defgeneric build (builder system &key &allow-other-keys)
  (:documentation "Builds HTML documentation for ASDF system and returns absolute path to the dir with docs."))


(defmethod build ((builder t) (system string) &rest rest &key &allow-other-keys)
  (apply #'build
         builder
         (asdf:find-system system)
         rest))


(defmethod build ((builder t) (system symbol) &rest rest &key &allow-other-keys)
  (apply #'build
           builder
           (asdf:find-system system)
           rest))


(defmethod build :around ((builder t) (system asdf:system) &key &allow-other-keys)
  (log:info "Building docs for system ~A found at ~A"
            system
            (asdf:system-relative-pathname system ""))
  (let ((result (call-next-method)))
    (when (or (null result)
              (not (uiop:directory-exists-p result)))
      (error "BUILD method of (~S ~S) should return a pathname to a directory with HTML docs."
             builder system))
    result))
