(defpackage #:docs-builder/core
  (:use #:cl)
  (:import-from #:docs-builder/api
                #:build
                #:make-builder)
  (:import-from #:log4cl))
(in-package docs-builder/core)


(defmethod make-builder ((system symbol))
  (make-builder (asdf:find-system system)))


(defmethod make-builder ((system string))
  (make-builder (asdf:find-system system)))


(defmethod make-builder ((system asdf:system))
  (loop for factory in docs-builder/api::*builder-factories*
          thereis (funcall factory
                           system)
        finally (error "Unable to create documentation builder for system ~S"
                       system)))


(defmethod build ((builder t) (system string))
  (build builder (asdf:find-system system)))


(defmethod build ((builder t) (system symbol))
  (build builder (asdf:find-system system)))


(defmethod build :before ((builder t) (system asdf:system))
  (log:info "Building docs for system ~A found at ~A"
            system
            (asdf:system-relative-pathname system "")))
