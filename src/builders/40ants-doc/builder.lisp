(uiop:define-package #:docs-builder/builders/40ants-doc/builder
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:log4cl)
  (:import-from #:docs-builder/utils
                #:system-packages)
  (:import-from #:40ants-doc
                #:section)
  (:import-from #:40ants-doc/full)
  (:import-from #:alexandria
                #:remove-from-plistf
                #:when-let))
(in-package docs-builder/builders/40ants-doc/builder)


(defclass builder ()
  ())


(defun find-all-sections (system)
  (let ((packages (system-packages system)))
    (unless packages
      (error "No packages with names like \"~A\""
             (string-upcase (asdf:primary-system-name system))))
    
    (loop with results = nil
          for package in packages
          do (do-external-symbols (symbol package)
               (let ((value (and (boundp symbol)
                                 (symbol-value symbol))))
                 (when (and value
                            (typep value
                                   'section))
                   (pushnew value results))))
          finally (return results))))


(defun find-root-sections (system)
  (let* ((sections (find-all-sections system))
         (references
           (loop for section in sections
                 append (remove-if-not
                         (lambda (obj)
                           (typep obj '40ants-doc/reference::reference))
                         (40ants-doc:section-entries section)))))
    (loop for section in sections
          for section-name = (40ants-doc:section-name section)
          when (not (member section-name
                            references
                            :key #'40ants-doc/reference::reference-object))
          collect section)))


(defun make-github-source-uri-fn (system)
  (let ((url (or (second (ignore-errors (asdf:system-source-control system)))
                 (ignore-errors (asdf:system-homepage system)))))
    (when (and url
               (str:starts-with-p "https://github.com/" url))
      (40ants-doc/github:make-github-source-uri-fn system url))))


(defmethod docs-builder/builder:build ((builder builder) (system asdf:system)
                                       &rest rest
                                       &key local root-sections)
  (remove-from-plistf rest :local :root-sections)
  
  (let* ((user-specified-root-sections (loop for item in root-sections
                                             collect (typecase item
                                                       (symbol (symbol-value item))
                                                       (t item))))
         (root-sections (or user-specified-root-sections
                            (find-root-sections system)))
         (target-dir (asdf:system-relative-pathname system #P"docs/build/")))
    
    (cond
      (root-sections
       (log:info "Building docs in \"~A\" dir"
                 target-dir)

       (flet ((readme-section-p (section)
                (string-equal
                 (symbol-name (40ants-doc:section-name section))
                 "@README"))
              (changelog-section-p (section)
                (string-equal
                 (symbol-name (40ants-doc:section-name section))
                 "@CHANGELOG")))
         (let ((doc-sections
                 (loop for section in root-sections
                       unless (or (readme-section-p section)
                                  (changelog-section-p section))
                         collect section))
               (readme-sections (remove-if-not #'readme-section-p
                                               root-sections))
               (changelog-section (when-let ((sections (remove-if-not #'changelog-section-p
                                                                      root-sections)))
                                    (unless (= (length sections) 1)
                                      (error "More than one @CHANGELOG section were found"))
                                    (first sections))))
           (when (and (> (length doc-sections)
                         1)
                      (not user-specified-root-sections))
             (warn "Found more then one root section: ~S, probably you forgot to include one into another"
                   doc-sections))

           (apply #'40ants-doc/builder:update-asdf-system-docs
                  (append doc-sections
                          ;; We want to include changelog into the HTML documentation
                          ;; and markdown version will be built because of :CHANGELOG-SECTIONS argument
                          (when changelog-section
                            (list changelog-section)))
                  system
                  :readme-sections readme-sections
                  :changelog-sections changelog-section
                  :docs-dir target-dir
                  :base-url (unless local
                              (asdf/system:system-homepage system))
                  ;; When we are building docs for local usage,
                  ;; we don't want to trim index.html from urls.
                  :clean-urls (not local)
                  rest)
       
           (values target-dir))))
      (t
       (log:error "Unable to find any documentation section in the ~S system. Export @index, @readme and @changelog 40ANTS-DOC:SECTION's please."
                  (asdf:component-name system))
       (error "Unable to find any documentation section in the ~S system.~%Export @index, @readme and @changelog 40ANTS-DOC:SECTION's please."
              (asdf:component-name system))))))
