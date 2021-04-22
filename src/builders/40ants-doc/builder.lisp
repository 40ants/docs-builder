(uiop:define-package #:docs-builder/builders/40ants-doc/builder
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:log4cl)
  (:import-from #:docs-builder/utils
                #:system-packages)
  (:import-from #:40ants-doc
                #:section)
  (:import-from #:40ants-doc/full))
(in-package docs-builder/builders/40ants-doc/builder)


(defclass builder ()
  ())


(defun find-all-sections (system)
  (let ((packages (system-packages system)))
    (loop with results = nil
          for package in packages
          do (do-external-symbols (symbol package)
               (let ((value (and (boundp symbol)
                                 (symbol-value symbol))))
                 (when (and value
                            (typep value
                                   'section))
                   (push value results))))
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


(defun make-pages (root-sections system target-dir)
  (loop for section in root-sections
        collect (append
                 (list :objects (list section)
                       :source-uri-fn (make-github-source-uri-fn system))
                 ;; When there is only one root section there is no
                 ;; need to call it @index, we'll automatically
                 ;; make it's name index.html
                 (when (= (length root-sections) 1)
                   (list :output (list (uiop:merge-pathnames* "index.html" target-dir)
                                       :if-exists :supersede))))))


(defmethod docs-builder/builder:build ((builder builder) (system asdf:system))
  (let ((root-sections (find-root-sections system))
        (target-dir (asdf:system-relative-pathname system #P"docs/build/")))
    (cond
      (root-sections
       (log:info "Building docs in \"~A\" dir"
                 target-dir)

       (when (> (length root-sections) 1)
         (warn "Found more then one root section: ~S, probably you forgot to include one into another"
               root-sections))

       (cond
         ((probe-file (asdf:system-relative-pathname system "README.md"))
          (log:info "Updating README.md file")
          (40ants-doc/builder::update-asdf-system-readme root-sections
                                                         system
                                                         :format :markdown))
         ((probe-file (asdf:system-relative-pathname system "README"))
          (log:info "Updating README file")
          (40ants-doc/builder::update-asdf-system-readme root-sections
                                                         system
                                                         :format :plain))
         (t
          (log:info "No README files found.")))
       
       (40ants-doc/builder:update-asdf-system-html-docs
        root-sections
        system
        :target-dir target-dir
        :pages (make-pages root-sections system target-dir))
       
       (values target-dir))
      (t
       (log:error "Unable to find any documentation section in the ~S system"
                  (asdf:component-name system))
       (error "Unable to find any documentation section in the ~S system"
              (asdf:component-name system))))))
