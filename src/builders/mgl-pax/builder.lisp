(mgl-pax-minimal:define-package #:docs-builder/builders/mgl-pax/builder
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:mgl-pax)
  (:import-from #:log4cl)
  (:import-from #:docs-builder/utils
                #:system-packages)
  (:import-from #:mgl-pax-minimal
                #:section))
(in-package docs-builder/builders/mgl-pax/builder)


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
                           (typep obj 'mgl-pax-minimal:reference))
                         (mgl-pax-minimal:section-entries section)))))
    (loop for section in sections
          for section-name = (mgl-pax-minimal:section-name section)
          when (and
                (not (member section-name
                             references
                             :key #'mgl-pax-minimal:reference-object))
                ;; MGL-PAX package includes this section
                ;; to collect all other sections into the "World":
                ;; @mgl-pax-world-dummy
                ;; It shouldn't be included into the documentation.
                (not (string-equal (symbol-name section-name)
                                   "@mgl-pax-world-dummy")))
            collect section)))


(defun make-github-source-uri-fn (system)
  (let ((url (or (second (ignore-errors (asdf:system-source-control system)))
                 (ignore-errors (asdf:system-homepage system)))))
    (when (and url
               (str:starts-with-p "https://github.com/" url))
      (mgl-pax:make-github-source-uri-fn system url))))


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

       (log:info "Found these root sections:" root-sections)

       (mgl-pax:update-asdf-system-readmes root-sections
                                           system)
       
       (mgl-pax:update-asdf-system-html-docs
        root-sections
        system
        :target-dir target-dir
        :pages (make-pages root-sections system target-dir))
       
       (values target-dir))
      (t
       (log:error "Unable to find any MGL-PAX section in the ~S system"
                  (asdf:component-name system))
       (error "Unable to find any MGL-PAX section in the ~S system"
              (asdf:component-name system))))))
