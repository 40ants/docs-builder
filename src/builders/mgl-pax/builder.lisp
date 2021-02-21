(defpackage #:docs-builder/builders/mgl-pax/builder
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:mgl-pax)
  (:import-from #:log4cl))
(in-package docs-builder/builders/mgl-pax/builder)


(defclass builder ()
  ())


(defun find-main-section (system)
  (let* ((system-name (asdf:primary-system-name system))
         (package-name (string-upcase system-name))
         (package (find-package package-name)))
    ;; TODO: Probably we need to do something smarter here:     
    (cond
      (package
        (let ((symbol (intern "@INDEX" package)))
          (when (boundp symbol)
            (symbol-value symbol))))
      (t
       (log:warn "No package ~S found"
                 package-name)))))


(defun make-github-source-uri-fn (system)
  (let ((url (or (second (ignore-errors (asdf:system-source-control system)))
                 (ignore-errors (asdf:system-homepage system)))))
    (when (and url
               (str:starts-with-p "https://github.com/" url))
      (mgl-pax:make-github-source-uri-fn system url))))


(defun make-pages (main-section system)
  (list (list :objects (list main-section)
              :source-uri-fn (make-github-source-uri-fn system))))


(defmethod docs-builder/builder:build ((builder builder) (system asdf:system))
  (let ((main-section (find-main-section system))
        (target-dir (uiop:merge-pathnames* #P"docs/build/")))
    (cond
      (main-section
       (log:info "Building docs in \"~A\" dir"
                 target-dir)

       (mgl-pax:update-asdf-system-readmes main-section
                                           system)
       
       (mgl-pax:update-asdf-system-html-docs
        main-section
        system
        :target-dir target-dir
        :pages (make-pages main-section system))
       
       (values target-dir))
      (t
       (error "Unable to find @INDEX section in the ~S package"
              (asdf:component-name system))))))
