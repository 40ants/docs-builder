(defpackage #:docs-builder/builders/geneva/builder
  (:use #:cl)
  (:import-from #:geneva.html)
  (:import-from #:geneva.mk2)
  (:import-from #:docs-builder/builder))
(in-package docs-builder/builders/geneva/builder)


(defclass builder ()
  ())


(defmethod docs-builder/builder:build ((builder builder) (system asdf:system))
  (let* ((docs-source-dir
           (asdf:system-relative-pathname system
                                          "docs/source/"))
         (docs-output-dir
           (asdf:system-relative-pathname system
                                          "docs/build/"))
         (index-input-filename
           (uiop:merge-pathnames* docs-source-dir
                                  "index.mk2"))
         (index-output-filename
           (uiop:merge-pathnames* docs-output-dir
                                  "index.html"))
         (document (with-open-file (s index-input-filename)
                     (geneva.mk2:read-mk2 s))))
    (ensure-directories-exist docs-output-dir)
    
    (uiop:with-output-file (s index-output-filename
                              :if-exists :supersede)
      (geneva.html:render-html document :stream s)
      ;; Return a directory with resulting docs:
      docs-output-dir)))
