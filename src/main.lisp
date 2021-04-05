(defpackage #:docs-builder/main
  (:use #:cl)
  (:import-from #:docs-builder/core)
  (:import-from #:log4cl)
  (:import-from #:alexandria)
  (:import-from #:log4cl-extras)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled))
(in-package docs-builder/main)


(defun main (&rest argv)
  (unless argv
    (format *error-output* "Please, provide a system name.~%")
    (uiop:quit 1))

  (let ((*trace-output* *error-output*)
        (*terminal-io* *error-output*))
    (log:config :debug :sane)

    (let* ((system-name (first argv)))
      (log:info "Quickloading system ~S" system-name)
      (ql:quickload system-name
                    :silent t)

      (let* ((output-dir (ignore-errors
                          (with-log-unhandled ()
                            (docs-builder/core:build system-name)))))
        (unless output-dir
          (log:error "Unable to build docs")
          (uiop:quit 1))

        (let ((output-filename (second argv)))
          (when output-filename
            (alexandria:with-output-to-file (s output-filename
                                               :if-does-not-exist :create
                                               :if-exists :supersede)
              (format s "~A~%"
                      output-dir))))))))
