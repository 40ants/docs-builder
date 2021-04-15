(defpackage #:docs-builder/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:docs-builder/core)
  (:import-from #:log4cl)
  (:import-from #:alexandria)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/config))
(in-package docs-builder/main)


(defmain main ((error-on-warnings "If documentation will generate any warnings, build will fail. This can be \"true\" or \"false\"."
                                  :default "true")
               &rest systems)
  (unless systems
    (format *error-output* "Please, provide a system name.~%")
    (uiop:quit 1))

  (let ((*trace-output* *error-output*)
        (*terminal-io* *error-output*)
        (error-on-warnings (string-equal error-on-warnings
                                         "true")))
    
    (log4cl-extras/config:setup
      '(:level :debug
        :appenders ((this-console :layout :plain))))

    (let* ((system-name (first systems)))
      (log:info "Quickloading system ~S" system-name)
      (ql:quickload system-name
                    :silent t)

      (let* ((output-dir (ignore-errors
                          (with-log-unhandled ()
                            (docs-builder/core:build system-name
                                                     :error-on-warnings error-on-warnings)))))
        (unless output-dir
          (log:error "Unable to build docs")
          (uiop:quit 1))

        (let ((output-filename (second systems)))
          (when output-filename
            (alexandria:with-output-to-file (s output-filename
                                               :if-does-not-exist :create
                                               :if-exists :supersede)
              (format s "~A~%"
                      output-dir))))))))
