(defpackage #:docs-builder/main
  (:use #:cl)
  (:import-from #:docs-builder/api)
  (:import-from #:log4cl))
(in-package docs-builder/main)


(defun main (&rest argv)
  (unless argv
    (format *error-output* "Please, provide a system name.~%")
    (uiop:quit 1))

  (let ((*trace-output* *error-output*)
        (*terminal-io* *error-output*))
    (log:config :debug)

    (let* ((system-name (first argv)))
      (log:info "Quickloading system ~S" system-name)
      (ql:quickload system-name
                    :silent t)

      (let* ((builder (docs-builder/api:make-builder system-name))
             (output-dir
               (docs-builder/api:build builder
                                       system-name)))

        (when output-dir
          (format t "~A~%"
                  output-dir))))))
