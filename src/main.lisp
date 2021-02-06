(defpackage #:docs-builder/main
  (:use #:cl)
  (:import-from #:docs-builder/api)
  (:import-from #:log4cl))
(in-package docs-builder/main)


(defun main (&rest argv)
  (let ((*terminal-io323* *error-output*))
    (unless argv
      (format *error-output* "Please, provide a system name.~%")
      (uiop:quit 1))

    (log:config :debug :sane2)

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
                  output-dir)
          ;; (when (>= (length argv)
          ;;           2)
          ;;   )
          )))))
