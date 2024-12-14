(uiop:define-package #:docs-autobuilder
  (:use #:cl)
  (:import-from #:lack.component)
  (:import-from #:lack.app.file)
  (:import-from #:usocket)
  (:import-from #:clack)
  (:import-from #:bordeaux-threads)
  (:import-from #:docs-builder)
  (:import-from #:fs-watcher)
  (:import-from #:trivial-open-browser
                #:open-browser)
  (:import-from #:alexandria
                #:remove-from-plistf)
  (:export #:build
           #:stop))
(in-package #:docs-autobuilder)


(defvar *app* nil)

(defvar *server* nil)

(defvar *thread* nil)


(defun port-available-p (port interface)
  (handler-case (let ((socket (usocket:socket-listen interface port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))


(defun available-port (interface)
  "Return a port number not in use from 8000 to 60000."
  (loop for port from 8000 upto 60000
        if (port-available-p port interface)
          return port))


(defun serve-docs (root env)
  (let* ((path-info (string-left-trim (list #\/)
                                      (getf env :path-info)))
         (path (if (uiop:directory-pathname-p path-info)
                   (merge-pathnames "index.html" path-info)
                   path-info))
         (full-path (merge-pathnames path root)))
    (if (probe-file full-path)
        (lack.component:call (make-instance 'lack.app.file:lack-app-file
                                            :root root
                                            :file path)
                             env)
        (list 404
              (list :content-type "text/plain")
              (list (format nil "File ~A not found."
                            full-path))))))


(defun make-app (root)
  (flet ((docs-server-app (env)
           (serve-docs root env)))
    #'docs-server-app))


(defun in-subdir-p (root file)
  (let ((root (namestring root))
        (file (namestring file)))
    (and (> (length file)
            (length root))
         (string-equal root
                       (subseq file 0 (length root))))))


(defun build (system &rest rest-args
                     &key in-thread port (interface "localhost")
                     &allow-other-keys)
  (when *server*
    (error "Server already running."))

  (remove-from-plistf rest-args
                      :in-thread
                      :port
                      :interface)

  (let* ((system-path (asdf:system-relative-pathname system "./"))
         (docs-path (handler-bind ((docs-builder:documentation-has-problems
                                     (lambda (c)
                                       (let ((restart (find-restart 'continue c)))
                                         (when restart
                                           (invoke-restart restart))))))
                      (docs-builder:build system)))
         (port (or port
                   (available-port interface)))
         (app (make-app docs-path))
         (server (progn
                   (log:info "Starting Clack server to serve docs from ~A" docs-path)
                   (clack:clackup app
                                  :port port
                                  :address interface)))
         (url (format nil "http://~A:~A/"
                      interface port)))
    
    (with-simple-restart (skip-opening-the-browser "Skip opening the browser")
      (open-browser url))
    
    (labels ((build-system (changed-file)
               (cond
                 ((or (in-subdir-p docs-path changed-file)
                      (string-equal (pathname-name changed-file)
                                    "README")
                      (string-equal (pathname-name changed-file)
                                    "ChangeLog"))
                  (log:debug "File ~A was changed, but it is in the documentation folder, skipping docs build step."
                             changed-file))
                 (t
                  (log:info "File ~A was changed. Rebuilding the docs of ~A system."
                            changed-file system)
                  (handler-case
                      (progn
                        (ql:quickload system)
                        (apply #'docs-builder:build system
                               rest-args))
                    (docs-builder:documentation-has-problems (c)
                      (log:error "Unable to build docs for ~A system. ~A"
                                 system c))
                    (error ()
                      (log:error "Unable to build docs for ~A system."
                                 system))))))
             (run-docs-autobuilder ()
               (fs-watcher:watch system-path #'build-system)))
      (cond
        (in-thread
         (setf *app* app)
         (setf *server* server)
         (setf *thread*
               (bordeaux-threads:make-thread #'run-docs-autobuilder
                                             :name (format nil "Docs Autobuilder for ~A: ~A"
                                                           system url))))
        (t
         (unwind-protect
              (run-docs-autobuilder)
           (clack:stop server))))

      (values))))


(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil
          *app* nil))
  (when *thread*
    (when (bordeaux-threads:thread-alive-p *thread*)
      (bordeaux-threads:destroy-thread *thread*))
    (setf *thread* nil))
  (values))
