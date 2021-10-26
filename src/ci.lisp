(defpackage #:docs-builder/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs))
(in-package docs-builder/ci)


(defparameter *asdf-version* "3.3.5.1"
  "At some point installation of the latest roswell version was broken:
   https://github.com/roswell/roswell/issues/497")


(defworkflow linter
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-version *asdf-version*)))


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/docs:build-docs
          ;; There are still a few warnings which should be shown.
          ;; And I have no time to fix this.
          :error-on-warnings nil
          :asdf-version *asdf-version*)))
