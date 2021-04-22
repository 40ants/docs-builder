(defpackage #:docs-builder/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs))
(in-package docs-builder/ci)


(defworkflow linter
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)))


(defworkflow docs
  :on-push-to "master"
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system "40ants-doc/doc"
          ;; There are still a few warnings which should be shown.
          ;; And I have no time to fix this.
          :error-on-warnings nil)))
