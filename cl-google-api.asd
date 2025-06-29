(defsystem "cl-google-api"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (#:log4cl #:dexador #:defstar #:defclass-std #:cl-json
                        #:bt-semaphore)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-google-api/tests"))))

(defsystem "cl-google-api/tests"
  :author ""
  :license ""
  :depends-on ("cl-google-api"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main-test"))))
  :description "Test system for cl-google-api"
  :perform (test-op (op c) (symbol-call :rove :run c)))
