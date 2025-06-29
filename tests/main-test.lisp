(defpackage :cl-google-api/main-test
  (:use :cl)
  (:import-from :cl-google-api)
  (:import-from :rove))
(in-package :cl-google-api/main-test)

(defclass mock-net-transport (cl-google-api::net-transport) ())

(defmethod cl-google-api::request-post ((this mock-net-transport) url content &key headers)

  "{\"access_token\":\"token\",\"refresh_token\":\"refresh-token\",\"expires_in\":3600}")

(defclass mock-auth (cl-google-api::auth-server)
  ((cl-google-api::token-url :initform "mock-token-url")))

(rove:deftest test-is-expired
  (rove:testing "should is-expired-p be true when fresh oauth2 instance is created"
   (let ((o (make-instance 'cl-google-api:oauth2)))
     (rove:ok (cl-google-api::is-expired-p o))))

  (rove:testing "should is-expired-p be false after token is refreshed"
    (let* ((mock-net (make-instance 'mock-net-transport))
           (mock-auth (make-instance 'mock-auth :net-transport mock-net))
           (o (make-instance 'cl-google-api:oauth2
                            :auth-server mock-auth
                            :client-id "client-id"
                            :client-secret "client-secret"
                            :refresh-token "refresh-token")))
      ;; when
      (cl-google-api::do-refresh-token o)

      ;; then
      (rove:ok (= (cl-google-api::expires-in o) 3600))
      (rove:ok (local-time:timestamp>= (local-time:now)
                                       (cl-google-api::start-date o)))
      (rove:ok (not (cl-google-api::is-expired-p o))))))
