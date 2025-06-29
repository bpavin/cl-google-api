(defpackage cl-google-api
  (:use :cl)
  (:import-from :log4cl)
  (:import-from :dexador)
  (:import-from :cl-json)
  (:import-from :bt-semaphore)

  (:export :oauth2
           :obtain-callback-url
           :obtain-token-from-callback-url
           :get-token))
(in-package :cl-google-api)

(defclass net-transport () ())

(defgeneric request-post (this url content &key headers)
  (:method ((this net-transport) url content &key headers)
    (log:trace "Request [url=~A, content=~A]" url content)
    (let ((response (dex:post url :content content :headers headers)))
      (log:trace "Response [url=~A, response=~A]" url response)
      response)))

(defclass auth-server ()
  ((net-transport :initarg :net-transport :accessor net-transport
                  :initform (make-instance 'net-transport))

   (auth-url :initarg :auth-url :accessor auth-url)
   (token-url :initarg :token-url :accessor token-url)))

(defmethod obtain-code ((this auth-server) (client-id string) (redirect-uri string) (scopes list) &key (get-refresh-token-p T))
  (let* ((url (format nil "~A?client_id=~A&redirect_uri=~A&response_type=code&scope=~A&access_type=~A&state=~A"
                      (auth-url this)
                      client-id
                      (quri:url-encode redirect-uri)
                      (if get-refresh-token-p "offline" "online")
                      (format nil "~{~A~^ ~}" scopes)
                      "1a2b3cd4")))
    (log:info "Paste URL to your browser:~%~A" url)
    (log:info "Call '(obtain-token-from-callback-url ((this oauth2) (callback-url string)))' method to get access token.")))

(defmethod obtain-token-with-code ((this auth-server) (code string) (client-id string) (client-secret string)
                                   (redirect-uri string))
  (obtain-token this
                (format nil "code=~A&client_id=~A&client_secret=~A&redirect_uri=~A&grant_type=authorization_code"
                        code
                        client-id
                        client-secret
                        (quri:url-encode redirect-uri))))

(defmethod obtain-token-with-refresh-token ((this auth-server) (refresh-token string) (client-id string) (client-secret string))
  (obtain-token this
                (format nil "refresh_token=~A&client_id=~A&client_secret=~A&grant_type=refresh_token"
                        refresh-token
                        client-id
                        client-secret)))

(defmethod obtain-token ((this auth-server) (body string))
  (request-post (net-transport this)
                (token-url this)
                body :headers '(("Content-Type" . "application/x-www-form-urlencoded"))))

(defclass google-auth-server (auth-server)
  ((auth-url :initarg :auth-url :accessor auth-url
             :initform "https://accounts.google.com/o/oauth2/auth" )
   (token-url :initarg :token-url :accessor token-url
              :initform "https://oauth2.googleapis.com/token")))

;;----------------------------------------------------------------------------------------------------
;; Oauth2 logic
;;----------------------------------------------------------------------------------------------------

(defclass oauth2 ()
  ((lock :accessor lock :initform (bt:make-lock))
   (auth-server :initarg :auth-server :accessor auth-server
                :initform (make-instance 'google-auth-server))

   (client-id :initarg :client-id :accessor client-id)
   (client-secret :initarg :client-secret :accessor client-secret)
   (redirect-uri :initarg :redirect-uri :accessor redirect-uri)
   (scopes :type list :initarg :scopes :accessor scopes)

   (token :initarg :token :accessor token)
   (refresh-token :initarg :refresh-token :accessor refresh-token)
   (expires-in :initarg :expires-in :accessor expires-in
               :initform 0)
   (start-date :initarg :start-date :accessor start-date
               :initform (local-time:encode-timestamp 0 0 0 0 1 1 1970))))

(defmethod obtain-callback-url ((this oauth2))
  (obtain-code (auth-server this) (client-id this) (redirect-uri this) (scopes this)))

(defmethod obtain-token-from-callback-url ((this oauth2) (callback-url string))
  (let* ((code (get-code-from-url this callback-url)))
    (obtain-token-with-code (auth-server this) code
                            (client-id this) (client-secret this) (redirect-uri this))))

(defmethod get-code-from-url ((this oauth2) (callback-url string))
  (let* ((args (cl-ppcre:split "&" (string-left-trim (redirect-uri this) callback-url)))
         (code (find-if (lambda (x)
                         (search "code=" x :test #'string=))
                       args)))
    (if code
        (cadr (cl-ppcre:split "=" code))
        (error 'code-not-found-in-callback-url))))

(defmethod get-token ((this oauth2))
  (if (is-expired-p this)
      (get-token/lock this))
  (token this))

(defmethod get-token/lock ((this oauth2))
  (bt:with-lock-held ((lock this))
    (if (is-expired-p this)
        (do-refresh-token this))
    (token this)))

(defmethod is-expired-p ((this oauth2))
  (local-time:timestamp> (local-time:now)
                         (local-time:timestamp+ (start-date this) (expires-in this) :sec)))

(defmethod do-refresh-token ((this oauth2))
  (let ((new-token (obtain-token-with-refresh-token
                    (auth-server this) (refresh-token this) (client-id this) (client-secret this))))
    (from-json this new-token)))

(defmethod from-json ((this oauth2) (json string))
  (let ((token-alist (cl-json:decode-json-from-string json)))
    (setf
     (token this) (aget token-alist :access--token)
     (expires-in this) (aget token-alist :expires--in)
     (scopes this) (aget token-alist :scope)
     (start-date this) (local-time:now))))

(defun aget (alist item)
  (cdr (assoc item alist)))
