# Cl-Google-Api

## Obtain Google API credentials

1. Visit https://console.cloud.google.com/apis/credentials
1. Select or create new project
1. Create credentials
1. Choose: OAuth client ID
1. Choose: Application type -> Web application
1. Enter Name
1. Enter redirect URI
1. Click: Create
1. Click: Download JSON

Enable Gmail API:
1. Visit https://console.cloud.google.com/apis/library
1. Enter search: Gmail api
1. Choose Gmail API
1. Enable

## Google OAuth flow for web-applications
https://developers.google.com/identity/protocols/oauth2/javascript-implicit-flow

## Usage

``
(setf *oauth2-client* 
    (make-instance 'cl-google-api:oauth2 
                    :client-id "your_client_id" 
                    :client-secret "your_client_secret"
                    :redirect-uri "your_redirect_uri" 
                    :scopes '("https://www.googleapis.com/auth/gmail.send")))
``

Enter in repl:
`` 
(cl-google-api:obtain-callback-url *oauth2-client*)
`` 
this will generate URL which must be pasted to browser. (Browser won't automatically open, manual operation is needed.)

After authentication is done in browser, browser will redirected to defined redirect_uri link.  
(Again, manual operation is needed.)  
Copy link from browser to repl:
`` 
(cl-google-api:obtain-token-from-callback-url *oauth2-client*  "http://urlfrombrowser/getCode?code=123123&scope=https://www.googleapis.com/auth/gmail.send")
``
This should produce valid token:
``
{
  "access_token": "ya29.....",
  "expires_in": 3600,
  "refresh_token": "1//03A0Dw9Rsn.....",
  "scope": "https://www.googleapis.com/auth/gmail.send",
  "token_type": "Bearer"
}"
``

Refresh token can now be used on server side to call gmail.send API.  
Refresh token doesn't expire, so it can be used unlimited number of times, as long as the access is not revoked.
``
(let* ((oauth (make-instance 'cl-google-api:oauth2 
                             :client-id "your_client_id"
                             :client-secret "your_client_secret"
                             :refresh-token "1//03A0Dw9Rsn....."))
       (bearer-token (cl-google-api:get-token oauth))
       (msg (base64:string-to-base64-string 
"From: from@from.from
To:  to@to.to
Subject: subject

message

")))
    (dex:post "https://gmail.googleapis.com/gmail/v1/users/me/messages/send"
            :headers (list (cons "Authorization" (format nil "Bearer ~A" bearer-token)))
            :content (format nil "{\"raw\":\"~A\"}" msg)))
``


## Installation

