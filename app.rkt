#lang web-server/base

(require racket/list
         racket/file
         racket/match
	 web-server/http
	 web-server/stuffers
	 web-server/page
	 web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/dispatch
         xml
         "model.rkt")

(module+ test
    (require rackunit))

(provide interface-version)
(define interface-version 'stateless)

; render-uploader: request -> doesn't return
(define (render-uploader db #:status [status ""] request)
  ; renders the upload page
  (define (response-generator embed/url)
    (response/xexpr
     `(html 
       (head (title "loosie"))
       (body (h1 "LOOSIE")
             (form 
              ([action ,(embed/url upload-handler)]
               [method "POST"]
               [enctype "multipart/form-data"])
              ,@(formlet-display upload-formlet)
              (input ([type "submit"] [value "Upload"])))
             (div ([class "status"]) ,status)))))

  (define (upload-handler request)
    (define loosie-data
      (formlet-process upload-formlet request))
    (match loosie-data
      ['invalid-password
       (let ([failure-msg
              (string-append "Password must be between 8 and 32 characters"
                             " and can only contain numbers, letters and"
                             " the following special characters: !@#$%^&*()")])
         (render-uploader db #:status failure-msg request))]
      ['no-upload
       (let ([failure-msg "Please upload a file."])
         (render-uploader db #:status failure-msg request))]
      [else
       (render-upload-success db loosie-data request)]))
       
(send/suspend/dispatch response-generator))

; render-upload-success -> doesn't return
(define (render-upload-success db loosie request)
  (define (response-generator embed/url)
    (upload-file db loosie)
    (response/xexpr
     `(html
       (head (title "loosie - success"))
       (body (h1 "Success!")
             (p ,(string-append "Your link is: "
                                (access-code->url-string
                                 (loosie-access-code loosie) request)))
             (a ([href ,(embed/url home-handler)]) "« Back to home")))))
  
  (define (access-code->url-string code request)
    (string-append app-url "/" code))
  
  (define (home-handler request)
    (render-uploader db request))

  (send/suspend/dispatch response-generator))

; upload-formlet: formlet (binding?)
(define upload-formlet
  (formlet
   (div "Passphrase (Optional)" ,{input-string . => . pw}
        "File to upload" ,{(file-upload) . => . binds})
   ; (formlet-process upload-formlet request)
   ; returns the file name and contents:
   (let* ([hashed-pw (get-password-hash pw)]
          [fname (bytes->string/utf-8 (binding:file-filename binds))]
          [mime-type (get-mime-type fname)]
          [access-code (make-access-code)]
          [fcontents (binding:file-content binds)])
     (cond
       [(equal? fname "") 'no-upload]
       [(not (valid-password? pw)) 'invalid-password]
       [else 
        (make-loosie #:name            fname
                     #:mime-type       mime-type
                     #:content         fcontents
                     #:access-code     access-code
                     #:passphrase      hashed-pw
                     #:pass-protected? (if (equal? pw "") #f #t))]))))

(define passphrase-entry
  (formlet
   (div "Enter passphrase to access: " ,{input-string . => . pw})
   pw))

 ; start-upload: request -> doesn't return
(define (start-upload request)
  (define db (init-db))
  (render-uploader db request))

(define (file-not-found-handler request)
  (response/xexpr
   `(html
     (head (title "loosie not found")
           (body (h1 "The loosie you requested does not exist")
                 (p (a ([href "/"]) "« go home")))))))

; displays retrieved loosie
(define (render-loosie loosie request)
  (define mime-type (loosie-mime-type loosie))
  (response/output
   (λ (op) (write-bytes (loosie-content loosie) op))
   #:code 200
   #:mime-type (if (equal? mime-type 'unknown) #""
                   mime-type)))

(define (render-pass-form #:status [status ""] loosie request)
  (define (response-generator embed/url)
    (response/xexpr 
     `(html 
       (head (title "loosie - verify"))
       (body (h1 "LOOSIE")
             (form 
              ([action ,(embed/url password-submit-handler)]
               [method "POST"]
               [enctype "multipart/form-data"])
              ,@(formlet-display passphrase-entry)
              (input ([type "submit"] [value "Submit"])))
             (div ([class "status"]) ,status)))))

  (define (password-submit-handler request)
    (define pw (formlet-process passphrase-entry request))
    (define pwh (loosie-passphrase loosie))
    (cond
      [(password-matches? pw pwh) (render-loosie loosie request)]
      [else
       (let ([failed-msg "Verification failed. Try again."])
         (render-pass-form #:status failed-msg loosie request))]))

    (send/suspend/dispatch response-generator))


; get-content: request -> doesn't return
(define (get-content request)
  (define db (init-db))
  (define access-code
    (path/param-path (car (url-path (request-uri request)))))
  (define loosie (get-file-data db access-code))
  (if (not loosie) (file-not-found-handler request)
      (match (loosie-pass-protected? loosie)
        [#f (render-loosie loosie request)]
        [#t (render-pass-form loosie request)]
        [_ (error "should never get here. if it did then i cri")])))

; dispatches based on request
(define-values (start reverse-uri)
  (dispatch-rules
   [("") #:method "get" start-upload]
   [else get-content]))

(serve/servlet start
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:listen-ip #f
               #:port port)
