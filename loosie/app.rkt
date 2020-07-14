#lang web-server/base

(require racket/list
         racket/file
         racket/runtime-path
         racket/match
	 web-server/http
	 web-server/stuffers
	 web-server/page
	 web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/dispatch
         web-server/templates
         web-server/configuration/responders
         xml
         "model.rkt"
         "formlet.rkt")

(module+ test
    (require rackunit))

(provide interface-version)
(define interface-version 'stateless)

; render-uploader: request -> doesn't return
(define (render-uploader db #:status [status ""] request)
  ; renders the upload page
  (define (response-generator embed/url)
    (response/full
     200 #f
     (current-seconds) TEXT/HTML-MIME-TYPE '()
     (list (string->bytes/utf-8 (include-template "static/upload.html")))))

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
  ; renders the success page
  (define (response-generator embed/url)
    (upload-file db loosie)
    (define access-link
      (access-code->url-string
       (loosie-access-code loosie) request))
    (response/full
     200 #f
     (current-seconds) TEXT/HTML-MIME-TYPE '()
     (list (string->bytes/utf-8 (include-template "static/success.html")))))
 
  (define (access-code->url-string code request)
    (string-append app-url "/l/" code))
  
  (define (home-handler request)
    (render-uploader db request))

  (send/suspend/dispatch response-generator))

 ; start-upload: request -> doesn't return
(define (start-upload request)
  (define db (init-db))
  (render-uploader db request))

(define (file-not-found-handler request)
  (response/full
   404 #f
   (current-seconds) TEXT/HTML-MIME-TYPE '()
   (list (string->bytes/utf-8 (include-template "static/file-not-found.html")))))

; displays retrieved loosie
(define (render-loosie loosie #:password [pw ""] request)
  (define mime-type (loosie-mime-type loosie))
  (define wrap
    (read (open-input-bytes (loosie-wrap loosie))))
  (define enc-content (loosie-content loosie))
  (define content (decrypt-data enc-content wrap pw))
  (response/output
   (λ (op) (write-bytes content op))
   #:code 200
   #:mime-type (if (equal? mime-type #"unknown") #""
                   mime-type)))

(define (render-pass-form #:status [status ""] loosie request)
  (define (response-generator embed/url)
    (response/full
     200 #f
     (current-seconds) TEXT/HTML-MIME-TYPE '()
     (list (string->bytes/utf-8 (include-template "static/verify.html")))))

  (define (password-submit-handler request)
    (define pw (formlet-process passphrase-entry request))
    (define pwh (loosie-passphrase loosie))
    (cond
      [(password-matches? pw pwh) (render-loosie loosie #:password pw request)]
      [else
       (let ([failed-msg "Verification failed. Try again."])
         (render-pass-form #:status failed-msg loosie request))]))

    (send/suspend/dispatch response-generator))


; get-content: request -> doesn't return
(define (get-content request access-code)
  (define db (init-db))
  (define loosie (get-file-data db access-code))
  (if (not loosie) (file-not-found-handler request)
      (match (loosie-pass-protected? loosie)
        [#f (render-loosie loosie request)]
        [#t (render-pass-form loosie request)]
        [_ (error "should never get here. if it did then i cri")])))

(define (not-found request)
  (response/full
   404 #f
   (current-seconds) TEXT/HTML-MIME-TYPE '()
   (list (string->bytes/utf-8 (include-template "static/404.html")))))

(define (serve-css request filename)
  (file-response 200 #"OK" filename))

(define-runtime-path htdocs-path "htdocs")

; dispatches based on request
(define-values (start reverse-uri)
  (dispatch-rules
   [("") #:method "get" start-upload]
   [("l" (string-arg)) get-content]))

(serve/servlet start
               #:extra-files-paths (list htdocs-path)
               #:servlet-path ""
               #:servlet-regexp #rx""
               #:file-not-found-responder not-found
               #:listen-ip #f
               #:launch-browser? #f
               #:port port)
