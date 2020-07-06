#lang web-server/base

(require racket/list
         racket/file
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
(define (render-uploader request)
  (define (upload-handler request)
    (define-values (fname fcontents)
      (formlet-process upload-formlet request))
    (current-directory "/home/atharva/loosie/")
    (display-to-file fcontents "test.html" #:exists 'replace)
    (response/xexpr "SUCCESS"))

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
              (input ([type "submit"] [value "Upload"])))))))

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
          [fcontents (binding:file-content binds)])
     (make-loosie #:name fname
                  #:mime-type mime-type
                  #:content fcontents
                  #:passphrase hashed-pw))))

 ; start: request -> doesn't return
(define (start request)
  (render-uploader request))

(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port)
