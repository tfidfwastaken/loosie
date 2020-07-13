#lang racket/base

(require web-server/formlets
	 web-server/http/request-structs
         "model.rkt")

(provide upload-formlet passphrase-entry)
; upload-formlet: formlet (binding?)
(define upload-formlet
  (formlet
   (#%# (label "Passphrase (Optional) ") ,{input-string . => . pw}
        (label "File to upload ") ,{(file-upload) . => . binds})
   ; (formlet-process upload-formlet request)
   ; returns the file name and contents:
   (let* ([hashed-pw (get-password-hash pw)]
          [fname (bytes->string/utf-8 (binding:file-filename binds))]
          [mime-type (get-mime-type fname)]
          [access-code (make-access-code)]
          [fcontents (binding:file-content binds)])
     (cond
       [(equal? fname "") 'no-upload]
       [(not (or (equal? pw "") (valid-password? pw))) 'invalid-password]
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
