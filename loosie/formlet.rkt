#lang racket/base

(require web-server/formlets
	 web-server/http/request-structs
         racket/port
         "utils.rkt"
         "model.rkt")

(provide upload-formlet passphrase-entry)
; upload-formlet: formlet (binding?)
(define upload-formlet
  (formlet
   (#%#
    (div ([class "row"])
         (label "Passphrase (Optional) ")
         ,((to-string
            (required
             (password-input
              #:attributes '([autocomplete "new-password"] [novalidate "novalidate"]
                             [id "password-field"]
                             [pattern "^[ 0-9A-Za-z!@#$%^&*()]{6,32}$"]))))
           . => . pw)
         (span ([class "noselect"] [onclick "viewPassword()"]) " 👁 "))

    (div ([class "row"])
         ,{(file-upload #:attributes '([id "file"] [required "required"])) . => . binds}
         (label ([class "button"] [for "file"]) "Select a File...")))

   ; (formlet-process upload-formlet request)
   ; returns the file name and contents:
   (let* ([hashed-pw (get-password-hash pw)]
          [fname (bytes->string/utf-8 (binding:file-filename binds))]
          [mime-type (get-mime-type fname)]
          [access-code (make-access-code)]
          [fcontents (binding:file-content binds)]
          [has-pw (if (equal? pw "") #f #t)])
     (cond
       [(equal? fname "") 'no-upload]
       [(not (or (equal? pw "") (valid-password? pw))) 'invalid-password]
       [else 
        (begin
          (define-values (enc-fcontents wrap)
            (encrypt-data fcontents pw))
          (define serialized-wrap (with-output-to-bytes
                                    (λ () (write wrap))))
          (make-loosie #:name            fname
                       #:mime-type       mime-type
                       #:content         enc-fcontents
                       #:access-code     access-code
                       #:passphrase      hashed-pw
                       #:pass-protected? has-pw
                       #:wrap            serialized-wrap))]))))

(define passphrase-entry
  (formlet
   (#%# (label "Enter passphrase to access: ")
    ,((to-string
       (required
        (password-input
         #:attributes '([autocomplete "new-password"] [id "password-field"]))))
       . => . pw)
    (span ([class "noselect"] [onclick "viewPassword()"]) " 👁 "))
   pw))
