#lang racket/base

(require racket/match
         db database-url
         deta
         net/base64
         crypto
         crypto/libcrypto
         racket/string)

(provide init-db port app-url
         (schema-out loosie)
         upload-file get-mime-type
         make-access-code
         get-password-hash verify-password)

(crypto-factories (list libcrypto-factory))

; A loosie is a text-based file we want to share
(define-schema loosie
  ([id id/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]
   [mime-type symbol/f]
   [content binary/f]
   [access-code string/f #:contract non-empty-string?]
   [passphrase string/f]
   [pass-protected? boolean/f #:contract (or #t #f)]))

(define (init-db)
 ; make-pg-connection : (-> connection)
  (define make-pg-connection
    (let ([db-url (getenv "DATABASE_URL")])
      (database-url-connector db-url)))
  ; pgc : connection
  (define pgc
    (virtual-connection
     (connection-pool
      (λ () (make-pg-connection)))))
  ; creates table if not present
  (create-table! pgc 'loosie)
  pgc)

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8080))

(define app-url (if (getenv "APP_URL")
                    (getenv "APP_URL")
                    "http://localhost:8080"))

(define (upload-file db a-loosie)
  (insert-one! db a-loosie))

(define (get-mime-type filename)
  (match filename
    [(pregexp #px".*\\.html?$") 'text/html]
    [(pregexp #px".*\\.md$") 'text/markdown]
    [(pregexp #px".*\\.txt$") 'text/plain]
    [(pregexp #px".*\\.pdf$") 'application/pdf]
    [_ 'unknown]))

; creates url-safe access code
(define (make-access-code)
  (let* ([b64-code (base64-encode (crypto-random-bytes 9) #"")]
         [no-plus (regexp-replace #rx"\\+" b64-code "-")]
         [no-slash (regexp-replace #rx"/" no-plus "_")]
         [no-equal (regexp-replace #rx"=" no-slash "*")])
    (bytes->string/utf-8 no-equal)))

(define (get-password-hash pw)
  (pwhash '(pbkdf2 hmac sha256)
          (if (bytes? pw) pw (string->bytes/utf-8 pw))
          '((iterations 20000))))

(define (verify-password pw pwh)
  (pwhash-verify #f 
                 (if (bytes? pw) pw (string->bytes/utf-8 pw))
                 pwh))

(define (valid-password? pw)
  (regexp-match? #px"^[ 0-9A-Za-z!@#$%^&*()]{8,32}$" pw))
