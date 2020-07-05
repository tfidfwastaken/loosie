#lang racket/base

(require db deta
         database-url
         crypto/libcrypto)

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8080))

; A loosie is a text-based file we want to share
(define-schema loosie
  ([id id/f #:primary-key #:auto-increment]
   [mime-type string/f #:contract non-empty-string? #:wrapper string-uppercase]
   [content binary/f]
   [passphrase string/f]))

(define (init-db)
  ; make-pg-connection : (-> connection)
  (define make-pg-connection
    (let ([db-url (getenv "DATABASE_URL")])
      (database-url-connector db-url)))
  ; pgc : connection
  (define pgc
    (virtual-connection
     (connection-pool
      (make-pg-connection))))
  ; creates table if not present
  (create-table! pgc 'loosie))

(define (upload-file a-loosie db)
  "TODO")

(define (store-password pw)
  (pwhash '(pbkdf2 hmac sha256) pw
          '((iterations 20000) (key-size 128))))

(define (verify-password pw pwh)
  (pwhash-verify #f pw pwh))
