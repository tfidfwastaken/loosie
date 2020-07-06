#lang racket/base

(require db database-url
         deta
         crypto
         crypto/libcrypto
         racket/string)

(provide init-db port upload-file get-password-hash verify-password)

(crypto-factories (list libcrypto-factory))

(define (init-db)
  ; A loosie is a text-based file we want to share
  (define-schema loosie
    ([id id/f #:primary-key #:auto-increment]
     [name string/f #:contract non-empty-string?]
     [mime-type string/f #:contract non-empty-string? #:wrapper string-upcase]
     [content binary/f]
     [passphrase string/f]))

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

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8080))

(define (upload-file a-loosie db)
  (insert-one! db a-loosie))

(define (get-password-hash pw)
  (pwhash '(pbkdf2 hmac sha256)
          (if (bytes? pw) pw (string->bytes/utf-8 pw))
          '((iterations 20000))))

(define (verify-password pw pwh)
  (pwhash-verify #f 
                 (if (bytes? pw) pw (string->bytes/utf-8 pw))
                 pwh))

(define (valid-password? pw)
  (regexp-match? #rx"^[0-9A-Za-z!@#$%^&*()]{8,32}$" pw))
