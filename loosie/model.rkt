#lang racket/base

(require db database-url
         deta threading
         racket/string)


(provide init-db port app-url
         upload-file get-file-data
         (schema-out loosie))

; A loosie is a text-based file we want to share
(define-schema loosie
  ([id id/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]
   [mime-type binary/f]
   [content binary/f]
   [access-code string/f #:contract non-empty-string? #:unique]
   [passphrase string/f]
   [pass-protected? boolean/f]
   [wrap binary/f]))

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

(define (get-file-data db access-code)
  (define query-vec
    (query-maybe-row db (~> (from loosie #:as l)
                            (where (= l.access-code ,access-code)))))
  (if (equal? query-vec #f) #f
      (make-loosie #:name            (vector-ref query-vec 1)
                   #:mime-type       (vector-ref query-vec 2)
                   #:content         (vector-ref query-vec 3)
                   #:access-code     access-code
                   #:passphrase      (vector-ref query-vec 5)
                   #:pass-protected? (vector-ref query-vec 6)
                   #:wrap            (vector-ref query-vec 7))))
