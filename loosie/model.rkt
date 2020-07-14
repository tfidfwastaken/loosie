#lang racket/base

(require racket/match
         db database-url
         deta threading
         net/base64
         crypto
         crypto/libcrypto
         racket/string)

(module+ test
  (require rackunit))

(provide init-db port app-url
         (schema-out loosie)
         upload-file get-mime-type
         make-access-code get-file-data
         encrypt-data decrypt-data
         get-password-hash
         password-matches?
         valid-password?)

(crypto-factories (list libcrypto-factory))

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


(define (get-mime-type filename)
  (match filename
    [(pregexp #px".*\\.html?$") #"text/html"]
    [(pregexp #px".*\\.md$") #"text/markdown"]
    [(pregexp #px".*\\.txt$") #"text/plain"]
    [(pregexp #px".*\\.pdf$") #"application/pdf"]
    [_ #"unknown"]))

; creates url-safe access code
(define (make-access-code)
  (regexp-replaces (bytes->string/utf-8
                    (base64-encode (crypto-random-bytes 9) #""))
                   '([#rx"\\+" "-"] [#rx"/" "_"] [#rx"=" "*"])))

; data, password -> cipher-data and hash table of decryption info
(define (encrypt-data data pw)
    (let* ([pwb (string->bytes/utf-8 pw)]
           [dek (generate-cipher-key '(aes gcm) #:size 32)]
           [iv (generate-cipher-iv '(aes gcm))]
           [salt (crypto-random-bytes 32)]
           [kek (kdf '(pbkdf2 hmac sha256) pwb salt
                     '([iterations 100000] [key-size 32]))])
      (define cipher-data
        (encrypt '(aes gcm) dek iv data))
      (define encrypted-dek
        (encrypt '(aes gcm) kek iv dek))
      (values cipher-data
              (hash 'encrypted-dek encrypted-dek
                    'salt salt
                    'iv iv))))

; cipher data, decryption hash table, password -> output data or exception
(define (decrypt-data cipher-data wrap pw)
    (let* ([pwb (string->bytes/utf-8 pw)]
           [salt (hash-ref wrap 'salt)]
           [iv (hash-ref wrap 'iv)]
           [enc-dek (hash-ref wrap 'encrypted-dek)]
           [kek (kdf '(pbkdf2 hmac sha256) pwb salt
                     '([iterations 100000] [key-size 32]))]
           [dek (decrypt '(aes gcm) kek iv enc-dek)])
      (decrypt '(aes gcm) dek iv cipher-data)))

; functions for password storage
(define (get-password-hash pw)
  (pwhash '(pbkdf2 hmac sha256)
          (if (bytes? pw) pw (string->bytes/utf-8 pw))
          '((iterations 20000))))

(define (password-matches? pw pwh)
  (pwhash-verify #f 
                 (if (bytes? pw) pw (string->bytes/utf-8 pw))
                 pwh))

(define (valid-password? pw)
  (regexp-match? #px"^[ 0-9A-Za-z!@#$%^&*()]{6,32}$" pw))

(module+ test
  (test-case
      "checks if mime-types match correctly"
    (check-equal? #"text/html"
                  (get-mime-type "abcd.html"))
    (check-equal? #"text/html"
                  (get-mime-type "ble/h.html"))
    (check-equal? #"text/html"
                  (get-mime-type "bleh.htm"))
    (check-equal? #"text/markdown"
                  (get-mime-type "ghanshyam.md"))
    (check-equal? #"text/plain"
                  (get-mime-type "baburao.txt"))
    (check-equal? #"application/pdf"
                  (get-mime-type "aman_rashid.pdf"))
    (check-equal? #"unknown"
                  (get-mime-type "freedom is an illusion")))

  (test-case
      "checks if access codes are cleaned properly"
    (define-simple-check (check-not-regexp-match pxstr str)
      (not (regexp-match? (pregexp pxstr) str)))
    (check-not-regexp-match "[+=/]"
                            (for/fold ([combined ""])
                                      ([i (in-range 30)])
                              (string-append combined (make-access-code)))))

  (test-case
      "check password validity"
    (check-true (valid-password? "fast and bulbous"))
    (check-true (valid-password? "#I u5e (em@*$^)!"))
    (check-false (valid-password? "bulbous-also-tapered"))
    (check-false (valid-password? "short"))
    (check-false (valid-password? "this passphrase is too heccin long"))
    (check-false (valid-password? "nuh/uh"))
    (check-false (valid-password? "passphrasen't"))
    (check-false (valid-password? ":[")))

  (test-case
      "check password verification"
    (check-true
     (password-matches? "a password"
                        (get-password-hash "a password"))))

  (test-case
      "check encryption and decryption"
    (define-values (data pw) (values #"s3cr3t d4t4" "some password"))
    (check-equal?
     data
     (let-values ([(enc-data wrap) (encrypt-data data pw)])
       (decrypt-data enc-data wrap pw)))))
