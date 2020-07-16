#lang racket/base

(require crypto crypto/libcrypto
         net/base64
         markdown/parse markdown/display-xexpr
         racket/match)

(module+ test
  (require rackunit))

(provide get-mime-type
         markdown/bytes->html/bytes
         make-access-code
         encrypt-data decrypt-data
         get-password-hash
         password-matches?
         valid-password?)

(crypto-factories (list libcrypto-factory))

(define (get-mime-type filename)
  (match filename
    [(pregexp #px".*\\.html?$") #"text/html"]
    [(pregexp #px".*\\.md$") #"text/markdown"]
    [(pregexp #px".*\\.txt$") #"text/plain"]
    [(pregexp #px".*\\.pdf$") #"application/pdf"]
    [_ #"unknown"]))

(define (markdown/bytes->html/bytes md)
  (define md-string (bytes->string/utf-8 md))
  (define xexprs (parse-markdown md-string))
  (define html-xexpr
    `(html
      (head (title "LOOSIE – Markdown Preview")
            (link ([href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.1/styles/default.min.css"]
                   [rel "stylesheet"] [type "text/css"]))
            (link ([rel "stylesheet"] [href "/normalize.css"] [type "text/css"]))
            (link ([rel "stylesheet"] [href "/skeleton.css"] [type "text/css"]))
            (link ([rel "stylesheet"] [href "/user.css"] [type "text/css"])))
      (body (div ([class "container"] [style "margin-top: 3vh"])
                 ,@xexprs)
            (script
             ([src
               "//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.1/highlight.min.js"]))
            (script "hljs.initHighlightingOnLoad();"))))
  (string->bytes/utf-8 (xexpr->string html-xexpr)))
                  

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

