#lang web-server/base

(require racket/list
	 web-server/http
	 web-server/stuffers
	 web-server/page
	 web-server/servlet
         web-server/servlet-env
         web-server/dispatch)

(provide interface-version)
(define interface-version 'stateless)

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8080))

(define (counter cnt)
  (page
   (response/xexpr
    `(html (head (title "loosie"))
           (body (h1 ,(number->string cnt))
                 (a ([href ,(embed/url
                             (λ (req) (counter (add1 cnt))))])
                    "Increment"))))))

(define (start req)
  (counter 0))

(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port)
