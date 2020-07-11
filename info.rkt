#lang info

(define collection "loosie")
(define version "0.0.1")
(define test-omit-paths '("loosie/app.rkt"))
(define scribblings '())
(define deps '("base"
               "crypto-lib"
               "database-url"
               "db-lib"
               "deta-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '("rackunit-lib"))
