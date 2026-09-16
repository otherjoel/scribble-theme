#lang racket/base

;; Checks that css->html-defaults copies CSS files under content-hashed names,
;; rewrites @import references, and changes the names when the content changes.

(require racket/file
         racket/path
         racket/string
         rackunit
         scribble/html-properties
         scribble-theme)

(define dir (make-temporary-file "fingerprint-test-~a" 'directory))
(define main-css (build-path dir "style.css"))
(define import-css (build-path dir "fonts.css"))

(define (write-css! body)
  (display-to-file (string-append "@import url(\"fonts.css\");\n" body) main-css #:exists 'replace))

(write-css! "body { color: red; }")
(display-to-file "@font-face { font-family: X; }" import-css #:exists 'replace)

(define (basename p) (path->string (file-name-from-path p)))

(define h1 (css->html-defaults main-css))
(define main1 (html-defaults-style-path h1))
(define extras1 (html-defaults-extra-files h1))

(check-regexp-match #px"^style-[0-9a-f]{8}[.]css$" (basename main1))
(check-equal? (length extras1) 1)
(check-regexp-match #px"^fonts-[0-9a-f]{8}[.]css$" (basename (car extras1)))
(check-true (file-exists? main1))
(check-true (file-exists? (car extras1)))
(check-true (string-contains? (file->string main1)
                              (string-append "@import url(\"" (basename (car extras1)) "\")"))
            "@import rewritten to the hashed name")
(check-equal? (file->string (car extras1)) "@font-face { font-family: X; }")

;; Same content -> same names
(check-equal? (basename (html-defaults-style-path (css->html-defaults main-css)))
              (basename main1))

;; Changed content -> different main name, same import name
(write-css! "body { color: blue; }")
(define h2 (css->html-defaults main-css))
(check-not-equal? (basename (html-defaults-style-path h2)) (basename main1))
(check-equal? (basename (car (html-defaults-extra-files h2))) (basename (car extras1)))

;; Changed import -> different import name and different main name
(display-to-file "@font-face { font-family: Y; }" import-css #:exists 'replace)
(define h3 (css->html-defaults main-css))
(check-not-equal? (basename (car (html-defaults-extra-files h3))) (basename (car extras1)))
(check-not-equal? (basename (html-defaults-style-path h3)) (basename (html-defaults-style-path h2)))

;; Fingerprinting off -> original paths untouched
(define h4 (css->html-defaults main-css #:fingerprint? #f))
(check-equal? (html-defaults-style-path h4) (path->string main-css))
(check-equal? (html-defaults-extra-files h4) (list (path->string import-css)))

(delete-directory/files dir)
