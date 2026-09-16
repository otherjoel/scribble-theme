#lang racket/base

;; Renders a small scribble/manual document through the single-page and
;; multi-page HTML renderers and checks that the site nav bar appears exactly
;; once in every output page.

(require racket/file
         racket/list
         racket/path
         racket/string
         rackunit
         scribble/render
         scribble/html-properties
         (prefix-in html: scribble/html-render)
         scribble-theme
         "nav-doc.scrbl")

(define nav-items
  (list (cons "Home" "https://example.com/")
        (cons "Docs" (list (cons "Guide" "https://example.com/guide/")
                           (cons "Reference" "https://example.com/reference/")))))

(define css-file (make-temporary-file "nav-test-~a.css"))
(display-to-file "body { color: black; }" css-file #:exists 'replace)

(define themed-doc
  (scribble/manual-custom-css '(file "nav-doc.scrbl")
                              (css->html-defaults css-file)
                              #:nav nav-items))

(define (count-matches rx str)
  (length (regexp-match* rx str)))

(define (render-and-check mixin dest-name expected-pages)
  (define dir (make-temporary-file "nav-test-~a" 'directory))
  (render (list themed-doc) (list dest-name)
          #:render-mixin mixin
          #:dest-dir dir
          #:quiet? #t)
  (define pages
    (for/list ([f (in-list (find-files (λ (p) (equal? (path-get-extension p) #".html")) dir))])
      f))
  (check-equal? (length pages) expected-pages (format "~a page count" dest-name))
  (for ([page (in-list pages)])
    (define html (file->string page))
    (check-equal? (count-matches #rx"<nav class=\"theme-nav\">" html) 1
                  (format "one nav in ~a" page))
    (check-true (string-contains? html "<details class=\"theme-nav-menu theme-nav-item\">")
                (format "menu in ~a" page))
    (check-equal? (count-matches #rx"href=\"https://example.com/reference/\"" html) 1
                  (format "menu link in ~a" page)))
  (delete-directory/files dir))

;; Single page: the top part only
(render-and-check html:render-mixin "single" 1)

;; Multi page: index plus one page per top-level section
(render-and-check (λ (%) (html:render-multi-mixin (html:render-mixin %))) "multi" 3)

(delete-file css-file)
