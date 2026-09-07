#lang racket/base

(require (for-syntax racket/base racket/path)
         racket/class
         racket/cmdline
         racket/contract
         racket/file
         racket/list
         racket/path
         scribble/base
         scribble/core
         scribble/html-properties)

(provide
 theme/provide-doc
 nav-link/c
 nav-item/c
 (contract-out
  [css-imports (-> absolute-path? (listof absolute-path?))]
  [css->html-defaults (-> absolute-path? html-defaults?)]
  [theme/nav-block (-> (listof nav-item/c) block?)]
  [scribble/manual-custom-css (->* (module-path? html-defaults?)
                                   (#:nav (or/c #f (listof nav-item/c)))
                                   part?)]))


;;================================================
;; CSS

;; Get a list of CSS files referenced via CSS @import
(define (css-imports css-file-path)
  (define import-regex
    #px"@import\\s+(?:url\\s*\\(\\s*)?['\"]?([^'\"\\)]+)['\"]?\\s*\\)?[^;]*;")
  (define css-dir (path-only css-file-path))
  (define content (file->string css-file-path))
  
  (for/list ([match (in-list (regexp-match* import-regex content #:match-select cadr))]
             #:when (file-exists? (build-path css-dir match)))
    (build-path css-dir match)))

;; Construct html-defaults struct value from a CSS file
;; The `extra-files` field consists of any additional CSS files discovered via @import
(define (css->html-defaults abs-css-path)
  (html-defaults '(collects #"scribble" #"scribble-prefix.html")
                 (path->string abs-css-path)
                 (map path->string (css-imports abs-css-path))))


;;================================================
;; Site navigation bar

;; A nav link is (label . url). A nav menu is (label . (listof nav-link)).
(define nav-link/c (cons/c string? string?))
(define nav-item/c (or/c nav-link/c (cons/c string? (listof nav-link/c))))

(define (nav-link->element lnk)
  (hyperlink (cdr lnk) (car lnk)))

(define (nav-item->block item)
  (cond
    [(string? (cdr item))
     (paragraph (style "theme-nav-item" (list (alt-tag "span")))
                (nav-link->element item))]
    [else
     (nested-flow (style "theme-nav-menu" (list (alt-tag "details")
                                                (attributes '((class . "theme-nav-item")))))
                  (list (paragraph (style #f (list (alt-tag "summary"))) (car item))
                        (itemization (style #f '())
                                     (for/list ([lnk (in-list (cdr item))])
                                       (list (paragraph (style #f (list (alt-tag "span")))
                                                        (nav-link->element lnk)))))))]))

(define (nav-flow items)
  (nested-flow (style "theme-nav" (list (alt-tag "nav")))
               (map nav-item->block items)))

;; Rendered in parts that do not start a new HTML page, so that the nav
;; appears exactly once per page.
(define nav-placeholder
  (paragraph (style #f (list (alt-tag "span") (attributes '((hidden . ""))))) '()))

;; Returns a block that renders as the nav bar only when placed in a part that
;; begins a whole HTML page (the top part in --html mode; every split page in
;; --htmls mode). Each part needs its own copy of this block, because the
;; renderer caches the resolved value per block object.
(define (theme/nav-block items)
  (delayed-block
   (lambda (renderer part ri)
     (if (and (method-in-interface? 'part-whole-page? (object-interface renderer))
              (send renderer part-whole-page? part ri))
         (nav-flow items)
         nav-placeholder))))


;;================================================
;; Scribble doc customizations

(define (manual-racket-css-addition? v)
  (equal? v (css-style-addition '(collects #"scribble" #"manual-racket.css"))))

;; For scribble/manual docs: remove the css-style-addition and replace html-defaults
;; Recursively updates all parts in the document tree; when nav-items is given,
;; also prepends a site nav block to every part.
;; Filename-string html-defaults [#:nav (listof nav-item)] -> Part
(define (scribble/manual-custom-css scrbl-file new-html-defaults #:nav [nav-items #f])
  (define (update-prop v)
    (cond [(html-defaults? v) new-html-defaults] ; replace html-defaults
          [(manual-racket-css-addition? v) #f]   ; omit css-addition added by scribble/manual
          [else v]))

  ;; Recursively update a part and all its sub-parts
  (define (update-part p)
    (define new-style
      (style #f (filter-map update-prop (style-properties (part-style p)))))
    (define new-blocks
      (if nav-items
          (cons (theme/nav-block nav-items) (part-blocks p))
          (part-blocks p)))
    (define new-parts (map update-part (part-parts p)))
    (struct-copy part p
                 [style new-style]
                 [blocks new-blocks]
                 [parts new-parts]))

  (define doc (dynamic-require scrbl-file 'doc))
  (update-part doc))

;; The main macro
(define-syntax (theme/provide-doc stx)
  (define (build scrbl-filename css-path nav-expr)
    (define css-path-datum (syntax->datum css-path))
    (with-syntax ([scrbl-filename scrbl-filename]
                  [nav-expr nav-expr]
                  [abs-css-path
                   (if (absolute-path? css-path-datum)
                       (build-path css-path-datum)
                       (datum->syntax css-path
                                      (simplify-path
                                       (build-path (path-only (syntax-source stx))
                                                   css-path-datum))))]
                  [DOC (datum->syntax stx 'doc)])
      #'(begin
          (define DOC
            (scribble/manual-custom-css
             scrbl-filename
             (css->html-defaults abs-css-path)
             #:nav nav-expr))
          (provide DOC))))
  (syntax-case stx ()
    [(_ scrbl-filename css-path)
     (build #'scrbl-filename #'css-path #'#f)]
    [(_ scrbl-filename css-path #:nav nav-expr)
     (build #'scrbl-filename #'css-path #'nav-expr)]))


;;================================================
;; Utility

(define (copy-base-css-files outfile)
  (define manual-style-css (collection-file-path "manual-style.css" "scribble"))
  (define manual-racket-css (collection-file-path "manual-racket.css" "scribble"))
  (with-output-to-file
      outfile
    (λ ()
      (displayln (file->string manual-style-css))
      (displayln (file->string manual-racket-css))))
  (list manual-style-css manual-racket-css))

(module+ main
  (define output-file
    (command-line
     #:usage-help "Concatenates manual-style.css and manual-racket.css from the main scribble collection into the file <outfile>."
     #:args (outfile)
     outfile))
  (define css-paths (copy-base-css-files output-file))
  (displayln (format "Concatenated into ~a:" output-file))
  (for ([fp (in-list css-paths)])
    (displayln fp)))
