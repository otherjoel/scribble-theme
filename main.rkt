#lang racket/base

(require (for-syntax racket/base racket/path)
         file/sha1
         racket/class
         racket/cmdline
         racket/contract
         racket/file
         racket/list
         racket/path
         racket/string
         scribble/base
         scribble/core
         scribble/html-properties)

(provide
 theme/provide-doc
 nav-link/c
 nav-item/c
 (contract-out
  [css-imports (-> absolute-path? (listof absolute-path?))]
  [css->html-defaults (->* (absolute-path?) (#:fingerprint? any/c) html-defaults?)]
  [theme/nav-block (-> (listof nav-item/c) block?)]
  [scribble/manual-custom-css (->* (module-path? html-defaults?)
                                   (#:nav (or/c #f (listof nav-item/c)))
                                   part?)]))


;;================================================
;; CSS

;; Get a list of CSS files referenced via CSS @import
(define import-regex
  #px"@import\\s+(?:url\\s*\\(\\s*)?['\"]?([^'\"\\)]+)['\"]?\\s*\\)?[^;]*;")

(define (css-imports css-file-path)
  (define css-dir (path-only css-file-path))
  (define content (file->string css-file-path))
  
  (for/list ([match (in-list (regexp-match* import-regex content #:match-select cadr))]
             #:when (file-exists? (build-path css-dir match)))
    (build-path css-dir match)))

;;------------------------------------------------
;; Fingerprinting: copy CSS files under content-hashed names so that browsers
;; and CDNs fetch a fresh copy whenever the contents change.

;; Directory for the fingerprinted copies. Names are content-derived, so
;; repeated builds overwrite identical files rather than accumulating.
(define (fingerprint-dir)
  (define dir (build-path (find-system-path 'temp-dir) "scribble-theme"))
  (make-directory* dir)
  dir)

;; First 8 hex digits of the SHA-1 of a string
(define (fingerprint str)
  (substring (sha1 (open-input-string str)) 0 8))

;; "manual-style.css" + "3f2a9c1b" -> "manual-style-3f2a9c1b.css"
(define (fingerprinted-name name fp)
  (define name-str (if (path? name) (path->string name) name))
  (if (regexp-match? #rx"[.]css$" name-str)
      (regexp-replace #rx"[.]css$" name-str (string-append "-" fp ".css"))
      (string-append name-str "-" fp)))

;; Write `content` to the fingerprint directory under a hashed version of
;; `orig-name`; returns the new path.
(define (write-fingerprinted orig-name content)
  (define dest (build-path (fingerprint-dir)
                           (fingerprinted-name orig-name (fingerprint content))))
  (unless (file-exists? dest)
    (display-to-file content dest #:exists 'replace))
  dest)

;; Copy the main CSS file and its @imports under fingerprinted names, rewriting
;; the @import references in the main file to point at the hashed copies.
;; Returns (values main-copy-path (listof import-copy-path)).
(define (fingerprint-css abs-css-path)
  (define css-dir (path-only abs-css-path))
  (define imports (css-imports abs-css-path))
  ;; original relative name -> fingerprinted copy path
  (define copies
    (for/hash ([imp (in-list imports)])
      (values (path->string (find-relative-path css-dir imp))
              (write-fingerprinted (file-name-from-path imp) (file->string imp)))))
  (define rewritten
    (regexp-replace* import-regex (file->string abs-css-path)
                     (λ (whole ref)
                       (define copy (hash-ref copies ref #f))
                       (if copy
                           (string-replace whole ref
                                           (path->string (file-name-from-path copy))
                                           #:all? #f)
                           whole))))
  (values (write-fingerprinted (file-name-from-path abs-css-path) rewritten)
          (hash-values copies)))

;; Construct html-defaults struct value from a CSS file
;; The `extra-files` field consists of any additional CSS files discovered via @import
;; When fingerprint? is true, the files are copied under content-hashed names.
(define (css->html-defaults abs-css-path #:fingerprint? [fingerprint? #t])
  (define-values (main-path extra-paths)
    (if fingerprint?
        (fingerprint-css abs-css-path)
        (values abs-css-path (css-imports abs-css-path))))
  (html-defaults '(collects #"scribble" #"scribble-prefix.html")
                 (path->string main-path)
                 (map path->string extra-paths)))


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
  (define (build scrbl-filename css-path nav-expr fingerprint-expr)
    (define css-path-datum (syntax->datum css-path))
    (with-syntax ([scrbl-filename scrbl-filename]
                  [nav-expr nav-expr]
                  [fingerprint-expr fingerprint-expr]
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
             (css->html-defaults abs-css-path #:fingerprint? fingerprint-expr)
             #:nav nav-expr))
          (provide DOC))))
  ;; Optional keyword arguments, in any order
  (define (parse-keywords kw-stxs)
    (let loop ([kws (syntax->list kw-stxs)] [nav #'#f] [fp #'#t])
      (syntax-case kws ()
        [() (values nav fp)]
        [(#:nav expr . rest) (loop #'rest #'expr fp)]
        [(#:fingerprint? expr . rest) (loop #'rest nav #'expr)]
        [(other . _)
         (raise-syntax-error #f "expected #:nav or #:fingerprint?" stx #'other)])))
  (syntax-case stx ()
    [(_ scrbl-filename css-path . kws)
     (let-values ([(nav-expr fp-expr) (parse-keywords #'kws)])
       (build #'scrbl-filename #'css-path nav-expr fp-expr))]))


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
