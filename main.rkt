#lang racket/base

(require (for-syntax racket/base racket/path)
         racket/cmdline
         racket/contract
         racket/file
         racket/list
         racket/path
         scribble/core
         scribble/html-properties)

(provide
 theme/provide-doc
 (contract-out
  [css-imports (-> absolute-path? (listof absolute-path?))]
  [css->html-defaults (-> absolute-path? html-defaults?)]
  [scribble/manual-custom-css (-> module-path? html-defaults? part?)]))


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
;; Scribble doc customizations

(define (manual-racket-css-addition? v)
  (equal? v (css-style-addition '(collects #"scribble" #"manual-racket.css"))))

;; For scribble/manual docs: remove the css-style-addition and replace html-defaults
;; Recursively updates all parts in the document tree
;; Filename-string Symbol -> Part
(define (scribble/manual-custom-css scrbl-file new-html-defaults)
  (define (update-prop v)
    (cond [(html-defaults? v) new-html-defaults] ; replace html-defaults
          [(manual-racket-css-addition? v) #f]   ; omit css-addition added by scribble/manual
          [else v]))

  ;; Recursively update a part and all its sub-parts
  (define (update-part p)
    (define new-style
      (style #f (filter-map update-prop (style-properties (part-style p)))))
    (define new-parts (map update-part (part-parts p)))
    (struct-copy part p
                 [style new-style]
                 [parts new-parts]))

  (define doc (dynamic-require scrbl-file 'doc))
  (update-part doc))

;; The main macro
(define-syntax (theme/provide-doc stx)
  (syntax-case stx ()
    [(_ scrbl-filename css-path)
     (let ([css-path-datum (syntax->datum #'css-path)])
       (with-syntax ([abs-css-path
                      (if (absolute-path? css-path-datum)
                          (build-path css-path-datum)
                          (datum->syntax #'css-path
                                         (simplify-path 
                                          (build-path (path-only (syntax-source stx)) 
                                                      css-path-datum))))]
                     [DOC (datum->syntax stx 'doc)])
         #'(begin
             (define DOC
               (scribble/manual-custom-css
                scrbl-filename
                (css->html-defaults abs-css-path)))
             (provide DOC))))]))


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
