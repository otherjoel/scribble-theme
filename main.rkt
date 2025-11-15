#lang racket/base

(require (for-syntax racket/base)
         racket/cmdline
         racket/contract
         racket/file
         racket/list
         scribble/core
         scribble/html-properties)

(provide scribble/manual-custom-css
         customize+provide-doc
         (struct-out html-defaults))

(define themes
  (hasheq 'my-theme
          (html-defaults '(collects #"scribble" #"scribble-prefix.html")
                         '(collects #"scribble-theme" #"manual-my-style.css")
                         '((collects #"scribble-theme" #"manual-my-fonts.css")))))

(define (manual-racket-css-addition? v)
  (equal? v (css-style-addition '(collects #"scribble" #"manual-racket.css"))))

;; For scribble/manual docs: remove the css-style-addition and replace html-defaults
;; Filename-string Symbol -> Part
(define/contract (scribble/manual-custom-css scrbl-file theme-key-or-data)
  (-> module-path? (or/c symbol? html-defaults?) part?)

  (define (key-missing-thunk)
    (raise-argument-error
     'scribble/manual-custom-css
     (format "one of: ~a" (hash-keys themes))
     theme-key-or-data))

  (define new-html-theme
    (if (html-defaults? theme-key-or-data)
        theme-key-or-data
        (hash-ref themes theme-key-or-data key-missing-thunk)))

  (define (update-prop v)
    (cond [(html-defaults? v) new-html-theme]  ; replace html-defaults
          [(manual-racket-css-addition? v) #f] ; omit css-addition added by scribble/manual
          [else v]))
  
  (define doc (dynamic-require scrbl-file 'doc))
  (define new-style
    (style #f (filter-map update-prop (style-properties (part-style doc)))))
  (struct-copy part doc [style new-style]))

;; Convenience: get the updated doc from scribble/manual-custom-css and provide it
(define-syntax (customize+provide-doc stx)
  (syntax-case stx ()
    [(_ filename theme-key-or-data)
     (with-syntax ([DOC (datum->syntax stx 'doc)])
       #'(begin
           (define DOC (scribble/manual-custom-css filename theme-key-or-data))
           (provide DOC)))]))

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