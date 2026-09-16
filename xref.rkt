#lang racket/base

;; Cross-reference info that links to published copies of user-scope docs.
;;
;; When `scribble` renders a themed doc, links into other documents are
;; resolved from the cross-reference info of the locally installed copies.
;; With `--redirect`, every such link is sent through a search URL (normally
;; docs.racket-lang.org/local-redirect/), which cannot know about copies
;; published elsewhere. This module instead loads the cross-reference info of
;; every user-scope document itself and rewrites each destination:
;;
;;  - Destinations in the docs you publish yourself get the absolute URL of
;;    the published page. The HTML renderer uses that URL as the link target.
;;
;;  - Destinations in every other user-scope doc are given a path as if the
;;    doc were installed in the main doc directory. Scribble's
;;    `--redirect-main <url>` flag then links to <url>/<doc>/<page>, exactly as
;;    it does for the main docs, and indirect links (which always go through a
;;    search URL) keep working too.
;;
;; So render with `--redirect-main https://docs.racket-lang.org/` and without
;; `--redirect`, which would override the published URLs.

(require racket/class
         racket/contract
         racket/fasl
         racket/path
         racket/string
         net/uri-codec
         scribble/base-render
         scribble/core
         scribble/xref
         (prefix-in html: scribble/html-render)
         (only-in scribble/private/literal-anchor literal-anchor? literal-anchor-string)
         setup/collects
         setup/dirs
         setup/main-doc
         setup/xref)

(provide (contract-out
          [theme/load-xref (-> (hash/c string? string?) xref?)]))

;;================================================
;; Destination URLs

;; A destination, as stored in cross-reference info, is a vector:
;;   0 title, 1 anchor, 2 section number, 3 output file (in "relative" form),
;;   4 whole-page?, and optionally 5 a redirect URL.
;; The renderer uses element 5, when present, as the complete link target.

(define (dest? v)
  (and (vector? v) (= (vector-length v) 5)))

;; Same encoding as `anchor-name` in scribble/html-render.
(define (anchor-name v)
  (define (encode-byte b)
    (string-append (if (< b 16) "~0" "~") (number->string b 16)))
  (define (encode-bytes str)
    (string->bytes/utf-8 (encode-byte (bytes-ref str 0))))
  (if (literal-anchor? v)
      (literal-anchor-string v)
      (let* ([v (string->bytes/utf-8 (format "~a" v))]
             [v (regexp-replace* #rx#"[A-Z.]" v #".&")]
             [v (regexp-replace* #rx#" " v #"._")]
             [v (regexp-replace* #rx#"\"" v #".'")]
             [v (regexp-replace* #rx#"[^-a-zA-Z0-9_!+*'()/.,]" v encode-bytes)])
        (bytes->string/utf-8 v))))

(define (ensure-trailing-slash s)
  (if (string-suffix? s "/") s (string-append s "/")))

;; Renderer that rewrites deserialized destinations. `doc-url` maps the id of
;; a document (the name of its directory) to the URL of its published copy, or
;; to #f for a document that `--redirect-main` should handle.
(define (redirecting-render% doc-url)
  (class (html:render-mixin render%)
    (super-new)
    (inherit root-relative? root-relative->path)

    ;; Same as the private `relative->path` in scribble/html-render.
    (define (relative->path p)
      (if (root-relative? p)
          (root-relative->path p)
          (let ([p (if (or (not (pair? p)) (eq? (car p) 'doc))
                       (main-doc-relative->path p)
                       p)])
            (if (path? p) p (collects-relative->path p)))))

    ;; Path of the destination file relative to the document directory `root`;
    ;; #f if it cannot be determined.
    (define (dest-file dest root)
      (define p (relative->path (vector-ref dest 3)))
      (and (path? p)
           (let ([rel (if (and root (complete-path? p))
                          (find-relative-path root p)
                          (file-name-from-path p))])
             (and (andmap path? (explode-path rel))
                  rel))))

    (define (path->url-string p)
      (string-join (map path-element->string (explode-path p)) "/"))

    (define (anchor-suffix dest)
      (if (vector-ref dest 4)
          ""
          (string-append "#" (uri-unreserved-encode (anchor-name (vector-ref dest 1))))))

    ;; Destination with the complete URL of the published page as link target
    (define (published-dest dest file base-url)
      (vector (vector-ref dest 0) (vector-ref dest 1) (vector-ref dest 2)
              (vector-ref dest 3) (vector-ref dest 4)
              (string-append base-url (path->url-string file) (anchor-suffix dest))))

    ;; Destination placed under the main doc directory, for `--redirect-main`
    (define (main-doc-dest dest file doc-id)
      (vector (vector-ref dest 0) (vector-ref dest 1) (vector-ref dest 2)
              (path->main-doc-relative (build-path (find-doc-dir) doc-id file))
              (vector-ref dest 4)))

    (define (rewrite val root doc-id base-url)
      (define v (if (known-doc? val) (known-doc-v val) val))
      (define file (and (dest? v) (dest-file v root)))
      (cond
        [(not file) val]
        ;; Left without a document id on purpose: the renderer sends an
        ;; indirect link (`#:indirect`) through a search URL, where a document
        ;; id would ask for a page path that a redirected destination cannot
        ;; supply. Without the id it asks for the tag instead, which works.
        [base-url (published-dest v file base-url)]
        [else
         (define new-v (main-doc-dest v file doc-id))
         (if (known-doc? val)
             (known-doc new-v (known-doc-id val) (known-doc-pkg val))
             new-v)]))

    (define/override (deserialize-info v ci
                                       #:root [root #f]
                                       #:doc-id [doc-id #f]
                                       #:pkg [pkg #f])
      (cond
        [(not doc-id)
         (super deserialize-info v ci #:root root #:doc-id doc-id #:pkg pkg)]
        [else
         ;; Deserialize into a scratch table so that only this document's
         ;; entries are rewritten.
         (define scratch (struct-copy collect-info ci [ext-ht (make-hash)]))
         (super deserialize-info v scratch #:root root #:doc-id doc-id #:pkg pkg)
         (define ht (collect-info-ext-ht ci))
         (define base-url (doc-url doc-id))
         (for ([(k val) (in-hash (collect-info-ext-ht scratch))])
           (hash-set! ht k (rewrite val root doc-id base-url)))]))))


;;================================================
;; Loading

(define (doc-dir-id dir)
  (define-values (base name dir?) (split-path dir))
  (path->string name))

(define (sxref-files dir)
  (for/list ([f (in-list (directory-list dir #:build? #t))]
             #:when (regexp-match? #rx"^out[0-9]+[.]sxref$"
                                   (path->string (file-name-from-path f))))
    f))

;; Loads the cross-reference info of every rendered user-scope document. Each
;; destination in a document whose id is a key of `urls` gets a link target
;; under that URL; destinations in other user-scope documents are made to look
;; like main-installation docs, so that `--redirect-main` handles them.
(define (theme/load-xref urls)
  (define (doc-url id)
    (define u (hash-ref urls id #f))
    (and u (ensure-trailing-slash u)))
  (load-xref
   (for*/list ([dir (in-list (get-rendered-doc-directories #f #t))]
               [sxref (in-list (sxref-files dir))])
     (λ ()
       (make-data+root+doc-id (cadr (call-with-input-file* sxref fasl->s-exp))
                              dir
                              (doc-dir-id dir))))
   #:render% (redirecting-render% doc-url)))
