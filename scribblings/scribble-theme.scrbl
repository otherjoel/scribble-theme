#lang scribble/manual

@(require (for-label scribble-theme
                     scribble/core
                     scribble/html-properties
                     racket/base)
          scribble/core
          scribble/decode
          scribble/html-properties
          racket/runtime-path)

@(define-runtime-path add-css "doc-aux.css")

@(define (terminal . args)
  (compound-paragraph (style "terminal" (list (css-style-addition add-css)
                                              (alt-tag "div")))
                      (list (apply verbatim args))))

@(define (:> . elems)
  (element (style "prompt" (list (css-style-addition add-css)))
           (apply exec elems)))

@(define (inline-note #:type [type 'note] . elems)
  (compound-paragraph
   (style "inline-note" (list (css-style-addition add-css)
                          (attributes `((class . ,(format "refcontent ~a" type))))
                          (alt-tag "aside")))
   (decode-flow elems)))

@title{Scribble themes}
@author{Joel Dueck}

@defmodule[scribble-theme]

Scribble documents published as HTML already look great (at least, the ones written in @code{#lang
scribble/manual}). If you want to thoroughly customize the look of the rendered HTML, though, it can
be tricky, so I made this package to make it a bit easier.

@inline-note{You can see an example of a themed version of these docs at
@url{https://joeldueck.com/what-about/scribble-theme/}.}

In my case, I wanted Scribble docs for my packages to render normally when rendered as part of a
local install or the main site @url{https://docs.racket-lang.org}, but I wanted to easily substitute
my own CSS when publishing to my website.

In order to attempt this, you have to be handy with CSS.


@;===============================================

@section{How it works}

A @deftech{theme} is a CSS file (and optionally additional CSS files it references via @tt{@"@"import}
directives). @deftech{Theming}, for our purposes, involves substituting your own CSS file(s) for
the ones supplied by @racketmodname[scribble/manual], and doing so in a way that does not affect
the output when rendered using "normal" methods like @exec{raco setup}.

By default, documents written in @code{#lang scribble/manual} that are rendered to HTML link out to
three files stored in the @tt{scribble} collection: @filepath{manual-style.css} (which in turn
brings in @filepath{manual-fonts.css} via a CSS @tt{@"@"import} declaration), and
@filepath{manual-racket.css}. This is all documented in @secref["manual-render-style" #:doc '(lib
"scribblings/scribble/scribble.scrbl")].

@margin-note{It's possible to overrride @filepath{manual-style.css} by adding a @racket[#:style]
argument to @racket[title] inside your Scribble doc; but this would affect the styling of your
document every time it is rendered, which you might not want. Also, 
@racketmodname[scribble/manual] links @filepath{manual-racket.css} as a
@racket[css-style-addition] after your code runs, so there’s no way to add code to your document
that can suppress that file from being included.}

This module makes it easy to create a separate “themed” version of your document that imports its
@racket[doc] value (provided by all Scribble modules), strips out the default CSS and adds in
your own. The normal/original document will still render normally, but you can also render the
themed version for customized output. When you supply your main CSS file, any additional CSS files
referenced via @tt{@"@"import} directives (one level deep) are automatically discovered and included.

@;===============================================

@section{Installation}

Install this package from the command line:

@terminal{
@:>{raco pkg install scribble-theme}
}

You can also install it from the @hyperlink["https://github.com/otherjoel/scribble-theme"]{GitHub repository}
if you prefer.

@;===============================================

@section{Setting Up Your Theme}

A @tech{theme} is simply a CSS file (and any additional CSS files it references via @tt{@"@"import}).
You can create and store your theme's CSS files anywhere you like—typically in the same directory
as your Scribble source files, or in a project subdirectory.

To get started, you can use this package’s command-line utility to generate a CSS file containing
the default styles from @racketmodname[scribble/manual]:

@terminal{
@:>{racket -l- scribble-theme my-theme.css}
Concatenated into my-theme.css:
/Applications/Racket v8.18/share/pkgs/scribble-lib/scribble/manual-style.css
/Applications/Racket v8.18/share/pkgs/scribble-lib/scribble/manual-racket.css
}

This gives you a starting point that you can customize.


@;===============================================

@section{Rendering HTML}

In the same folder as your Scribble sources, create a new file:

@filebox["my-themed-scribblings.scrbl"]{
@codeblock|{
#lang racket/base

(require scribble-theme)

(theme/provide-doc "my-package.scrbl" "my-theme.css")
}|}

This new file acts like a custom overlay over your original Scribble doc. The first argument is
the path to your original Scribble source file, and the second is the path to your main CSS file.
If the CSS path is relative, it's resolved relative to this themed file's location.

@inline-note{Any files referenced in your main CSS file via @tt{@"@"import} will
automatically be discovered and copied with the other output files. (But these files will not in
turn be recursively searched for further @tt{@"@"import}s.)}

You can render this file with @exec{scribble} like so:

@terminal{
@:>{
scribble --html +m \@(linebreak)
@hspace[9] --redirect https://docs.racket-lang.org/local-redirect/ \@(linebreak)
@hspace[9] --dest docs/ \@(linebreak)
@hspace[9] --dest-name index.html \@(linebreak)
@hspace[9] my-themed-scribblings.scrbl}
}

This will place the output in the @filepath{docs/} subfolder with @filepath{index.html}
as the main HTML file. Your CSS files are copied into the output under content-hashed names, such as
@filepath{my-theme-3f2a9c1b.css}, so browsers and CDNs fetch a fresh copy whenever the CSS changes
(see @racket[css->html-defaults]).

@inline-note{The above example also shows how to ensure cross references to other Racket docs link
out to the main Racket documentation website. See @secref["running" #:doc '(lib
"scribblings/scribble/scribble.scrbl")] and @secref["xref-flags" #:doc '(lib
"scribblings/scribble/scribble.scrbl")] for details.}

@;===============================================

@section[#:tag "nav"]{Adding a site navigation bar}

Themed docs published on your own website usually need a way back to the rest of that site. Pass a
list of navigation items to @racket[theme/provide-doc] with the @racket[#:nav] keyword, and a bar of
links is inserted at the top of every HTML page:

@filebox["my-themed-scribblings.scrbl"]{
@codeblock|{
#lang racket/base

(require scribble-theme)

(define site-nav
  (list (cons "My Site" "https://example.com/")
        (cons "Projects" "https://example.com/projects.html")
        (cons "Other docs"
              (list (cons "Guide" "https://example.com/guide/")
                    (cons "Reference" "https://example.com/reference/")))))

(theme/provide-doc "my-package.scrbl" "my-theme.css" #:nav site-nav)
}|}

Each item is either a @deftech{nav link} (a label paired with a URL) or a @deftech{nav menu} (a label
paired with a list of nav links). A menu renders as a @tt{<details>} element, so it opens and closes
without JavaScript.

The bar appears exactly once on every page: on the single page produced by @exec{scribble --html},
and on every page produced by @exec{scribble --htmls}. Internally, a block is added to the front of
every @racket[part] in the document; at render time this block produces the bar only in parts that
begin a new HTML page, and an empty hidden @tt{<span>} everywhere else.

This package supplies no CSS for the bar. It generates the markup below, and you style it in your
@tech{theme}:

@verbatim|{
<nav class="theme-nav">
  <span class="theme-nav-item"><a href="https://example.com/">My Site</a></span>
  <span class="theme-nav-item"><a href="https://example.com/projects.html">Projects</a></span>
  <details class="theme-nav-menu theme-nav-item">
    <summary>Other docs</summary>
    <ul>
      <li><span><a href="https://example.com/guide/">Guide</a></span></li>
      <li><span><a href="https://example.com/reference/">Reference</a></span></li>
    </ul>
  </details>
</nav>
}|

The bar is placed inside the main column, just after the page heading, so a fixed position is the
simplest way to keep it at the top of the window. The CSS below is a starting point. It also moves
Scribble's own fixed elements (table of contents, page navigation, version box) down to make room:

@verbatim|{
:root { --site-nav-height: 2.4rem; }

.theme-nav {
    position: fixed; top: 0; left: 0; right: 0; z-index: 12000;
    height: var(--site-nav-height);
    display: flex; align-items: center;
    background: #fff; border-bottom: 1px solid #eaeaea;
}
.theme-nav-item:first-child { margin-right: auto; }
.theme-nav-menu { position: relative; }
.theme-nav-menu > summary { list-style: none; cursor: pointer; }
.theme-nav-menu ul {
    position: absolute; right: 0; top: 100%;
    list-style: none; background: #fff; border: 1px solid #eaeaea;
}

.tocset, .navsettop { top: var(--site-nav-height); }
.maincolumn { margin-top: calc(4rem + var(--site-nav-height)); }
.versionbox { top: calc(0.25rem + var(--site-nav-height)); }
@media print { .theme-nav { display: none; } }
}|

@;===============================================

@section{Reference}

@defform[(theme/provide-doc scrbl-filename css-path keyword-option ...)
         #:grammar ([keyword-option (code:line #:nav nav-items-expr)
                                    (code:line #:fingerprint? fingerprint-expr)])
         #:contracts ([nav-items-expr (or/c #f (listof nav-item/c))]
                      [fingerprint-expr boolean?])]{

The main macro for creating a themed version of a Scribble document.

Dynamically requires the @racket[doc] value from @racket[scrbl-filename], replaces its default CSS
with the CSS specified by @racket[css-path], and provides the updated @racket[doc].

The @racket[scrbl-filename] should be a module path (typically a string naming a @filepath{.scrbl}
file).

The @racket[css-path] can be either an absolute path or a relative path. If relative, it is resolved
relative to the location of the file containing the @racket[theme/provide-doc] call.

Any additional CSS files referenced via @tt{@"@"import} directives in the main CSS file (one level
deep) are automatically discovered and included in the output.

If @racket[#:nav] is given, a site navigation bar built from the @tech{nav links} and @tech{nav
menus} in @racket[nav-items-expr] is added to the top of every HTML page (see
@secref["nav"]).

The CSS files are copied into the output under content-hashed names unless @racket[#:fingerprint?]
is @racket[#f] (see @racket[css->html-defaults]).

This macro expands to a call to @racket[scribble/manual-custom-css] wrapped in a @racket[provide]
that exports the @racket[doc] binding.

@history[#:changed "2.1" @elem{Added the @racket[#:nav] and @racket[#:fingerprint?] arguments.}]

}


@;------------------------------------------------

@subsection{Under the hood}

You probably won’t need these functions unless you want to dynamically construct Scribble
@racket[part]s that use your @tech{themes}.

@defproc[(scribble/manual-custom-css [scrbl-file module-path?]
                                     [new-html-defaults html-defaults?]
                                     [#:nav nav-items (or/c #f (listof nav-item/c)) #f]) part?]{

Dynamically requires the @racket[doc] value from @racket[scrbl-file], replaces its HTML styling
properties with @racket[new-html-defaults], and returns the updated @racket[part]. If
@racket[nav-items] is a list, a block produced by @racket[theme/nav-block] is added to the front of
every part in the document.

This is the function used internally by @racket[theme/provide-doc].

@history[#:changed "2.1" @elem{Added the @racket[#:nav] argument.}]

}

@deftogether[(@defthing[nav-link/c contract? #:value (cons/c string? string?)]
              @defthing[nav-item/c contract?
                        #:value (or/c nav-link/c (cons/c string? (listof nav-link/c)))])]{

Contracts for @tech{nav links} and @tech{nav menus}: a link is a label paired with a URL, and a menu
is a label paired with a list of links.

@history[#:added "2.1"]

}

@defproc[(theme/nav-block [items (listof nav-item/c)]) block?]{

Returns a @racket[delayed-block] that renders as a @tt{<nav class="theme-nav">} element when it
appears in a part that begins a whole HTML page, and as an empty hidden @tt{<span>} otherwise. Each
part that should carry the bar needs its own block from this function, because the renderer caches
the resolved block per object.

@history[#:added "2.1"]

}

@defproc[(css-imports [css-file-path absolute-path?]) (listof absolute-path?)]{

Returns a list of absolute paths to CSS files referenced via @tt{@"@"import} directives in
@racket[css-file-path].

Only @tt{@"@"import} directives that reference files existing in the same directory as
@racket[css-file-path] are included. The function does not recursively search for @tt{@"@"import}
directives in the imported files.

}

@defproc[(css->html-defaults [abs-css-path absolute-path?]
                             [#:fingerprint? fingerprint? boolean? #t]) html-defaults?]{

Constructs an @racket[html-defaults] struct suitable for use with @racketmodname[scribble/manual]
documents.

The @racket[html-defaults] uses the default Scribble prefix file and sets @racket[abs-css-path] as
the main style file. The @racket[extra-files] field is populated with any additional CSS files
discovered via @racket[css-imports].

When @racket[fingerprint?] is true, the main CSS file and its imports are first copied to a
@filepath{scribble-theme} folder inside the system temporary directory, each under a name that
includes the first eight hex digits of the SHA-1 hash of its contents (for example,
@filepath{my-theme.css} becomes @filepath{my-theme-3f2a9c1b.css}). The @tt{@"@"import} references
in the copy of the main file are rewritten to the hashed names, and the copies are what Scribble
installs and links in the rendered HTML. A change to any of the files produces new names, so caches
never serve stale CSS; unchanged files keep the same names from build to build.

@history[#:changed "2.1" @elem{Added the @racket[#:fingerprint?] argument.}]

}


