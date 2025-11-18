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
as the main HTML file. 

@inline-note{The above example also shows how to ensure cross references to other Racket docs link
out to the main Racket documentation website. See @secref["running" #:doc '(lib
"scribblings/scribble/scribble.scrbl")] and @secref["xref-flags" #:doc '(lib
"scribblings/scribble/scribble.scrbl")] for details.}

@;===============================================

@section{Reference}

@defform[(theme/provide-doc scrbl-filename css-path)]{

The main macro for creating a themed version of a Scribble document.

Dynamically requires the @racket[doc] value from @racket[scrbl-filename], replaces its default CSS
with the CSS specified by @racket[css-path], and provides the updated @racket[doc].

The @racket[scrbl-filename] should be a module path (typically a string naming a @filepath{.scrbl}
file).

The @racket[css-path] can be either an absolute path or a relative path. If relative, it is resolved
relative to the location of the file containing the @racket[theme/provide-doc] call.

Any additional CSS files referenced via @tt{@"@"import} directives in the main CSS file (one level
deep) are automatically discovered and included in the output.

This macro expands to a call to @racket[scribble/manual-custom-css] wrapped in a @racket[provide]
that exports the @racket[doc] binding.

}


@;------------------------------------------------

@subsection{Under the hood}

You probably won’t need these functions unless you want to dynamically construct Scribble
@racket[part]s that use your @tech{themes}.

@defproc[(scribble/manual-custom-css [scrbl-file module-path?]
                                     [new-html-defaults html-defaults?]) part?]{

Dynamically requires the @racket[doc] value from @racket[scrbl-file], replaces its HTML styling
properties with @racket[new-html-defaults], and returns the updated @racket[part].

This is the function used internally by @racket[theme/provide-doc].

}

@defproc[(css-imports [css-file-path absolute-path?]) (listof absolute-path?)]{

Returns a list of absolute paths to CSS files referenced via @tt{@"@"import} directives in
@racket[css-file-path].

Only @tt{@"@"import} directives that reference files existing in the same directory as
@racket[css-file-path] are included. The function does not recursively search for @tt{@"@"import}
directives in the imported files.

}

@defproc[(css->html-defaults [abs-css-path absolute-path?]) html-defaults?]{

Constructs an @racket[html-defaults] struct suitable for use with @racketmodname[scribble/manual]
documents.

The @racket[html-defaults] uses the default Scribble prefix file and sets @racket[abs-css-path] as
the main style file. The @racket[extra-files] field is populated with any additional CSS files
discovered via @racket[css-imports].

}


