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

A @deftech{theme} is a set of one or more CSS files. @deftech{Theming}, for our purposes, involves
substituting your own CSS file(s) for the ones supplied by @racketmodname[scribble/manual], and
doing so in a way that does not affect the output when rendered using “normal” methods like
@exec{raco setup}.

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
themed version for customized output.

@;===============================================

@section{Installation}

I recommend to install this package by forking/cloning its
@hyperlink["https://github.com/otherjoel/scribble-theme"]{GitHub repository}.

When you have a local copy of it, install it as a “linked” package:

@terminal{
@:>{raco pkg install --link scribble-theme/}
}

@inline-note{You can also install the package from the main package server. But in that case it will
be more difficult to follow the instructions below for adding custom CSS. You’ll need to limit
yourself to the approach in @secref["supplying-themes"].}

@;===============================================

@section{Setting Up Your Theme}

Assuming you’ve installed the package from a local/linked folder on your computer, you can simply
edit the base @tech{theme} supplied with this package.

Edit the @filepath{manual-my-style.css} and @filepath{manual-my-fonts.css} files in this repo's main
folder to customize your theme.

@section{Rendering HTML}

In the same folder as your Scribble sources, create a new file:

@filebox["my-themed-scribblings.scrbl"]{
@codeblock|{
#lang racket/base

(require scribble-theme)

(customize+provide-doc "my-package.scrbl" 'my-theme)
}|}

This new file acts like a custom overlay over your original Scribble doc.
You can render this file with @exec{scribble} like so:

@terminal{
@:>{
scribble --html +m \@(linebreak)
@hspace[9] --redirect https://docs.racket-lang.org/local-redirect/ \@(linebreak)
@hspace[9] --dest docs/ \@(linebreak)
@hspace[9] --dest-name index.html \@(linebreak)
@hspace[9] my-themed-scribblings.scrbl}
}

This will place the output in the @filepath{doc/} subfolder with @filepath{index.html}
as the main HTML file.

@;===============================================

@section{Adding Themes}

The default approach in this package is to set up one @tech{theme}, housed within this package. That
way you can reuse the theme across multiple projects and update it for all of them by editing one
set of files. 

But you can have more than one theme. You can do this either by adding more themes to your copy of
this package, or by supplying themes stored within your own package.

Whichever method you choose, you’ll first need to create additional CSS files for your theme. You
can get started quickly by copying the default styles for @racketmodname[scribble/manual] into a
single new CSS file by running this command from this package’s main folder:

@terminal{
@:>{racket -t main.rkt new-theme.css}
Concatenated into new-theme.css:
/Applications/Racket v8.18/share/pkgs/scribble-lib/scribble/manual-style.css
/Applications/Racket v8.18/share/pkgs/scribble-lib/scribble/manual-racket.css
}


@;------------------------------------------------

@subsection{Adding themes to this package}

Edit the value for the @racket[themes] hash table at the top of @filepath{main.rkt} in your local
copy of this package.

Add keys to that hash table. Each key must refer to an @racket[html-defaults] struct. If you keep
your CSS files for the new theme in the same folder as this package (probably most convenient),
refer to them as @racket['(collects #"scribble-theme" #"<filename>")] within the struct fields. Then
you can supply the new key in your call to @racket[customize+provide-doc].



@;------------------------------------------------

@subsection[#:tag "supplying-themes"]{Supplying themes stored in another collection}

In your themed @filepath{.scrbl} file’s call to @racket[customize+provide-doc], you can supply your
own @racket[html-defaults] struct which refers directly to your own CSS files:

@filebox["my-themed-scribblings.scrbl"]{
@codeblock|{
#lang racket/base

(require scribble-theme)

(customize+provide-doc "my-package.scrbl"
                       (html-defaults '(collects #"scribble" #"scribble-prefix.html")
                                      '(collects #"my-collection" #"my-theme.css")
                                      '((collects #"my-collection" #"my-fonts.css"))))
}|}

The first element in the struct is the @tech[#:doc '(lib
"scribblings/scribble/scribble.scrbl")]{prefix file} — here we're just using the Scribble default.
The second element is our @tech[#:doc '(lib "scribblings/scribble/scribble.scrbl")]{style file}. The
third is a list of any additional files that need to be copied/included with the published HTML
files. (If you don’t need a separate CSS file for your fonts, you could just supply @racket['()]
here.)

@;===============================================

@section{Reference}

In addition to the bindings below, this module also re-exports @racket[html-defaults] from
@racketmodname[scribble/html-properties].

@defform[(customize+provide-doc scrbl-file theme-key)
         #:grammar ([scrbl-filename module-path?]
                    [theme-key (or/c symbol? html-defaults?)])]{

Dynamically requires the @racket[doc] value from @racket[scrbl-file], overwrites its theme settings
with the @racket[html-defaults] value linked to @racket[theme-key], and reprovides the updated
@racket[doc]. This is a convenience wrapper around @racket[scribble/manual-custom-css].

}

@defproc[(scribble/manual-custom-css [scrbl-file module-path?] 
                                     [theme-key (or/c symbol? html-defaults?)]) part?]{

Dynamically requires the @racket[doc] value from @racket[scrbl-file], overwrites its theme settings
with the @racket[html-defaults] value linked to @racket[theme-key], and returns the updated
@racket[part].

}


