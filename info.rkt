#lang info
(define collection "scribble-theme")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/scribble-theme.scrbl" () ("Scribble Libraries"))))
(define pkg-desc "Replace scribble/manual CSS styles with custom ones")
(define version "2.0")
(define pkg-authors '("Joel Dueck"))
(define license '(Apache-2.0 OR MIT))
