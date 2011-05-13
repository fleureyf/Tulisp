;;;
;;; Paul Y Gloess, http://www.enseirb-matmeca.fr/~gloess
;;; created 7 March 2011, last edition 30 March 2011
;;;
;;; Purpose: Compile all LISP files related to the Tulip graph interface,
;;;           and graph examples, either into ELF/ or MACH-O/
;;;           subdirectory, depending on the machine (SSH or MacBookPro)
;;;           (according to *FEATURES*).
;;;
;;; Note: This was adapted from previous "cmu-compile-all.lisp" file
;;;        implemented at the time we were using CMU Common LISP.
;;;
(in-package :common-lisp-user)

(require "SBCL" "sbcl")                    ; boot strap!

(sbcl-compile-file "sbcl")                 ; adapt REQUIRE and COMPILE-FILE to SBCL version.

(sbcl-compile-file "defcl")                ; simplified version of DEFCLASS and MAKE-INSTANCE.

(sbcl-compile-file "equalities")           ; EQUALS and SET-EQUALS.

(sbcl-compile-file "sets")                 ; some operations on relations or sets (represented
                                           ;  by lists).

(sbcl-compile-file "graphs")               ; basis of various kinds of graphs.

(sbcl-compile-file "cgraphs")              ; concrete graphs.

(sbcl-compile-file "time")                 ; time utilities.

(sbcl-compile-file "tulip")                ; Tulip format import/export.

;;;
;;; To compile everything, start a fresh LISP, then evaluate:
;;;    (load "sbcl-compile-all")
;;;
;;; Once everything is compiled, start a fresh LISP, and evaluate
;;;    (progn (load "sbcl") (sbcl-require "TULIP" "tulip"))
;;;  to get everything loaded. You can look at and evaluate commented
;;;  out forms in "graph-examples.lisp" file.
;;;