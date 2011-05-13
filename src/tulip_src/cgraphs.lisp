;;;
;;; TP Group I1G2, ENSEIRB-MATMECA
;;; created 23 March 2011, last edition 27 March 2011
;;;  http://www.enseirb-matmeca.fr/~gloess/enseignement/CL/2010_2011/
;;;
;;; Purpose: Simple implementation of concrete graphs:
;;;          - set of vertices represented by a list;
;;;          - set of edges represented by a list (simple or valued).
;;;
(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SBCL" "sbcl")         ; defines SBCL-REQUIRE !!!
  (sbcl-require "GRAPHS" "graphs"))
;;;
;;; Concrete graph representation
;;; -----------------------------
;;;  It is based on EDGEs and CGRAPH.
;;;
(defcl edge ()
  "An EDGE is composed of two vertices SOURCE and TARGET."
  source
  target)

(defmethod print-object ((edge edge) stream)
  "Prints the EDGE on STREAM, in the form {???, ???},
then returns the EDGE."
  (princ "{" stream)
  (princ (source edge) stream)
  (princ ", " stream)
  (princ (target edge) stream)
  (princ "}"  stream)
  edge)
  

;;; (new 'edge :source 'a :target 'b)
;;; (target (new 'edge :source 'a :target 'b))
;;; (class-of (new 'edge :source 'a :target 'b))

(defcl vedge (edge)
    "A VEDGE is an EDGE equiped with a value VAL: I union {nil},
for some type I. NIL may mean that the VEDGE does not hold!"
  val)

(defmethod print-object ((vedge vedge) stream)
  "Prints the VEDGE like an EDGE followed by |-> val, where val is
the edge value printout, then returns VEDGE."
  (call-next-method)     ; method inherited from EDGE class.
  (when (val vedge)
    (princ "|->" stream)
    (princ (val vedge) stream))
  vedge)

;;; (new 'vedge :source 'a :target 'b :val 3)
;;; (val (new 'vedge :source 'a :target 'b :val 3))
;;; (target (new 'vedge :source 'a :target 'b :val 3))
;;; (typep (new 'vedge :source 'a :target 'b :val 3) 'vedge)
;;; (typep (new 'vedge :source 'a :target 'b :val 3) 'edge)

(defparameter *indent-edges*
  0
  "Number of additional blank spaces to be output before
 printing EDGES:, so as to obtain a nice CGRAPH printout,
depending on kind of CGRAPH, monitored by PRINT-OBJECT methods
of CGRAPH subclasses.
Note: used by CGRAPH PRINT-OBJECT method.")

(defcl cgraph (graph)
  "A CGRAPH (read: concrete graph) on type V is a GRAPH on V plus
    EDGES: list[EDGE[V]]. This allows valued edges on some type I,
    with EDGES: list[VEDGE[V, I]], for some type I: in this case,
    we speak of a concrete graph on types V and I."
  edges)

(defmethod print-object ((cgraph cgraph) stream)
  "Prints <gg, edges: rr> where gg is printed by PRINT-OBJECT inherited from GRAPH,
and rr is EDGES printout, then returns CGRAPH."
  (princ "<" stream)
  (call-next-method)
  (fresh-line stream)
  (dotimes (k *indent-edges*) (princ " " stream))
  (format stream "           edges: ~a>" (edges cgraph))
  cgraph)

;;; (new 'cgraph :vertices '(a b c) :edges (list (new 'vedge :source 'a :target 'b :val 15) (new 'vedge :source 'b :target 'a) (new 'vedge :source 'b :target 'c :val 15)))

(provide "CGRAPHS")