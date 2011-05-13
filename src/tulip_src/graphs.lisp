;;;
;;; TP Group I1G2, ENSEIRB-MATMECA
;;; created 23 March 2011, last edition 27 March 2011
;;;  http://www.enseirb-matmeca.fr/~gloess/enseignement/CL/2010_2011/
;;;
;;; Purpose: Simple implementation of concrete graphs:
;;;          - set of vertices represented by a list;
;;;          - set of edges represented by a list.
;;;
(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SBCL" "sbcl")         ; defines SBCL-REQUIRE !!!
  (sbcl-require "SETS" "sets")
  (sbcl-require "DEFCL" "defcl"))


;;; Basis for graphs of various kinds:
;;; ---------------------------------
;;;
(defcl graph ()
  "This is the basis for different representations of a GRAPH,
    comprising VERTICES: list[V], for some type V."
  vertices)

(defmethod print-object ((graph graph) stream)
  "Prints GRAPH class, followed by GRAPH vertices, then returns GRAPH."
  (format stream "~a vertices: ~a" (string-capitalize (class-name (class-of graph))) (vertices graph))
  graph)

(defmethod equals ((graph1 graph) (graph2 graph))
  "Assuming GRAPH1, GRAPH2: GRAPH[V], for some type V,
    returns T iff GRAPH1 and GRAPH2 are equal, meaning that
     they have the same set of VERTICES."
  (set-equals (vertices graph1) (vertices graph2)))

(provide "GRAPHS")
