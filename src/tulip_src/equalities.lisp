;;;
;;; TP Group I1G2, ENSEIRB-MATMECA
;;; created 27 March 2011, last edition 27 March 2011
;;;  http://www.enseirb-matmeca.fr/~gloess/enseignement/CL/2010_2011/
;;;
;;; Purpose: introduce two kinds of equality:
;;;  -     EQUALS: a method version of EQUALP function, for testing the equality of
;;;                 two LISP objects;
;;;  - SET-EQUALS: similar to EQUALS, but differs on lists (or, eventually, other sequences)
;;;                 by considering them as sets.
;;;
;;; Remark: Only DEFGENERICs and default (most general) DEFMETHODs are provided here;
;;;         more specific methods are provided in relevant modules (for instance,
;;;          in SETS module, wrt list SET-EQUALS method).
;;;  
(in-package :common-lisp-user)

(defgeneric  equals (sexpr1 sexpr2)
  (:documentation "Defines equality as a kind of extendible EQUALP."))

(defmethod equals ((sexpr1 t) (sexpr2 t))
  "Default EQUALS is EQUALP!"
  (equalp sexpr1 sexpr2))

;;; (equals 128 (+ 124 4))

(defgeneric set-equals (sexpr1 sexpr2)
  (:documentation "Defines equality treatingconsidering lists as sets."))

(defmethod set-equals ((sexpr1 t) (sexpr2 t))
  "Default SET-EQUALS is EQUALS!"
  (equals sexpr1 sexpr2))

(provide "EQUALITIES")