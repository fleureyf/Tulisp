;;;
;;; TP Group I1G2, ENSEIRB-MATMECA
;;; created 17 March 2009, last edition 27 March 2011
;;;  http://www.enseirb-matmeca.fr/~gloess/enseignement/CL/2010_2011/
;;;
;;; Purpose: Define some operations or predicates on sets
;;;           represented as lists, not part of Common LISP
;;;           library. Note that we define EQUALS as a
;;;           general purpose method, since it is not possible
;;;           to extend EQUALP function with method definitions.
;;;
(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SBCL" "sbcl"))         ; defines SBCL-REQUIRE !!!

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sbcl-require "EQUALITIES" "equalities"))

(defun subset-p (list1 list2 &key (test #'equalp))
  "Assuming LIST1, LIST2: list[A], for some type A,
    and TEST: [A, A -> bool],
    returns T iff the set represented by LIST1 is included (or equal)
     to the set represented by LIST2, w.r.t. the specified TEST
     equality among elements of type A."
  (null (set-difference list1 list2 :test test)))

;;; (subset-p '(a b a b) '(c b a))    ; |-> T.
;;; (subset-p '(c b a) '(a b a b))    ; |-> NIL

(defun list-set-equals (list1 list2 &key (test #'equals))
  "Assuming LIST1, LIST2: list[A], for some type A,
    and TEST: [A, A -> ool],
    returns T iff the sets represented by LIST1 and LIST2
    are equal, w.r.t. the specified TEST equality among elements
    of type A."
  (and (subset-p list1 list2 :test test)
       (subset-p list2 list1 :test test)))

(defmethod set-equals ((list1 list) (list2 list))
  "Assuming LIST1 and LIST2 are clean lists, representing sets,
    returns T iff LIST1 and LIST2 sets are equal."
  (list-set-equals list1 list2))

;;;                   (set-equals '(a b c) '(b a a c))     ; => T.
;;;                 (set-equals '(a b a c) '(c b a b))     ; => T.
;;; (set-equals '("a" "b" "a" "c") '("c" "b" "a" "b"))     ; => T.
;;;                 (set-equals '(1 2 1 3) '(3 2 1 2))     ; => T.

(defun star (list1 list2)
  "Assuming LIST1 and LIST2 are lists,
    returns a list representing the cartesian product of the sets they represent,
     precisely a list of all pairs of the form (e1 . e2) with e1 in L1 and E2 in L2."
  (when list1
    (append (mapcar (lambda (e2) (cons (car list1) e2)) list2)
	    (star (cdr list1) list2))))

(defun star2 (list)
  "Assuming LIST is a list,
    returns (STAR LIST LIST)."
  (star list list))

(defparameter *void*
  (list 'void)
  "A unique object, other than NIL, meaning roughly the same as NIL.")

(defun all (predicate list &key (map #'identity))
  "Assuming PREDICATE: [U -> Bool], LIST: list[U], for some type U,
 and MAP: [U -> V], for some type V,
returns the list of MAP images of all LIST elements satisfying PREDICATE."
  (remove *void*
	  (mapcar (lambda (u) (if (funcall predicate u)
				  (funcall map u)
				  *void*))
		  list)))

;;;                           (all #'evenp '(1 2 3 4 5 6 7 8))   ; => (2 4 6 8)
;;; (all #'evenp '(1 2 3 4 5 6 7 8) :map (lambda (n) (/ n 2)))   ; => (1 2 3 4)

(provide "SETS")