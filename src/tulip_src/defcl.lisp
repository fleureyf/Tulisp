;;;
;;; Paul Y Gloess, http://www.enseirb-matmeca.fr/~gloess
;;; created 15 February 2011, last edition 25 March 2011
;;;
;;; Purpose: DEFCL is a simplified version of DEFCLASS
;;;           for pure LISP: no side-effects, standard
;;;           :reader and :initarg.
;;;          NEW is a short for MAKE-INSTANCE.
;;;
(in-package :common-lisp-user)

(defmacro defcl (class-name
		 super-class-list
		 class-documentation-string
		 &body slot-specs)
  "Assuming CLASS-NAME is a symbol, and SUPER-CLASS-LIST is a list
    of just one symbol, CLASS-DOCUMENTATION-STRING is a string,
    and SLOT-SPECS is defined by the grammar

         slot-specs ::=   ()
                        | (slot-spec . slot-specs)
          slot-spec ::=   slot-name       ; means the same as (slot-name nil).
                        | (slot-name)     ; means no default.
                        | (slot-name slot-default)
          slot-name ::= symbol            ; all slot-name should be distinct.
       slot-default ::= lisp-form         ; valid in the context of defcl.        
                        
expands into a DEFCLASS that derives standard :READER and :INITARG
 and :INITFORM (when there is a slot-default) from each slot-spec in SLOT-SPECS,
 see code for detail!"
  (when (and (consp super-class-list) (consp (cdr super-class-list)))
    (warn "~%Multiple inheritance ~a is not a good idea!~%
Please try to stick to one superclass only!"
	   super-class-list))
  `(defclass ,class-name ,super-class-list
     ,(mapcar (lambda (slot-spec)
		(let ((slot-name (if (consp slot-spec)
				      (car slot-spec)
				      slot-spec)))
		  (unless (symbolp slot-name)
		    (error "~%Invalid slot name ~a in slot spec ~a." slot-name slot-spec))
		  `(,slot-name :initarg ,(intern (symbol-name slot-name) :keyword)
			       :reader ,slot-name
			       . ,(cond ((and (consp slot-spec) (consp (cdr slot-spec)))
					 `(:initform ,(second slot-spec)))
					((atom slot-spec)
					 `(:initform nil))
					(t nil)))))   ; (slot-name) => no default.
	      slot-specs)
     (:documentation ,class-documentation-string)))

;;; (load "defcl")
;;; (macroexpand-1 '(defcl person () "A person" name (sex 'female) (age)))
;;; (macroexpand-1 '(defcl employee (person) "An employee" salary))

(defmacro new (&rest args)
  "Just a short synonymous of MAKE-INSTANCE CLOS function."
  `(make-instance . ,args))

;;; Evaluate the following forms in THIS ORDER in *slime-repl sbcl* buffer
;;;  (DO NOT use C-x C-e !):
;;;
;;; (load "defcl")
;;; (defcl person () "A person" name (sex 'female) (age))
;;; (defcl employee (person) "An employee" salary)
;;; (new 'person :sex 'male :age 50 :name "Michael Jackson")
;;;   (name *)
;;; (name (new 'person))   ; => NIL
;;; (sex (new 'person))    ; => FEMALE
;;; (age (new 'person))    ; ERROR because AGE is unbound!
;;; (new 'employee :sex 'female :age 45 :name "Madonna" :salary 50000000000000)
;;;   (name *)
;;;   (salary **)

(provide "DEFCL")