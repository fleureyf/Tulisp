;;;
;;; Paul Y Gloess, http://www.enseirb-matmeca.fr/~gloess
;;; created 7 March 2011, last edition 30 March 2011
;;;
;;; Addendum 30 March 2011:
;;;   - WITH-APPLICATION-DIRECTORY makes it possible to load
;;;       an application from a subdirectory of the current one.
;;;
;;; Purpose: Adapt REQUIRE and COMPILE-FILE to Sbcl environment
;;;           such as MacBookPro or ssh at ENSEIRB-MATMECA.
;;;           Note that it is likely that Mac compiled files
;;;            are not compatible with SSH, and vice versa.
;;;
;;; Note: This was adapted from previous "cmu.lisp" file
;;;        implemented at the time we were using CMU Common LISP, and
;;;        the compiler output depended on ENSEIRB-MATMECA machine.
;;;
(in-package :common-lisp-user)

(defun sbcl ()
  "Returns the string name of the feature :ELF or :MACH-O, whichever belongs
    to *FEATURES*. :ELF corresponds to SSH machine at ENSEIRB-MATMECA;
    :MACH-O corresponds to my MacBookPro."
  (string (car (or (member :elf *features*) (member :mach-o *features*)))))

(defun sbcl-compile-file (filename)
  "Same as COMPILE-FILE, but the output file goes into appropriate subdirectory
    corresponding to (SBCL)."
  (assert #+:X86 T #-:X86 nil)
  (compile-file filename :output-file (concatenate 'string (sbcl) "/" filename ".fasl")))

(defparameter *application-directory*
  '(:relative)
  "The relative directory of the application to be loaded,
current *DEFAULT-PATHNAME-DEFAULTS* directory by default.
For instance, (:relative \"tulip\") specifies the \"tulip\"
subdirectory of the current directory.")

(defun application-file (filename
			 &optional
			 (relative-directory *application-directory*)
			 (type "lisp"))
  "Assuming RELATIVE-DIRECTORY is a relative directory specification,
FILENAME is a string, representing the name of a file,
and TYPE is a string representing the file type, \"lisp\" or \"fasl\",
returns the corresponding pathname of the file."
  (merge-pathnames (make-pathname :directory relative-directory
				  :name filename
				  :type type)))

(defun application-source-file (filename
				&optional (relative-directory *application-directory*))
  "Assuming FILENAME and RELATIVE-DIRECTORY as in APPLICATION-FILE,
returns the correcsponding pathname of the \"lisp\" source file."
  (application-file filename relative-directory "lisp"))

(defun application-compiled-file (filename
				  &optional (relative-directory *application-directory*))
  "Assuming FILENAME and RELATIVE-DIRECTORY as in APPLICATION-FILE,
returns the correcsponding pathname of the \"fasl\" compiled file."
  (application-file filename `(,@relative-directory ,(sbcl)) "fasl"))

(defun sbcl-require (modulename filename)
  "Assuming MODULENAME is a string naming a module,
    and FILENAME is a string naming a file
    performs a REQUIRE of this module and this file
    in *APPLICATION-DIRECTORY* relative directory,
    using the \"fasl\" compiled version, from (sbcl) subdirectory,
    if the compiled version exists, or the \"lisp\" source
    version, otherwise."
  (require modulename (or (probe-file (application-compiled-file filename))
			  (application-source-file filename))))

(defmacro with-application-directory (application-directory
				      &body forms)
  "Assuming FORMS is a list of LISP forms,
Checks whether APPLICATION-DIRECTORY is a valid relative directory
specification, that is, either a list of the form
  (:RELATIVE string1 ... stringK)
or of the form
  (string1 ... stringK),
yielding an error if this is not the case,
then expands into a LET form that establishes a context for FORMS in which
*APPLICATION-DIRECTORY* is set to (:RELATIVE string1 ... stringK)."
  (labels ((proper-list-p (sexpr)
	     (or (null sexpr)
		 (and (consp sexpr) (proper-list-p (cdr sexpr))))))
    (unless (and (proper-list-p application-directory)
		 (or (null application-directory)
		     (eq :relative (car application-directory))
		     (stringp (car application-directory)))
		 (every #'stringp (cdr application-directory)))
      (error "~a is not a relative directory: should be a list of strings,
eventually prefixed with :RELATIVE keyword.~&" application-directory))
    (let ((application-directory (if (eq :relative (car application-directory))
				     application-directory
				     `(:relative . ,application-directory))))
      `(let ((*application-directory* ',application-directory))
	 . ,forms))))
;;; (macroexpand-1 '(with-application-directory ("tulip") (sbcl-require "EQUALITIES" "equalities")))
;;; (macroexpand-1 '(with-application-directory () (sbcl-require "EQUALITIES" "equalities")))

;;; Assuming current directory is "tulip"! This should load EQUALITIES module:
;;; (with-application-directory () (sbcl-require "EQUALITIES" "equalities"))

(provide "SBCL")