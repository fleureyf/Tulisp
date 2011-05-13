;;;
;;; TP Group I1G2, ENSEIRB-MATMECA
;;; created 31 March 2009, last edition 11 April 2011
;;;  http://www.enseirb-matmeca.fr/~gloess/enseignement/CL/2010_2011/
;;;
;;; Note: 11 April 2011: - Fixed import and export bugs, related to case preserving:
;;;                        replaced node with |node| in
;;;                        - TULIP2VERTICES, TULIP2EDGES (fixed TULIP2ALIST documentation),
;;;                        - PGRAPH2TLP
;;;                        Also replaced color with |color| in TULIP-BEST-CONTROLLER.
;;; Note:  5 April 2011: - used PRIN1 instead of PRINT in TGRAPH PRINT-OBJECT
;;;                         method, to avoid skipping a line at the beginning
;;;                         of .tlp file.
;;;                      - replaced SYMBOL-EQ recognizer builder with more
;;;                         tolerant NAME-EQUALP-FUN, because of the use
;;;                         of *PRESERVE-CASE-READTABLE*. All symbols are replaced
;;;                         with strings, as arguments of NAME-EQUALP-FUN.
;;;                         Remark: recognizer could be macro generated.
;;;                      - same as below for all Tulip directive symbols in
;;;                         PGRAPH2TLP function. Also fixed bad date directive
;;;                         generation.
;;;                      - surrounded all symbols in TULIP-BEST-CONTROLLER
;;;                         body with |...|, so that they keep their case,
;;;                         from Tulip language. Note that *PRINT-CASE*
;;;                         should have no effect, since we set the readtable
;;;                         to *PRESERVE-CASE-READTABLE* during the export.
;;;                      - changed DATE and COMMENTS defaults in TGRAPH class
;;;                         for compatibility with Tulip.
;;;                      - removed error message from DEFGRAPH, in order to
;;;                         allow any file specification instead of just a string.
;;;                      - used *PRESERVE-CASE-READTABLE* for exporting also,
;;;                         which should leave DataSet symbol unchanged!
;;;                      - introduced INTERVAL class, for correct printout
;;;                         of NODES specified interval
;;;                         in PGRAPH2TLP function: was "0..5" instead of 0..5.
;;; Note:  4 April 2011: corrected PGRAPH2TLP function
;;;                      - (nb_nodes ...) directive commes before (nodes ...) directive,
;;;                      - added (nb_edges ...) directive before (edges ...) directive.
;;; Note: 30 March 2011: added DEFPGRAPH macro for more convenient import
;;;                       of .tlp files: the user can specify the global
;;;                       parameter name of his choice without having to
;;;                       hack the .tlp file (no need to replace TLP
;;;                       with DEFTULIP).
;;;                        Example: (defpgraph *pgraph1* "example1.tlp")
;;;
;;; Note: 28 March 2011: the whole application has been entirely redesigned
;;;                       on top of "defcl" (CLOS): it handles most (if not all)
;;;                       of Tulip graph representation language.
;;; Note:  7 March 2011: adapted from CMU to SBCL, and fixed
;;;                       EVAL-WHEN directives according to
;;;                       HyperSpec recommendations.
;;;
;;; How to load Tulip (faster after Tulip application has been compiled):
;;; -----------------
;;;   (progn (require "SBCL" "sbcl") (sbcl-require "TULIP" "tulip"))
;;;
;;; How to compile Tulip application:
;;; --------------------------------
;;;   - Go to "tulip" directory (containing all source .lisp files and .tlp examples):
;;;     - make sure there is an empty "ELF" ("tulip/ELF") subdirectory, if you are under Linux,
;;;        or an empty "MACH-O" ("tulip/MACH-O") subdirectory, if you are under Mac OS X;
;;;   - Start a brand new LISP (SBCL) session from this ("tulip/") directory, with Slime;
;;;   - Evaluate the following form from *slime-repl sbcl* buffer:
;;;        (load "sbcl-compile-all")
;;;   - Exit, and check the contents of "ELF" or "MACH-O" subdirectory.
;;;
;;; Purpose: Provide Tulip graph format output and input, see
;;;   http://tulip.labri.fr/tlpformat.php or http://tulip.labri.fr/TulipDrupal/   .
;;;
;;;   - Tulip output:
;;;     ------------
;;;     A TGRAPH structure is defined as a PGRAPH clone, except for
;;;      being equiped with an appropriate print function (in
;;;      agreement with Tulip format), mainly based upon PGRAPH2TLP function.
;;;     A TLP-FILE method is defined to output PGRAPH or TGRAPH printout to a tlp file.
;;;
;;;   - Tulip input:
;;;     -----------
;;;     Tulip graphs files (.tlp type ) contain a (usually unique) LISP list of the form
;;;       (tlp . ...)
;;;     In order to be able to load Tulip files into LISP, using the LOAD or similar LISP
;;;      functions, we define TLP as a macro expanding (through DEFTULIP) into
;;;        (defparameter *<automatically-generated-symbol>*
;;;          "String from COMMENTS form found in the file"
;;;          (new 'pgraph
;;;               :vertices ... :edges ... :props ...
;;;               :version ... :date ... :author :comments ... :attributes ... :controller ...))
;;;     We also provide a DEFTULIP macro for doing roughly the same, with
;;;      a name provided by the user, to be used in the DEFPARAMETER. This
;;;      would require a slight modification of the tlp file.
;;;  
(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SBCL" "../tulip_bin/sbcl"))         ; defines SBCL-REQUIRE !!!

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sbcl-require "CGRAPHS" "cgraphs")
  (sbcl-require "TIME" "time"))

(defparameter *preserve-case-readtable*
  (let ((readtable (copy-readtable)))
    (setf (readtable-case readtable)
	  :preserve)
    readtable)
  "Copy of the current readtable,
except for READTABLE-CASE being set to :PRESERVE,
so that symbols read from .tlp files just keep their
exact case, and are printed in their original case,
without the surrounding |...|. For instance, in .tlp
files, we find the directive (DataSet ...) and
DataSet has to be kept that way; otherwise, the
Tulip graph visualization tool crashes!")

;;;
;;; Tulip Property definition representation:
;;;
(defcl property ()
    "Definition of a Tulip property."
  num              ; a natural number, often 0.
  typ              ; a symbol, denoting a type, e.g., layout. *** TYPE is locked!!!
  name             ; a symbol in the keyword package, naming the property.
  node-default     ; a LISP object (sometimes a list, sometimes a number),
                   ;   derived from a Tulip string.
  edge-default)    ; same as for NODE-DEFAULT.

(defmethod print-object ((property property) stream)
  "Prints like P name': typ', where name' and typ' respectively denote
 the string versions of PROPERTY NAME and TYP,
then returns PROPERTY."
  (format stream "{P ~a: ~a}" (string (name property)) (string-downcase (typ property)))
  property)

;;;
;;; Identity card, used as a basis for both vertices and edges:
;;;
(defcl card ()
    "Identity card: a basis for PVERTEX and PEDGE classes.
provides an identifier through the ID slot."
    id        ; identifier uniquely associated with the card.
    alist)    ; an association list between Tulip property names and values.


(defmethod equals ((card1 card) (card2 card))
  "Returns T iff CARD1 and CARD2 have the same ID.
Note that we rely upon the assumption that there are no two vertices or
two edges in the same PGRAPH with the same ID. This property should be maintained
when merging graphs."
  (equals (id card1) (id card2)))

;;;
;;; Vertices with Tulip property information
;;;
(defcl pvertex (card)
    "A vertex with Tulip properties: property names are keywords.")

(defmethod print-object ((pvertex pvertex) stream)
  "Assuming STREAM is a stream, prints PVERTEX as V-N with N = (id PVERTEX),
then returns PVERTEX."
  (format stream "V-~a" (id pvertex)))

(defcl pedge (card vedge)       ; multiple inheritance (avoid!).
    "A valued edge adapted to Tulip purposes.
Node that the VAL slot inherited from VEDGE is not used by Tulip.")

(defmethod equals ((pedge1 pedge) (pedge2 pedge))
  "Returns T iff PEDGE1 and PEDGE2 are the same edges,
orientation wise. We implicitely assume oriented graphs here.
Note that we do not use ID here, although we could probably do so."
  (and (equals (source pedge1) (source pedge2))
       (equals (target pedge1) (target pedge2))))

(defcl controller ()
    "Holds the Tulip controller directives in TULIP slot:
Avoids a very lengthy printout, when inspecting a Pgraph, caused
by huge strings belonging to these directives."
  (tulip (tulip-best-controller)))

;;;
;;; Concrete graphs with Tulip properties and controllers
;;;
(defcl pgraph (cgraph)
    "Same as CGRAPH, except that it is equiped with Tulip property definitions
and controller information, for Tulip graphic visualization purposes."
  props         ; list of Tulip property definitions.
  version       ; Tulip version, a string, e.g., "2.2" (see Tulip version directive).
  date          ; date of generation of tlp file, a string, e.g., "03-14-2011" (see Tulip date directive).
  author        ; a string naming the author: does not seem to be used by Tulip 2.2.
  comments      ; a string, e.g., "This file was generated by Tulip." (see Tulip comments directive).
  attributes    ; a list of Tulip attributes (cdr of attributes directive).
  controller)   ; an instance of CONTROLLER class.

(defmethod print-object ((pgraph pgraph) stream)
  "Prints <cg, properties: N> on STREAM,
where CG is inherited printout, and N the number of properties,
then returns PGRAPH."
  (princ "<" stream)
  (let ((*indent-edges* (1+ *indent-edges*)))
    (call-next-method))          ; add one extra space because of leading "<".
  (format stream "~&   properties: ~a>" (length (props pgraph)))
  pgraph)

;;;
;;; Tulip output:
;;; ------------
;;;
(defcl tgraph (pgraph)
    "Same as PGRAPH, except that it comes with a printing
function that produces Tulip file format, and has default
values for DATE, COMMENTS and CONTROLLER slots."
  (date (month-date-year-string))
  (comments (format nil "Tlp generated by LISP/Tulip interface from Tgraph instance.~%~a"
		    (universal-time-string)))
  (controller (new 'controller)))

(defmethod print-object ((tgraph tgraph) stream)
  "Prints the TGRAPH on STREAM using PGRAPH2TLP for the main part,
    then returns TGRAPH."
  (let ((*print-case* :downcase))       ; print all symbols in lowercase.
                                        ;  this should now have no effect because
                                        ;  of *PRESERVE-CASE-READTABLE*!!!
    (prin1 (pgraph2tlp tgraph) stream)  ; should start printing on first line!
    tgraph))

(defgeneric 2tgraph (pgraph)
  (:documentation "Conversion to TGRAPH."))

(defmethod 2tgraph ((pgraph pgraph))
  "Returns a PGRAPH almost clone as a TGRAPH instance,
with current DATE, different COMMENTS and best CONTROLLER available."
  (new 'tgraph :vertices (vertices pgraph)
	       :edges (edges pgraph)
	       :props (props pgraph)
	       :version (version pgraph)
	       :attributes (attributes pgraph)
	       :author (author pgraph)))

(defgeneric val-string (item)
  (:documentation
   "Yields the Tulip representation of a LISP value (that was produced by READ-FROM-TULIP-STRING).
Note that (val-string (read-from-tulip-string item)) = item."))

(defmethod val-string ((the-nil null))
  "The Tulip representation of NIL default value is \"()\"."
  "()")
;;; (val-string nil)  ; => "()"

(defmethod val-string ((val symbol))
  "Tulip representation of a symbolic value is its string name."
  (symbol-name val))
;;;                         (val-string :|false|)  ; => "false"
;;; (val-string (read-from-tulip-string "false"))  ; => "false"

(defmethod val-string ((val number))
  "Tulip representation of a numerical value is its string representation."
  (princ-to-string val))
;;; (val-string 123)  ; "123"

(defmethod val-string ((l list))
  "Tulip representation of a list value uses a comma for separator."
  (substitute #\, #\SPACE (princ-to-string l)))
;;; (val-string '(85 170 255 255))  ; => (85,170,255,255)

(defmethod val-string ((sexpr t))
  "Tulip default representation of a LISP value is the LISP value itself!
Normally, this method will only apply to strings: they will be left unchanged."
  sexpr)
;;; (val-string (read-from-tulip-string ""))  ; => ""
;;;               (val-string "usualString")  ; => "usualString"

;;;
;;; This method is useful for allowing PEDGEs like
;;;  {5, 7} instead of the recommended {V-5, V-7}:
;;;
(defmethod id ((sexpr T))
  "Default ID method: like identity function."
  sexpr)

(defcl interval ()
    "An interval in nat."
  low          ; minimum of the interval.
  hi)          ; maximum of the interval.

(defmethod print-object ((interval interval) stream)
  "The interval [0, 6] prints 0..6.
After printing, returns the INTERVAL."
  (format stream "~a..~a" (low interval) (hi interval))
  interval)

;;; Do not use this function, use 2TGRAPH and 2TLP-FILE methods instead.
(defun pgraph2tlp (pgraph)
  "Assuming PGRAPH is a PGRAPH (for example, a TGRAPH instance),
    returns a list in the Tulip file format, corresponding to the graph
    represented by PGRAPH.
    Remark: we ignore edge values, if any."
  (labels ((nat-interval-p (nats)
	     ;; Assuming NATS: list[nat],
	     ;;  returns T iff NATS is an ordered sequence forming an interval in nat.
	     (or (null nats)
		 (null (cdr nats))
		 (and (= 1 (- (second nats) (first nats)))
		      (nat-interval-p (cdr nats))))))
    (let ((vertices (vertices pgraph))
	  (edges (edges pgraph))
	  (comments (comments pgraph)))
      `(|tlp| ,(version pgraph)
	      (|date| ,(date pgraph))
	      ,@(when (author pgraph) (list `(|author| ,(author pgraph))))
	      (|comments| . ,(if (stringp comments)
				 (list comments)
				 comments))
	      (|nb_nodes| ,(length vertices))
	      (|nodes| . ,(let ((v-ids (sort (mapcar #'id vertices) #'<)))
			       (if (nat-interval-p v-ids)
				   (list (new 'interval :low (car v-ids) :hi (car (last v-ids))))
				   v-ids)))    ; Tulip 2.2 files seem to contain this directive.
	      ;;
	      ;; Generate EDGE directives from EDGES, as a list of elements of the form
	      ;;   ((EDGE id1 source-id1 target-id1) ... (EDGE idN source-idN target-idN))
	      ;; where each idK is the edge identifier, and source-idK and target-idK are
	      ;;  its respective SOURCE and TARGET vertex identifiers.
	      (|nb_edges| ,(length edges))
	      ,@(mapcar (lambda (edge) `(|edge| ,(id edge)
						,(id (source edge))
						,(id (target edge))))
			(sort edges #'< :key #'id))
	      ;;
	      ;; Generate the PROPERTY directives from PGRAPH PROPS, VERTICES and EDGES,
	      ;;  so that for each PROPERTY directive, the relevant vertices and edges
	      ;;  are added to that PROPERTY directive, unless their ALIST value for
	      ;;  this PROPERTY is the same as the PROPERTY default.
	      ;;
	      ,@(mapcar (lambda (property)
			  `(|property| ,(num property) ,(typ property) ,(string (name property))
				       (|default|
					,(val-string (node-default property))
					,(val-string (edge-default property)))
				       ,@(remove nil (mapcar (lambda (vertex)
							       (let ((val (cdr (assoc (name property) (alist vertex)))))
								 (unless (equals val (node-default property))
								   `(|node| ,(id vertex) ,(val-string val)))))
							     vertices))
				       ,@(remove nil (mapcar (lambda (edge)
							       (let ((val (cdr (assoc (name property) (alist edge)))))
								 (unless (equals val (edge-default property))
								   `(|edge| ,(id edge) ,(val-string val)))))
							     edges))))
			(props pgraph))
	      (|attributes| . ,(attributes pgraph))
	      (|controller| . ,(tulip (controller pgraph)))))))

(defgeneric 2tlp-file (pgraph filename)
  (:documentation "Outputs PGRAPH Tulip printout to specified FILENAME.tlp (in current directory)."))

(defmethod 2tlp-file ((tgraph tgraph) (filename string))
  "Outputs TGRAPH printout to filename.tlp, in current directory,
then returns the pathname and TGRAPH corresponding to filename.tlp.
Remark: if filename.tlp exists, its contents are erased and replaced with the new output."
  (with-open-file (tlp (merge-pathnames (make-pathname :name filename :type "tlp"))
		       :direction :output :if-exists :supersede)
    (let ((*readtable* *preserve-case-readtable*))    ; has an effect on printing also!
      ;;
      ;; Symbols like |DataSet| will be printed DataSet.
      (print-object tgraph tlp))
    (values tlp tgraph)))

(defmethod 2tlp-file ((pgraph pgraph) (filename string))
  "Applies 2TGRAPH conversion to PGRAPH, allowing the use of TGRAPH 2TLP-FILE method."
  (2tlp-file (2tgraph pgraph) filename))


;;;
;;; Tulip input:
;;; -----------
;;;   - TLP macro: a Tulip (.tlp) file contains a big (tlp . ...) list;
;;;   - DEFTULIP macro: this will require editing the .tlp file ...
;;;

(defun read-from-tulip-string (string)
  "Same as READ-FROM-STRING, except that case is preserved,
and symbols are interned in the keyword package,
and commas are first replaced with blank spaces, so that lists are correctly generated.
Empty string yields itself."
  (let ((*package* (find-package :keyword))
	(*readtable* *preserve-case-readtable*))
    (read-from-string (substitute #\SPACE #\, string)
		      nil "")))    ; no EOF error, return "" (normally, case of empty string).

;;;      (read-from-tulip-string "(1.50, 2, -4.67, 12)")  ; => (1.5 2 -4.67 12)
;;;           (read-from-tulip-string "viewBorderColor")  ; => :|viewBorderColor|
;;;                          (read-from-tulip-string "")  ; => ""                       ;  
;;; (symbolp (read-from-tulip-string "viewBorderColor"))  ; => T.

;; (defun symbol-eq (symbol)
;;   "Assuming SYMBOL: symbol,
;;     returns a function for testing that a list has the form (SYMBOL . _)."
;;   (lambda (cons) (eq symbol (car cons))))

;;; (funcall (symbol-eq 'property) '(property  0 color "viewBorderColor" (default "(0,0,0,255)" "(0,0,0,255)")))

;;; The commented out SYMBOL-EQ function does not work anymore as a Tulip directive
;;;  recognizer builder, because of the use of *PRESERVE-CASE-READTABLE*: to make test
;;;  more robust, we build a recognizer that uses EQUALP on symbol names!
(defun name-equalp-fun (string)
  "Assuming STRING: string,
returns a fonction for testing that a list has the form (symbol . _),
 where symbol has a name EQUALP to STRING."
  (lambda (cons)
    (equalp string (symbol-name (car cons)))))

;;; (funcall (name-equalp-fun "property") '(property  0 color "viewBorderColor" (default "(0,0,0,255)" "(0,0,0,255)")))
    

(defparameter *tulip-pgraph-defparameter-name*
  nil
  "The name of a global variable to be used ar the parameter in the DEFPARAMETER resulting
from the expandion of TLP macro, or NIL, if the parameter name should be generated
automatically.")

(defmacro defpgraph (pgraph-defparameter-name tlp-file)
  "Assuming PGRAPH-DEFPARAMETER-NAME is a valid <symbol> for naming
a global variable, and TLP-FILE is a <form> (normally yielding a string or pathname
denoting a .tlp file), expands into a
   (let ((*tulip-pgraph-defparameter-name* '<symbol>))
     (load <form>))
that will be equivalent, when executed, to a
   (defparameter <symbol> '<pgraph-instance> <doc-string>)
form, binding <symbol> to the <pgraph-instance> represented
by TLP-FILE."
  (unless (and pgraph-defparameter-name (symbolp pgraph-defparameter-name))
    (error "Invalid parameter name ~a in DEFPGRAPH.~&" pgraph-defparameter-name))
;;;   (unless (stringp tlp-file)
;;;     (error "Invalid .tlp file name ~a in DEFPGRAPH.~&" tlp-file))
  `(let ((*tulip-pgraph-defparameter-name* ',pgraph-defparameter-name)
	 (*readtable* *preserve-case-readtable*))   ; keep symbol exact case! Useful for DataSet!
     (load ,tlp-file)))

;;; For debugging purposes:
;;; a simple tlp macro, which is not the |tlp| real one!
(defmacro tlp (tulip-version &rest tulip-directives)
  "Sets *TULIP-DIRECTIVES* to the list of all directives."
  (declare (ignore tulip-version))
  (warn "You are not using the proper readtable for a tlp file!
 Hence, the Pgraph instance will not be correct, because symbol case
 is not preserved. You should use DEFPGRAPH rather than LOAD to
 safely import a tlp file. The TLP macro is not the |tlp| macro:
 it is used for debugging purposes only!")
  `(defparameter *tulip-directives* ',tulip-directives
     "List of all directives!"))
  
(defmacro |tlp| (tulip-version &rest tulip-directives)
  "Assuming TULIP-VERSION is a string, and TULIP-DIRECTIVES are as specified in
    Tulip format documentation, see http://tulip.labri.fr/tlpformat.php,
    expands into a DEFTULIP form
      (deftulip *<name-generated-automatically-or-not>*
        \"text resulting from the COMMENTS directive\"
        ...)
    whose body is made of the TULIP-DIRECTIVES."
  `(deftulip ,(tulip-pgraph-defparameter-name tulip-directives)
       ,tulip-version
       ,(tulip2comments tulip-directives)
     . ,tulip-directives))
;;; (load "example1.tlp")

(defmacro deftulip (defparameter-name tulip-version documentation &body tulip-directives)
  "Assuming DEFPARAMETER-NAME: symbol, TULIP-VERSION, DOCUMENTATION: string,
    and TULIP-DIRECTIVES is as in TLP macro,
    expands into a DEFPARAMETER form documented by DOCUMENTATION, and
    setting this symbol to a PGRAPH corresponding to TULIP-VERSION and TULIP-DIRECTIVES."
  (format t "Defining ~a as a Pgraph instance.~&" defparameter-name)
  (let ((tulip2vertices (tulip2vertices tulip-directives)))
    `(defparameter ,defparameter-name     
       (new 'pgraph
	    :vertices ',(tulip2vertices tulip-directives)
	    :edges ',(tulip2edges tulip-directives tulip2vertices)
	    :props ',(tulip2props tulip-directives)
	    :version ,tulip-version
	    :date ,(tulip2date tulip-directives)
	    :author ,(tulip2author tulip-directives)
	    :comments ',(tulip2comments tulip-directives)
	    :attributes ',(tulip2attributes tulip-directives)
	    :controller ',(tulip2controller tulip-directives))
       ,documentation)))

(defun property-directives (tulip-directives)
    "Assuming TULIP-DIRECTIVES is as specified in TLP macro,
      returns the list of all property directives belonging
      to TULIP-DIRECTIVES. Here is the syntax of a property directive:

       property-directive ::= (PROPERTY num typ name default
                                . prop-node-edge-directives)
                  default ::= (DEFAULT node-default edge-default)
       property-directive-node-edge-directives ::=   ()
                                     | ((NODE node-val) . property-directive-node-edge-directives)
                                     | ((EDGE edge-val) . property-directive-node-edge-directives) ."
    (all (name-equalp-fun "property")
	 tulip-directives))

(defun property-directive-num (property-directive)
  "Assuming PROPERTY-DIRECTIVE as explained in PROPERTY-DIRECTIVES,
returns the num component."
  (second property-directive))

(defun property-directive-typ (property-directive)
  "Assuming PROPERTY-DIRECTIVE as explained in PROPERTY-DIRECTIVES,
returns the typ component."
  (third property-directive))

(defun property-directive-name (property-directive)
  "Assuming PROPERTY-DIRECTIVE as explained in PROPERTY-DIRECTIVES,
returns the name component."
  (fourth property-directive))

(defun property-directive-default (property-directive)
  "Assuming PROPERTY-DIRECTIVE as explained in PROPERTY-DIRECTIVES,
returns the default component."
  (fifth property-directive))

(defun property-directive-node-default (property-directive)
  "Assuming PROPERTY-DIRECTIVE as explained in PROPERTY-DIRECTIVES,
returns the node-default component."
  (second (property-directive-default property-directive)))

(defun property-directive-edge-default (property-directive)
  "Assuming PROPERTY-DIRECTIVE as explained in PROPERTY-DIRECTIVES,
returns the edge-default component."
  (third (property-directive-default property-directive)))

(defun property-directive-node-edge-directives (property-directive)
  "Assuming PROPERTY-DIRECTIVE as explained in PROPERTY-DIRECTIVES,
returns the prop-node-edge-directives component."
  (cdr (cddddr property-directive)))

(defun node-edge-directive-num (node-edge-directive)
  "Assuming NODE-EDGE-DIRECTIVE is an element of PROPERTY-DIRECTIVE-NODE-EDGE-DIRECTIVES,
see PROPERTY-DIRECTIVE function documentation,
 returns the NUM component."
  (second node-edge-directive))

(defun node-edge-directive-val (node-edge-directive)
  "Assuming NODE-EDGE-DIRECTIVE is an element of PROPERTY-DIRECTIVE-NODE-EDGE-DIRECTIVES,
see PROPERTY-DIRECTIVE function documentation,
 returns the VAL component."
  (third node-edge-directive))

(defun tulip2props (tulip-directives)
  "Assuming PROPERTY-DIRECTIVES is a list of property directives
according to property-directive syntax defined in PROPERTY-DIRECTIVE
function,
returns the list of all Tulip PROPERTY instances corresponding
 to PROPERTY-DIRECTIVES function."
  (mapcar (lambda (property-directive)
	    (new 'property
		 :num (property-directive-num property-directive)
		 :typ (property-directive-typ property-directive)
		 :name (intern (property-directive-name property-directive)
			       (find-package :keyword))    ; symbol in keyword package.
		 :node-default (read-from-tulip-string (property-directive-node-default property-directive))
		 :edge-default (read-from-tulip-string (property-directive-edge-default property-directive))))
	  (property-directives tulip-directives)))

;;; (tulip2props '((property  0 color "viewBorderColor" (default "(0,0,0,255)" "(0,0,0,255)"))))
;;; (tulip2props '((property 0 color "viewColor" (default "(85,170,255,255)" "(0,0,0,255)"))))
;;; (load "example1.tlp")
;;; **** test only: (tulip2props (property-directives *tulip-directives*))

(defun str-interval-low (low..hi)
  "Assuming LOW..HI is a string of the form \"lll..hhh\" where lll and hhh are the
representations of two natural numbers LOW and HI, returns LOW.
Remark: we accept a symbol instead of a string."
  (read-from-string (string-right-trim "." (string-right-trim "0123456789" (string low..hi)))))

(defun str-interval-hi (low..hi)
  "Assuming LOW..HI is a string of the form \"lll..hhh\" where lll and hhh are the
representations of two natural numbers LOW and HI, returns HI.
Remark: we accept a symbol instead of a string."
  (read-from-string (string-left-trim "." (string-left-trim "0123456789" (string low..hi)))))

(defun closed-interval (low hi)
  "Assuming LOW, HI: nat,
returns the closed interval [LOW, HI] in nat as a list, which is empty iff LOW > HI."
  (labels ((downto-low-app (hi nat-list)
	     ;; Assuming NAT-LIST: list[nat],
	     ;;  returns (append [LOW, HI] NAT-LIST).
	     (if (> low hi)
		 nat-list
		 (downto-low-app (1- hi) (cons hi nat-list)))))
    (downto-low-app hi nil)))

;;; (closed-interval 1 5)   ; => (1 2 3 4 5)
	       
(defun tulip-nodes-directive (tulip-directives)
  "Assuming TULIP-DIRECTIVES is as in TLP macro,
    returns the (NODES . nodes-spec) Tulip nodes directive found in TULIP-DIRECTIVES."
  (car (member-if (name-equalp-fun "nodes") tulip-directives)))

(defun tulip2alist (tulip-item-type tulip-item-id default-fun tulip-directives)
  "Assuming TULIP-ITEM-TYPE: {|node|, |edge|},
            TULIP-ITEM-ID: nat, identifies a node or edge,
            DEFAULT-FUN: {#'property-directive-node-default,#' property-directive-edge-default},
            TULIP-DIRECTIVES: as specified in TLP macro,
 returns the association list of all pairs (name' . val) such that (TULIP-ITEM-TYPE TULIP-ITEM-ID val) belongs to
           (PROPERTY-DIRECTIVE-NODE-EDGE-DIRECTIVES property-directive)
          for some property-directive in TULIP-DIRECTIVES, whose NAME is name,
           or val is the default computed by DEFAULT-FUN for this property-directive,
           and name' is the keyword package symbol version of name string."
  (mapcar (lambda (property-directive)
	    (let ((item-tail?
		   (member `(,tulip-item-type ,tulip-item-id DEFAULT)
			   (property-directive-node-edge-directives
			    property-directive)
			   :test (lambda (item-dir1 item-dir2)
				   (and (eq (car item-dir1) (car item-dir2))
					(= (node-edge-directive-num item-dir1)
					   (node-edge-directive-num item-dir2)))))))
	      `(,(intern (property-directive-name property-directive) (find-package :keyword))
		 . ,(read-from-tulip-string
		     (if item-tail?
			 (node-edge-directive-val (car item-tail?))
			 (funcall default-fun property-directive))))))
	  (property-directives tulip-directives)))

(defun tulip2vertices (tulip-directives)
  "Assuming TULIP-DIRECTIVES as specified in TLP macro,
returns the list of VERTEX instances corresponding to nodes-directive
 found in TULIP-DIRECTIVES, where
    nodes-directive ::=   (nodes m..n)
                        | (nodes . numbers)
 with each vertex ALIST set to a property / value association list collecting
  all data from relevant property directives associated with this vertex,
  see TULIP2ALIST for details."
  (let* ((nodes-spec (cdr (tulip-nodes-directive tulip-directives)))
	 (node-nums (if (and (consp nodes-spec) (symbolp (car nodes-spec)))
			(closed-interval (str-interval-low (car nodes-spec))
					 (str-interval-hi (car nodes-spec)))
			nodes-spec)))
    (mapcar (lambda (node-num)
	      (new 'pvertex
		   :id node-num
		   :alist (tulip2alist '|node| node-num #'property-directive-node-default tulip-directives)))
	    node-nums)))

;;; **** TEST ONLY! (load "example1.tlp")
;;; **** TEST ONLY! (tulip2vertices *tulip-directives*)

(defun tulip2comments (tulip-directives)
  "Assuming TULIP-DIRECTIVES is as specified in TLP macro,
   returns
    if (COMMENTS . (comment-string1 ... comment-stringK)) belongs to TULIP-DIRECTIVES,
     for some K >=1, as the first element of that form,
     then comment-string1 if K=1, (comment-string1 ... comment-stringK) if K>1,
     else NIL."
  (let ((comments-tail (cdr (car (member 'comments tulip-directives :key #'first)))))
    (if (consp (cdr comments-tail))
	comments-tail
	(first comments-tail))))
;;; (tulip2comments '((comments "aaa" "bbb" "ccc")))   ; => ("aaa" "bbb" "ccc")
;;;             (tulip2comments '((comments "aaa")))   ; => "aaa"

(defun tulip2attributes (tulip-directives)
  "Assuming TULIP-DIRECTIVES is as specified in TLP macro,
    returns the list of Tulip attributes found in the, supposedly unique,
    ATTRIBUTES directive, of the form (ATTRIBUTES . tulip2attributes)."
  (cdr (car (member-if (name-equalp-fun "attributes") tulip-directives))))
;;; (tulip2attributes '((edge 1 4 5) (attributes attr1 attr2 attr3)))

(defun tulip2controller (tulip-directives)
  "Assuming TULIP-DIRECTIVES is as specified in TLP macro,
    returns a CONTROLLER instance whose TULIP slot holds
    the list of controller directives found in
       (CONTROLLER . controller-directives) directive, if any,
     NIL otherwise."
  (new 'controller
       :tulip (cdr (car (member-if (name-equalp-fun "controller") tulip-directives)))))

(defun tulip2author (tulip-directives)
  "Assuming TULIP-DIRECTIVES is as specified in TLP macro,
    returns the string found in the (AUTHOR \"author-string\") directive, if any,
     NIL otherwise."
  (second (car (member-if (name-equalp-fun "author") tulip-directives))))

(defun tulip2date (tulip-directives)
  "Assuming TULIP-DIRECTIVES is as specified in TLP macro,
    returns the string found in the (DATE \"date-string\") directive, if any,
     NIL otherwise."
  (let ((date-directive? (car (member-if (name-equalp-fun "date") tulip-directives))))
    (if date-directive?
	(second date-directive?)
	"mm-dd-yyyy")))

;;; **** TEST ONLY! (tulip2date *tulip-directives*)
;;; (tulip2date nil)

(defun tulip-pgraph-defparameter-name (tulip-directives)
  "Assuming TULIP-DIRECTIVES is as specified in TLP macro,
    returns the symbol which is the value of *tulip-pgraph-defparameter-name*
     if it is not NIL, or a symbol of the form *<DATE>-<AUTHOR>-<NNN>* where
     <DATE> comes from the Tulip date directive, likewise for <AUTHOR>,
     and <NNN> is a counter generated by GENSYM."
  (or *tulip-pgraph-defparameter-name*
      (let ((date (or (tulip2date tulip-directives) "DATE?"))
	    (author (or (tulip2author tulip-directives) "AUTHOR?")))
	(intern (format nil "~a*" 
			(substitute #\- #\SPACE
				    (string-upcase (gensym (format nil "*~A-~A-" date author)))))))))

;;; (tulip-pgraph-defparameter-name '((date "02-26-2009") (author "John Doe")))
;;; **** TEST ONLY - DO NOT EVALUATE! (tulip-pgraph-defparameter-name *tulip-directives*)

(defun tulip-edge-directives (tulip-directives)
  "Assuming TULIP-DIRECTIVES is as in TLP macro,
    returns the list of Tulip edge directives found in TULIP-DIRECTIVES,
     in the same order."
  (all (name-equalp-fun "edge") tulip-directives))

;;; (tulip-edge-directives '((nb_edges 8) (edge 0 4 3) (edge 1 3 5) (edge 2 5 4) (edge 3 3 1)))
;;; **** TEST ONLY! (tulip-edge-directives *tulip-directives*)

(defun tulip-edge-directive-id (tulip-edge-directive)
  "Asuming TULIP-EDGE-DIRECTIVE is a Tulip edge directive as
    specified by TLP format (see TLP macro for Internet reference),
    returns the source part of TULIP-EDGE-DIRECTIVE.
    Note: TULIP-EDGE-DIRECTIVE has the form (EDGE id source target)"
  (second tulip-edge-directive))

(defun tulip-edge-directive-source (tulip-edge-directive)
  "Asuming TULIP-EDGE-DIRECTIVE is a Tulip edge directive as
    specified by TLP format (see TLP macro for Internet reference),
    returns the source part of TULIP-EDGE-DIRECTIVE.
    Note: TULIP-EDGE-DIRECTIVE has the form (EDGE id source target)"
  (third tulip-edge-directive))

(defun tulip-edge-directive-target (tulip-edge-directive)
  "Asuming TULIP-EDGE-DIRECTIVE as in TULIP-EDGE-DIRECTIVE-SOURCE function,
    returns the target part of TULIP-EDGE-DIRECTIVE."
  (fourth tulip-edge-directive))

(defun tulip2edges (tulip-directives &optional (tulip2vertices (tulip2vertices tulip-directives)))
  "Assuming TULIP-DIRECTIVES as specified in TLP macro,
 and TULIP2VERTICES as hinted by default value of this optional parameter,
returns the list of PEDGE instances corresponding to edge directives
 found in TULIP-DIRECTIVES, where
    edge-directive ::=   (EDGE id source target)
 More precisely, for each such edge-directive, the corresponding PEDGE comes with the followibg slot values:
 - ID: id,
 - SOURCE: the vertex in TULIP2VERTICES whose ID is source,
 - TARGET: the vertex in TULIP2VERTICES whose ID is target,
 - ALIST: set to a property / value association list collecting
           all data from relevant property directives associated with this PEDGE,
           see TULIP2ALIST for details."
  (mapcar (lambda (tulip-edge-directive)
	    (let ((edge-id (tulip-edge-directive-id tulip-edge-directive)))
	      (new 'pedge
		   :id edge-id
		   :source (car (member (tulip-edge-directive-source tulip-edge-directive) tulip2vertices
					:key #'id :test #'equals))
		   :target (car (member (tulip-edge-directive-target tulip-edge-directive) tulip2vertices
				    :key #'id :test #'equals))
		   :alist (tulip2alist '|edge| edge-id #'property-directive-edge-default tulip-directives))))
	  (tulip-edge-directives tulip-directives)))

;;; **** TEST ONLY! (load "example1.tlp") (tulip2edges *tulip-directives*)

(defun tulip-best-controller ()
  "Returns he best Tulip controller for adequate visualization of graphs,
so that is can be used as the cdr of the Tulip controller directive, that is:
     (controller . tulip-best-controller)    ."
  '((|DataSet| "MainController" 
     (|DataSet| "views" 
      (|DataSet| "view0" 
	       (|DataSet| "Node Link Diagram view" 
			(|DataSet| "data" 
				 (|DataSet| "Display" 
					  (|bool| "antialiased" |true|)
					  (|bool| "arrow" |false|)
					  (|bool| "displayNodes" |true|)
					  (|bool| "displayEdges" |true|)
					  (|bool| "displayMetaNodes" |true|)
					  (|bool| "nodeLabel" |true|)
					  (|bool| "edgeLabel" |false|)
					  (|bool| "metaLabel" |false|)
					  (|bool| "outScreenLabel" |false|)
					  (|bool| "elementOrdered" |false|)
					  (|bool| "elementZOrdered" |false|)
					  (|bool| "incrementalRendering" |true|)
					  (|bool| "edgeColorInterpolation" |false|)
					  (|bool| "edgeSizeInterpolation" |false|)
					  (|bool| "edge3D" |false|)
					  (|bool| "labelScaled" |false|)
					  (|bool| "labelOverlaped" |false|)
					  (|int| "labelMinSize" 10)
					  (|int| "labelMaxSize" 30)
					  (|int| "selectedNodesStencil" 2)
					  (|int| "selectedMetaNodesStencil" 2)
					  (|int| "selectedEdgesStencil" 2)
					  (|int| "nodesStencil" 65535)
					  (|int| "metaNodesStencil" 65535)
					  (|int| "edgesStencil" 65535)
					  (|int| "nodesLabelStencil" 65535)
					  (|int| "metaNodesLabelStencil" 65535)
					  (|int| "edgesLabelStencil" 65535)
					  (|bool| "edgesMaxSizeToNodesSize" |true|)
					  (|color| "selectionColor" "(255,0,255,255)"))
				 (|string| "scene"
					   "<?xml version=\"1.0\"?>
<scene>
  <data>
    <viewport>(0,0,492,472)</viewport>
    <background>(255,255,255,255)</background>
  </data>
  <children>
    <GlLayer name=\"Background\">
      <data>
        <camera>
          <data>
            <center>(-2.31357,0,0)</center>
            <eyes>(-2.31357,0,11.2571)</eyes>
            <up>(0,1,0)</up>
            <zoomFactor>1</zoomFactor>
            <sceneRadius>11.2571</sceneRadius>
            <d3>0</d3>
            <sceneBoundingBox0>(-8.18064,-4.40806,-0.5)</sceneBoundingBox0>
            <sceneBoundingBox1>(3.5535,4.40806,0.5)</sceneBoundingBox1>
          </data>
        </camera>
        <visible>0</visible>
      </data>
      <children type=\"GlComposite\">
        <data/>
        <children>
          <GlEntity name=\"background\" type=\"Gl2DRect\">
            <data>
              <visible>1</visible>
              <stencil>65535</stencil>
              <top>0</top>
              <bottom>1</bottom>
              <left>0</left>
              <right>1</right>
              <inPercent>1</inPercent>
              <textureName>TulipBitmapDir/tex_back.png</textureName>
              <xInv>0</xInv>
              <yInv>0</yInv>
            </data>
          </GlEntity>
        </children>
      </children>
    </GlLayer>
    <GlLayer name=\"Main\">
      <data>
        <camera>
          <data>
            <center>(-2.31357,0,0)</center>
            <eyes>(-2.31357,0,11.2571)</eyes>
            <up>(0,1,0)</up>
            <zoomFactor>1</zoomFactor>
            <sceneRadius>11.2571</sceneRadius>
            <d3>1</d3>
            <sceneBoundingBox0>(-8.18064,-4.40806,-0.5)</sceneBoundingBox0>
            <sceneBoundingBox1>(3.5535,4.40806,11.2571)</sceneBoundingBox1>
          </data>
        </camera>
        <visible>1</visible>
      </data>
      <children type=\"GlComposite\">
        <data/>
        <children>
          <GlEntity name=\"Hulls\" type=\"GlComposite\">
            <data>
              <visible>0</visible>
              <stencil>65535</stencil>
            </data>
            <children/>
          </GlEntity>
          <GlEntity name=\"graph\" type=\"GlGraphComposite\">
            <data>
              <visible>1</visible>
              <stencil>65535</stencil>
            </data>
          </GlEntity>
        </children>
      </children>
    </GlLayer>
    <GlLayer name=\"Foreground\">
      <data>
        <camera>
          <data>
            <center>(-2.31357,0,0)</center>
            <eyes>(-2.31357,0,11.2571)</eyes>
            <up>(0,1,0)</up>
            <zoomFactor>1</zoomFactor>
            <sceneRadius>11.2571</sceneRadius>
            <d3>0</d3>
            <sceneBoundingBox0>(-8.18064,-4.40806,-0.5)</sceneBoundingBox0>
            <sceneBoundingBox1>(3.5535,4.40806,11.2571)</sceneBoundingBox1>
          </data>
        </camera>
        <visible>1</visible>
      </data>
      <children type=\"GlComposite\">
        <data/>
        <children>
          <GlEntity name=\"labrilogo\" type=\"Gl2DRect\">
            <data>
              <visible>0</visible>
              <stencil>65535</stencil>
              <top>5</top>
              <bottom>55</bottom>
              <left>5</left>
              <right>55</right>
              <inPercent>0</inPercent>
              <textureName>TulipBitmapDir/logolabri.jpg</textureName>
              <xInv>1</xInv>
              <yInv>0</yInv>
            </data>
          </GlEntity>
        </children>
      </children>
    </GlLayer>
  </children>
</scene>
")))
	       (|uint| "id" 0)
	       (|int| "x" 0)
	       (|int| "y" 0)
	       (|int| "width" 500)
	       (|int| "height" 500)
	       (|bool| "maximized" |false|))))))

(format t "~&Tulip / LISP interface version 2.0 loaded.~&")

(provide "TULIP")
