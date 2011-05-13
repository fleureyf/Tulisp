;;; Projet Common-Lisp (Enseirb-Matmeca, informatique, première année, second semestre).
;;; Equipe 461 
;;; Fabien Fleurey
;;; Créé le 05 mai 2011
;;;
;;; Résumé : Fournis une représentation d'un graphe et quelques outils pour les manipuler.
;;; 

(in-package :common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SBCL" "tulip_bin/sbcl")
  (with-application-directory (:RELATIVE "tulip_src")
    (sbcl-require "TULIP" "tulip")))

;;;
;;; Definition de la classe mgraph.
;;;

(defcl mgraph (pgraph) "Représente un graphe muni de la matrice de donnée." data)

(defmacro defmgraph (mgraph-name pgraph data-file)
  "Permet de definir le mgraph de nom mgraph-name à partie du pgraph pgraph et du fichier de donnée data-file"
  `(defparameter ,mgraph-name (pgraph2mgraph ,pgraph ,data-file)))

(defmethod print-object ((mgraph mgraph) stream)
  "Defini la méthode d'affichage d'un mgraph"
  (princ "Vertices : "  stream)
  (princ (vertices mgraph) stream)
  (fresh-line stream)
  (princ "Edge : " stream)
  (princ (edges mgraph) stream)
  (fresh-line stream)
  (princ "Data : "  stream)
  (princ (data mgraph) stream))

;;;
;;; Quelques méthodes permetant de manipuler les pvertex.
;;;

(defmethod equals ((vertex1 pvertex) (vertex2 pvertex))
  "Retourne t si les deux pervertex vertex1 et vertex2 ont le même id et nil sinon"
  (eq (id vertex1) (id vertex2)))

(defmethod blocked-data ((vertex pvertex))
  "Retourne la valeur de la propriété blockedData du pvertex vertex."
  (labels ((search-blocked-data (a-list)
	     (if (eq (caar a-list) :|blockedData|)
		 (cdar a-list)
		 (search-color (cdr a-list)))))
    (search-blocked-data (alist vertex))))

(defmethod color ((vertex pvertex))
  "Retourne la valeur de la propriété viewColor du pvertex vertex."
  (labels ((search-color (a-list)
	     (if (eq (caar a-list) :|viewColor|)
		 (cdar a-list)
		 (search-color (cdr a-list)))))
    (search-color (alist vertex))))

(defmethod equal-color ((vertex pvertex) color)
  "Retourne t si le pvertex vertex est de couleur color et nil sinon."
  (equal (color vertex) (macroexpand color)))

;;;
;;; Fonctions permettant les équivalences entre pgraph et mgraph 
;;;

(defun pgraph2mgraph (pgraph data-file)
  "Retourne un mgraph à partir du pgraph pgraph et du fichier de donnée data-file." 
  (new 'mgraph 
       :vertices (mapcar #'add-blocked-data-to-pvertex (vertices pgraph))
       :edges (mapcar #'add-blocked-data-to-pedge (edges pgraph))
       :props (cons (new 'property :num 0 :typ '|int| :name :|blockedData| :node-default 0 :edge-default 0) (props pgraph)) 
       :version (version pgraph)
       :date (date pgraph)
       :author (author pgraph)
       :comments (comments pgraph)
       :attributes (attributes pgraph)
       :controller (controller pgraph)
       :data (get-data data-file)))

(defun mgraph2pgraph (mgraph)
  "Retourne un pgraph à partir d'un mgraph mgraph."
  (new 'pgraph
       :vertices (vertices mgraph)
       :edges (edges mgraph)
       :props (props mgraph)
       :version (version mgraph)
       :date (date mgraph)
       :author (author mgraph)
       :comments (comments mgraph)
       :attributes (attributes mgraph)
       :controller (controller mgraph)))

(defun add-blocked-data-to-pvertex (pvertex)
  "Retourne le pvertex identique doté de la propriété blockedData initialisée à sa valeur par défault."
  (new 'pvertex 
       :id (id pvertex) 
       :alist (cons (cons :|blockedData| 0) (alist pvertex))))

(defun add-blocked-data-to-pedge (pedge)
  "Retourne la pedge identique dotée de la propriété blockedData initialisée à sa valeur par défault."
  (new 'pedge 
       :source (source pedge) 
       :target (target pedge)
       :val (val pedge)
       :id (id pedge) 
       :alist (cons (cons :|blockedData| 0) (alist pedge))))

(defun get-data (data-file)
  "Lit la matrice de data et la retourne sous forme de liste de liste."
  (labels ((split-line (list-result line)
	     (setq buf (read line nil))
	     (if (null buf)
		 ()
		 (append list-result (list buf) (split-line list-result line)))) 
	   (get-data-rec (result data-stream)
	     (setq buf-line (read-line data-stream nil))
	     (if (null buf-line)
		 ()
		 (append result 
			 (with-input-from-string (line buf-line)
			   (list (split-line () line))) 
			 (get-data-rec result data-stream)))))
    (with-open-file (data-stream data-file)
      (get-data-rec () data-stream))))

;;;
;;; Fonctions outils pour les mgraph.
;;;

(defun search-id-vertex (mgraph id)
  "Retourne le pvertex portant l'id id dans le graphe mgraph."
  (find-if #'(lambda (vert) (eq (id vert) id)) (vertices mgraph)))

(defun colored-vertex (mgraph vertex color)
  "Retourne le graphe mgraph dans lequel le sommet vertex a la couleur color."
  (let* ((new-alist (cons (cons :|viewColor| (macroexpand color)) 
			  (remove-if #'(lambda (x) (equal (car x) :|viewColor|)) (alist vertex))))
	 (new-pvert (new 'pvertex :id (id vertex) :alist new-alist))
	 (new-vertices (cons new-pvert (remove-if #'(lambda (vert) (equals vert vertex)) (vertices mgraph)))))
    (new 'mgraph 
	 :vertices new-vertices
	 :edges (edges mgraph)
	 :props (props mgraph)
	 :version (version mgraph)
	 :date (date mgraph)
	 :author (author mgraph)
	 :comments (comments mgraph)
	 :attributes (attributes mgraph)
	 :controller (controller mgraph)
	 :data (data mgraph))))

(defun colored-mgraph (mgraph color)
  "Retourne le mgraph identique à mgraph où tous les sommets sont de couleur color."
  (labels ((colored-mgraph-rec (mgraph vertices color)
	     (if (null vertices)
		 mgraph
		 (colored-mgraph-rec (colored-vertex mgraph (car vertices) (macroexpand color)) (cdr vertices) color)))) 
    (colored-mgraph-rec mgraph (vertices mgraph) color)))

;;;
;;; Macros permettant de simplifier la manipulation des couleurs.
;;;

(defmacro BLUE ()
  "Defini la couleur bleu au format Tulip."
  `(85 170 255 255))

(defmacro RED ()
  "Defini la couleur rouge au format Tulip."
  `(255 0 0 255))

(defmacro WHITE ()
  "Defini la couleur blanche au format Tulip."
  `(255 255 255 255))

(defmacro GREY ()
  "Defini la couleur grise au format Tulip."
  `(100 100 100 255))

(defmacro BLACK ()
  "Defini la couleur noir au format Tulip."
  `(0 0 0 255))

(provide "MGRAPH")
