;;; Projet Common-Lisp (Enseirb-Matmeca, informatique, première année, second semestre).
;;; Equipe 461 
;;; Fabien Fleurey
;;; Créé le 05 mai 2011
;;;
;;; Résumé : Fournis les fonctions permettant d'identifier les sommets représentant des routeurs cruciaux.
;;; 

(use-package :common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "MGRAPH" "mgraph")
  (require "PARCOURS_GRAPHES" "parcours_graphes"))

;;; (load "detection_crucial")
;;; Fonctions annexes.
;;;

(defun p-color-vertex-in-mgraph (mgraph color)
  "Retourne vrai s'il existe un sommet de couleur color dans le graphe mgraph et faux sinon."
  (labels ((p-color-vertex-in-mgraph-rec (vertices) 
	     (cond ((null vertices) nil)
		   ((equal-color (car vertices) color) t)
		   (t (p-color-vertex-in-mgraph-rec (cdr vertices))))))
    (p-color-vertex-in-mgraph-rec (vertices mgraph))))

;;;
;;; Fonction de detection des sommets qui sont point d'articulation.
;;;

(defun p-crucial-vertex (mgraph vertex)
  "Retourne vrai si le sommet vertex est un point d'articulation du graphe mgraph et faux sinon."
  (if (null (cdr (vertices mgraph)))
      t
      (let* ((mgraph (colored-vertex (colored-mgraph mgraph '(WHITE)) vertex '(BLACK))) 
	     (mgraph (if (equals (first (vertices mgraph)) vertex)
			 (visit-vertex mgraph (second (vertices mgraph)))
			 (visit-vertex mgraph (first (vertices mgraph))))))
	(p-color-vertex-in-mgraph mgraph '(WHITE)))))

(defun crucial-vertices (mgraph)
  "Retourne la liste des sommets qui sont des points d'articulations du graphe mgraph."
  (labels ((crucial-vertices-rec (mgraph vertices)
	     (if (null vertices)
		 ()
		 (if (p-crucial-vertex mgraph (car vertices))
		     (cons (car vertices) (crucial-vertices-rec mgraph (cdr vertices)))
		     (crucial-vertices-rec mgraph (cdr vertices))))))
    (crucial-vertices-rec mgraph (vertices mgraph))))

;;;
;;; Traitement des sommets point d'articulation.
;;;

(defun colorize-crucial-vertices (mgraph color)
  "Retourne le graphe mgraphe avec les sommets qui sont point d'articulation de la couleur color." 
  (labels ((colorize-crucial-vertices-rec (mgraph crucials)
	     (if (null crucials)
		 mgraph
		 (colorize-crucial-vertices-rec (colored-vertex  mgraph (car crucials) color) (cdr crucials)))))
    (colorize-crucial-vertices-rec mgraph (crucial-vertices mgraph))))

(provide "DETECTION_CRUCIAL")