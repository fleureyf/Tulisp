;;; Projet Common-Lisp (Enseirb-Matmeca, informatique, première année, second semestre).
;;; Equipe 461 
;;; Fabien Fleurey
;;; Créé le 05 mai 2011
;;;
;;; Résumé : Fournis les fonctions permettant de calculer les données bloquées suite à la panne d'un routeur crucial.
;;; 

(use-package :common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "MGRAPH" "mgraph")
  (require "PARCOURS_GRAPHES" "parcours_graphes"))

;;;
;;; Fonctions annexes.
;;;

(defun color-to-color-vertices (mgraph former-color new-color)
  "Retourne le graphe dont tous les sommets qui etait de couleur former-color dans mgraph sont de couleur new-color."
  (labels ((color-to-color-vertices-rec (mgraph vertices)
	     (cond  ((null vertices)
		     mgraph)
		    ((equal-color (car vertices) former-color)
		     (color-to-color-vertices-rec (colored-vertex mgraph (car vertices) new-color) (cdr vertices)))
		    (t (color-to-color-vertices-rec mgraph (cdr vertices))))))
    (color-to-color-vertices-rec mgraph (vertices mgraph))))

(defun color-vertices (mgraph color)
  "Retourne la liste des sommets de couleur color dans le graphe mgraph"
  (labels ((color-vertices-rec (vertices)
	     (if (null vertices)
		 ()
		 (if (equal-color (car vertices) (macroexpand color))
		     (cons (car vertices) (color-vertices-rec (cdr vertices)))
		     (color-vertices-rec (cdr vertices))))))
    (color-vertices-rec (vertices mgraph))))

(defun connected-vertex-components (mgraph vertex)
  "Retourne les composantes connexes du graphe obtenu en enlevant le sommet vertex du graphe mgraph."
  (labels ((connected-vertex-components-rec (mgraph successors)
	     (if (null successors)
		 ()
		 (let ((mgraph (color-to-color-vertices mgraph '(BLACK) '(GREY))))
		   (if (equal-color (search-id-vertex mgraph (id (car successors))) '(WHITE))
		       (let ((mgraph (visit-vertex mgraph (car successors)))) 
			 (cons (color-vertices mgraph '(BLACK)) (connected-vertex-components-rec mgraph (cdr successors))))
		       (connected-vertex-components-rec mgraph (cdr successors)))))))
    (let ((mgraph (colored-vertex (colored-mgraph mgraph '(WHITE)) vertex '(BLACK))))
      (connected-vertex-components-rec mgraph (successors mgraph vertex)))))

;;;
;;; Fonction de calcul des pertes à différentes echelles.
;;;

(defun lost-data-vertex (mgraph vertex component recipients)
  "Retourne la quantité de données que vertex doit envoyer en dehors de la composante component"
  (if (null recipients)
      0
      (if (member (id (car recipients)) (mapcar #'id component))
	  (lost-data-vertex mgraph vertex component (cdr recipients))
	  (+ (nth (id (car recipients)) (nth (id vertex) (data mgraph))) 
	     (lost-data-vertex mgraph vertex component (cdr recipients))))))

(defun lost-data-component (mgraph vertex component)
  "Retourne la somme des données bloquées pour tous les sommets de component"
  (labels ((lost-data-component-rec (mgraph vertex vertices component)
	     (if (null vertices)
		 0
		 (+ (lost-data-vertex mgraph (car vertices) component (remove-if #'(lambda (vert) (equals vertex vert)) (vertices mgraph))) 
		    (lost-data-component-rec mgraph vertex (cdr vertices) component)))))
    (lost-data-component-rec mgraph vertex component component)))
  
(defun lost-data-components (mgraph vertex components)
  "Retourne la somme des données bloquées pour toutes les composantes components"
  (if (null components)
      0
      (+ (lost-data-component mgraph vertex (car components)) 
	 (lost-data-components mgraph vertex (cdr components)))))

;;;
;;; Fonction de calcul des données potentiellement bloquées et de mise à jour de la propriété blockedData.
;;;

(defun blocked-data-vertex (mgraph vertex)
  "Retourne la quantité de donnée bloquée si le sommet mvertex est retiré de mgraph"
  (lost-data-components mgraph vertex (connected-vertex-components mgraph vertex)))

(defun blocked-data-to-vertex (vertex blocked-data)
  "Retourne le pvertex vertex avec la propriété blockedData égale a blocked-data."
  (new 'pvertex 
       :id (id vertex) 
       :alist (cons (cons :|blockedData| blocked-data) (alist vertex))))

(defun blocked-data-to-vertices (mgraph vertices)
  "Retourne le graphe mgraph avec les valeurs blockedData calculé pour tous les sommets de vertices."
  (labels ((blocked-data-vertices (mgraph vertices)
	     (if (null vertices)
		 ()
		 (cons (blocked-data-to-vertex (car vertices) (blocked-data-vertex mgraph (car vertices)))
		       (blocked-data-vertices mgraph (cdr vertices))))))
    (new 'mgraph 
	 :vertices (let ((new-vertices (blocked-data-vertices mgraph vertices))) 
		     (append new-vertices (remove-if #'(lambda (vert) (member (id vert) (mapcar #'id new-vertices))) (vertices mgraph))))
	 :edges (edges mgraph)
	 :props (props mgraph) 
	 :version (version mgraph)
	 :date (date mgraph)
	 :author (author mgraph)
	 :comments (comments mgraph)
	 :attributes (attributes mgraph)
	 :controller (controller mgraph)
	 :data (data mgraph))))

(provide "CALCUL_PERTES")