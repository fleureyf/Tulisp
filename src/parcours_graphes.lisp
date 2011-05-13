;;; Projet Common-Lisp (Enseirb-Matmeca, informatique, première année, second semestre).
;;; Equipe 461 
;;; Fabien Fleurey
;;; Créé le 05 mai 2011
;;;
;;; Résumé : Permet le parcours en profondeur d'un graphe.
;;; 

(use-package :common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "MGRAPH" "mgraph"))

(defun successors (mgraph vertex)
  "Retourne la liste des sommets successeurs de vertex dans le graphe mgraph"
  (labels ((successors-rec (mgraph edges vertex)
	     (if (null edges)
		 ()
		 (cond ((equals (source (car edges)) vertex)
			(cons (search-id-vertex mgraph (id (target (car edges)))) (successors-rec mgraph (cdr edges) vertex)))
		       ((equals (target (car edges)) vertex)
			(cons (search-id-vertex mgraph (id (source (car edges)))) (successors-rec mgraph (cdr edges) vertex)))
		       (t (successors-rec mgraph (cdr edges) vertex))))))
    (successors-rec mgraph (edges mgraph) vertex)))

(defun visit-vertex (mgraph vertex)
  "Retourne le graphe mgraph avec la colorisation d'un parcours en profondeur."
  (labels ((visit-vertex-rec (mgraph successors)
	     (if (null successors)
		 mgraph
		 (if (equal-color (car successors) '(WHITE))
		     (let* ((mgraph (colored-vertex mgraph (car successors) '(BLACK)))
			    (successors (append (cdr successors)
						(remove nil
							(mapcar #'(lambda (vert) (if (equal-color vert '(WHITE)) vert))
								(successors mgraph (car successors)))))))
		       (visit-vertex-rec mgraph successors))
		     (visit-vertex-rec mgraph (cdr successors))))))
    (let ((mgraph (colored-vertex mgraph vertex '(BLACK))))
      (visit-vertex-rec mgraph (successors mgraph vertex)))))

(provide "PARCOURS_GRAPHES")