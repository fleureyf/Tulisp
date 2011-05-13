;;; Projet Common-Lisp (Enseirb-Matmeca, informatique, première année, second semestre).
;;; Equipe 461 
;;; Fabien Fleurey
;;; Créé le 05 mai 2011
;;;
;;; Résumé : Fournis des exemples d'utilisation des fonctions.
;;; 

(use-package :common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "MGRAPH" "mgraph")
  (require "PARCOURS" "parcours_graphes")
  (require "PERTES" "calcul_pertes")
  (require "CRUCIAL" "detection_crucial"))

;;;
;;; Chargement d'un fichier au format tulip "example2.tlp" dans le nouveau pgraph *pg* ;;; 
;;;

;(defpgraph *pg* "exemple/example1.tlp")

;;;
;;; Definition d'un nouveau mgraph *mg* contenant : 
;;;    - Toutes les informations contenues dans le pgraph *pg*.
;;;    - Une nouvelle propriété "blockedData" initialisé à sa valeur par défaut pour tous les sommets et toutes les arrêtes.
;;;    - La matrice, sous forme de liste de liste, représentant les echange de données lue à partir d'un fichier au format data "example2.data". 
;;;

;(defmgraph *mg* *pg* "exemple/example1.txt")

;;;
;;; Défini le mgraph *colored-crucial-vertices-mg* identique à *mg* avec les sommets cruciaux colorié en rouge. 
;;;

;(defparameter *colored-crucial-vertices-mg* (colorize-crucial-vertices *mg* '(RED)))

;;;
;;; Defini le mgraph *blocked-data-computed-mg* identique à *mg* ou a été calculé pour tous les sommets la valeur de la quantité de donnée bloqué si celui ci tombait en panne. 
;;;

;(defparameter *blocked-data-computed-mg* (blocked-data-to-vertices *mg* (crucial-vertices *colored-crucial-vertices-mg*)))

;;;
;;; Défini un nouveau pgraph *new-pg* identique au mgraph *mg* mais qui ne contient pas la matrice des octets echangés.
;;;

;(defparameter *new-pg* (mgraph2pgraph *blocked-data-computed-mg*))

;;;
;;; Crée un nouveau fichier au format tulip "new_exemple2.tlp" représentant le pgraph *new-pg*.
;;;

;(2tlp-file *new-pg* "new_exemple1")