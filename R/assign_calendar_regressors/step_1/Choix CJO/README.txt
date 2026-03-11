-----------------------------------------
|	Choix des régresseurs CJO	|
-----------------------------------------

Objectif : 
----------
On cherche à comparer les différents jeux de régresseurs pour chaque séries du jeu de donnée (export et import). Pour cela, on va comparer les effets des jeux de régressseurs cjo envisagés (effets des jours ouvrables).
On étudit alors les effets jours ouvrables et la qualité de modélisation

Plan : 
------
- Créer des WS de référence temporaire pour l'étude des effets CJO
- Créer une spécification (SPEC) par jeux de régresseurs et paramètrer selon nos données (période d'estimation, ...)
- Créer un SAP par spécification
- Faire une modélisation complète de toutes les séries avec tous les jeux de régresseurs (donc toutes les spécifications)

Programmes R :
--------------
- 01_BQ_REGS_CJO.R : 
	* cruncher les WS
	* créer la matrice de diagnostique et les indicateurs de qualité de chaque jeu
	* Extraction du bilan qualité
	* extraction des coefficients et p_values

- 02_CHOIX_JEU_CJO.R :
	* Faire un tableau de synthèse
	* Regrouper les résultats précédent, trier et récupérer selon les critères de l'aicc et de la note (sur les résidus) le meilleur jeu de régresseur par série
	* Exporter les résultats et enregistrer en csv et .Rdata
 