Orleans - R Clustering scripts
==============================


Les scripts principaux sont :

	- main-install.R: 	installe les librairies R nécessaire à l'exécution 
						des scripts (à exécuter une seule fois). 
					
	- main-generation.R:	écrit pour générer des données bidons, afin de tester 
							les autres scripts.
			- data.txt:	fichier généré, contenant les données brutes. les données 
						réelles doivent respecter ce format tabulaire.
			- data.pdf: représentation graphique des données. une ACP est réalisée 
						pour extraire seulement les deux vecteurs principaux.
			- pca.txt:	résultats de l'ACP, cachés pour être réutilisés plus tard 
						si nécessaire. 
									
	- main-process.R:	applique les algorithmes de clustering, puis les compare.
						certains résultats sont cachés dans des fichiers:
			- distances.bin:	objet R représentant la matrice de distances.
			- ALGO.txt: 		vecteur d'appartenance généré par l'algorithme de 
								clustering <ALGO>. Chaque valeur correspond au 
								numéro de cluster d'une instance.
			- performances.txt:	table contenant la valeur de Silhouete maximale 
								obtenue pour chaque algo de clustering, ainsi que 
								le nombre de clusters détectés.
			- ari.txt:			matrice comparant les clusters de chaque algo 2 à 
								2, en utilisant l'index de Rand ajusté (ARI).

Le reste des fichiers contient des fonctions utilisées par ces scripts.
