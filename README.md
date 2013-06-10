Orleans - R Clustering scripts
==============================

These scripts are meant to apply several standard clustering algorithms
to some tabular data, and then compare the resulting clusters, in order
to check for agreement between the clustering algorithms. 

The main scripts are:

	- main-install.R: 	Installs the libraries needed for executing 
						the other scripts. Just run this one once. 
					
	- main-generation.R:	Generates dummy data, to be used to test 
							the other scripts, in absence of actual data.
							Some parameters can be tuned cf. the 'TODO' 
							marks inside the script.
							It generates certain files:
			- data.txt:	Contains the raw data. Actual data must also respect 
						the same tabular format.
			- data.pdf: Graphical representation of the data. A PCA is first 
						performed to extract only the 2 main components. Those
						are then used to represent the data in a 2D space.
			- pca.txt:	Results from the PCA, cached to be used later (i.e.
						each time one wants to plot the clusters).
									
	- main-process.R:	Applies the clustering algorithms, then compare them.
						Certain parameters can also be tuned, cf. the 'TODO'
						marks inside the script, too.
						Some results are cached in specific files:
			- distances.bin:	R object representing the distance matrix.
								(binary file).
			- ALGO.txt: 		Membership vector, generated by the tool <ALGO>. 
								Each value correspond to the number of the cluster
								for the corresponding instance.
			- performances.txt:	Table containing the maximal average Silhouette width 
								obtained for each algorithm applied for clustering,
								as well as the number of detected clusters.
			- agreement.txt:	Matrix comparing partitions algorithm by algorithm. 
								The adjusted Rand index (ARI) is used to measure
								partition similarity.

The rest of the files contain functions used by the above scripts.