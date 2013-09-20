Detection of Social Capitalists
==============================

This project contains some source code used for the paper "RÃ´e communautaire des 
capitalistes sociaux dans Twitter". It contains both C++ and R programs and scripts.

The C++ code was used to:

	- process social capitalism indices
	- perform community detection thanks to a custom implementation of the Louvain
	  method, allowing to handle directed networks.
	- process several measures meant to characterize network nodes in terms of
	  community role.

The R code was used to:

	- Test and generate plots regarding the distribution of degree, social 
	  capitalism indices, communities and role measures
	- Normalize the role measure and partition them using k-means
	- Process statistics and generate plots to characterize the resulting clusters.
	
==============================
How to use the C++ code

	-Use make to compile
	-To be able to use parallelism, OpenMP is required
	To indicate the number of processors to use :
	export OMP_NUM_THREADS=nb_proc

	Convert the graph graphes/easygraphe in a binary format renumbering the ids and put the result in convert :
	./bin/convert -i graphes/easygraphe -o convert/easygraphe -r
	Process the overlap and ratio indiced and put the result in results :
	./bin/overlap convert/easygraphe results/overlap_easy
	Process our measures, improvements to the ones introduced by Guimera and Amaral using the community detection result obtained using Louvain algorithm :
	./bin/guimera convert/easygraphe results/result_louvain results/result_guimera	
		
==============================
How to use the R code
	
The scripts are numbered to indicate their order of execution. Each one corresponds
to a specific process:

	- install: installs the required R packages
	- generation: allows generating dummy data (for testing purposes)
	- degree: processes stats regarding degree distribution, generates plots
	- soccap: same thing with social capitalism indices
	- communities: same thing for (Louvain) community sizes
	- measures:	same thing with role measures
	- normalize: prepares the role measure for clustering
	- clustering: applies a parallel version of k-means and evaluate the obtained clusters
	- plot: renders the resulting clusters
	- anova: performs anovas on the role measures, using clusters as factors
	- comparison: compares the clusters obtained with the new measures, w
	- hive: generates hive plots

Cf. each scripts for more details.

The src folder contain the R scripts. The data folder is supposed to contain the 
files required by the scripts. Some scripts require some files produced by other 
scripts, so it is important to execute them in the specified order.

Note: the src/v1 archive contains the first version of the R scripts. Those were
not able to handle the size of our data, so we finally did not use them, but kept 
them as archives. The original.data.tar.gz archive contains the clean version of 
the networks used in the original Guimerà & Amaral paper, retrieved from
http://www.tuhh.de/ibb/publications/databases-and-software.html
