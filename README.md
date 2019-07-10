SocCap
==============================
*Detection of Social Capitalists*

* Copyright 2013-14 Nicolas Dugué, Vincent Labatut & Anthony Pérez

SocCap is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`
 
-----------------------------------------------------------------------

## Presentation
This software aims at studying social capitalists, which are a specific type of users of social networks services such as Twitter. The tool is generic, so it can actually be applied to completely different systems, as long as they can be represented as directed networks (i.e. digraphs). We applied our tool to Twitter in several research papers [DLP'13, DLP'14a, DLP'14b, DLP'15], detailed at the end of this document. Our work was also mentioned on the blog of the [MIT Technology review](http://www.technologyreview.com/view/528746/the-emerging-threat-from-twitters-social-capitalists/).

Please, cite [DLP'14a] if you use our tool. We would also be very interested to know your context of application and/or modification, so please, let us know.

To be more precise, the tool allows to perform the following tasks, on a network represented as an edgelist (the exact format is described later):

1. Process the social capitalism indices described in [DP'13], which allows identifying social capitalists, as well as the type of strategy they use to improve their visibility on the network. 
2. Detect the community structure of the considered network, through various existing algorithms.
3. Process various kinds of community role measures, aiming at describing the position of each node in the network from a community perspective (i.e. meso-scale).
4. Perform a cluster analysis of these community role values, in order to identify distinct community roles extending the notion introduced by Guimerà & Amaral in [GA'05].
5. Generate a bunch of stats and plots related to all these calculations, including [hive plots](http://www.hiveplot.net/).

Note the software may evolve depending on our future research work.

## Organization
The project mainly contains 3 types of source code:
* Custom C++ code used to perform the heavy lifting when treating huge networks such as the Twitter one.
* Third-party programs in various languages to also perform computationally intensive processing such as cluster analysis.
* Custom R code to perform stat- and plot-related processing, as well as glue all these pieces together.

The C++ code require to be compiled, while the R code needs certain library. See the next section for details.

Here are the third-party softwares included in this version:
* [Louvain](http://perso.uclouvain.be/vincent.blondel/research/louvain.html) by Vincent D. Blondel, Jean-Loup Guillaume, Renaud Lambiotte and Etienne Lefebvre. We actually extended this program so that it can take advantage of link directions. Optional, it can be used for community detection.
* [Oslom](http://www.oslom.org/) by A. Lancichinetti, F. Radicchi, J.J. Ramasco and S. Fortunato. Optional, can be used for community detection.
* [Parallel *k*-means](http://users.eecs.northwestern.edu/~wkliao/Kmeans/) by Wei-keng Liao. Optional, can be used for the cluster analysis step.
* [*x*-means](http://www.rd.dnc.ac.jp/~tunenori/xmeans_e.html) by Tsunenori Ishioka. Optional, can be used for the cluster analysis step.
* **Nico, qu'en est-il du code source Guimerà & Amaral utilisé dans le calcul des mesures de rôles ? Je pense qu'il faut proprement citer ça, et laisser les readme ou autres fichiers originellement associés a ce programme.**


Note any cluster analysis tool could be used in place of the ones we selected, just by providing the appropriate R wrappers. Same thing for the community detection methods, except they must be compatible with the selected community role measures. Indeed, if the measures take advantage of linkdirections, the community detection method should, too. Also, if the measures are designed for disjoined community, the community detection method should output a partition (by opposition to a cover, with overlapping communities).

The R source code heavily relies on the [igraph package](http://igraph.org/redirect.html) to handle graphs. All the used R packages are listed in the `install.R` script.

## Installation
The R libraries can be installed simply by executing the script `install.R`.

The rest of the programs must be compiled, according to the specific instructions placed in the folder containing their source code. Of course, you only need to compile the tools you want to use.

## Use
The whole process is launched from an R console, by executing the script `main.R`. You can modify this script to specify the location of the original network, and dis/enable each step of the process.

The program expects a network represented as an edgelist file. Such a file contains the list of all edges constituting the network, under the form of pairs of node ids. The nodes numbering must start from zero. For testing purposes, it is possible to generate a dummy network by setting `gen.data` to `TRUE` in `main.R`. The original networks from [GA'05] are included in the folder data (or at least, networks very similar to the ones described in the article), also for testing purposes. They were retrieved from the [TUHH website](http://www.tuhh.de/ibb/publications/databases-and-software.html).

The program outputs a bunch of files corresponding to the different processing steps and tools used. For instance, if you perform the whole process with too different clustering algorithms, then each result file depending on clustering will be produced in two versions, for comparison purposes. 

## Extension
It is possible to include any clustering or community detection tool in the process, by providing the program and related R wrapper. I would advise first to copy-paste an existing wrapper, such as `CommunityDetection/directed-louvain.R`, and adapt it to the targeted program. Then, you must modify `CommunityDetection/community-detection.R` or `ClusterAnalysis/cluster-analysys.R` accordingly, by including your wrapper with `source` (at the beginning of the file) and by completing the method `detect.communities` or `detect.cluster`, respectively.

## References
* **[DLP'15]** N. Dugué, V. Labatut & A. Perez. *A community role approach to assess social capitalists visibility in the Twitter network*, Social Network Analysis and Mining, 5:26.1-26.13, 2015. [doi: 10.1007/s13278-015-0266-0](https://doi.org/10.1007/s13278-015-0266-0) - [⟨hal-01163741⟩](https://hal.archives-ouvertes.fr/hal-01163741)
* **[DLP'14a]** N. Dugué, V. Labatut & A. Perez. *Identifying the Community Roles of Social Capitalists in the Twitter Network*, 2nd IEEE/ACM International Conference on Advances in Social Network Analysis and Mining (ASONAM), 371-374, 2014. [⟨10.1109/ASONAM.2014.6921612](https://doi.org/⟨10.1109/ASONAM.2014.6921612) - [⟨hal-01011910⟩](https://hal.archives-ouvertes.fr/hal-01011910)
* **[DLP'14b]** N. Dugué, V. Labatut & A. Perez. *Identification de rôles communautaires dans des réseaux orientés appliquée à Twitter*, 14ème Conférence Extraction et Gestion des Connaissances, 125-130, 2014. [⟨hal-00918175⟩](https://hal.archives-ouvertes.fr/hal-00918175)
* **[DLP'13]** N. Dugué, V. Labatut & A. Perez. *Rôle communautaire des capitalistes sociaux dans Twitter*, 4ème Conférence sur les modèles et l'analyse de réseaux : approches mathématiques et informatiques (MARAMI), 12p, 2013. [⟨hal-00859779⟩](https://hal.archives-ouvertes.fr/hal-00859779)
* **[DP'13]** N. Dugué & A. Perez. *Detecting social capitalists on Twitter using similarity measures*, 4th Workshop on Complex Networks (CompletNet - Complex Networks IV), Springer, Studies in Computational Intelligence 476:1-12, 2013. [doi: 10.1007/978-3-642-36844-8_1](https://doi.org/10.1007/978-3-642-36844-8_1) - [⟨hal-00942668⟩](https://hal.archives-ouvertes.fr/hal-00942668)
* **[GA'05]** R. Guimerà & L. A. N. Amaral. *Functional cartography of complex metabolic networks*, Nature, 433:895-900, 2005. [doi: 10.1038/nature03288](https://doi.org/10.1038/nature03288)
