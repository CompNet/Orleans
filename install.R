# Installs the required R packages.
#
# version: 1.1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("install.R")
###############################################################################


# power law tests
install.packages("igraph")
#print(packageVersion("igraph"))


# Davies-Bouldin measure
install.packages("clusterSim")
#print(packageVersion("clusterSim"))

# anova (unbalanced)
install.packages("car")


# Rand index
install.packages("flexclust")


# hive plots
install.packages("HiveR")#


# hclust clustering algorithm
install.packages("fastcluster")

# test for normality
install.packages("fBasics")

# Tukey's post-hoc test
install.packages("multcomp")

# Hive plot
install.packages("HiveR")
