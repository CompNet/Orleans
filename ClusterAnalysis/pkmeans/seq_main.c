/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   File:         seq_main.c   (an sequential version)                      */
/*   Description:  This program shows an example on how to call a subroutine */
/*                 that implements a simple k-means clustering algorithm     */
/*                 based on Euclid distance.                                 */
/*   Input file format:                                                      */
/*                 ascii  file: each line contains 1 data object             */
/*                 binary file: first 4-byte integer is the number of data   */
/*                 objects and 2nd integer is the no. of features (or        */
/*                 coordinates) of each object                               */
/*                                                                           */
/*   Author:  Wei-keng Liao                                                  */
/*            ECE Department Northwestern University                         */
/*            email: wkliao@ece.northwestern.edu                             */
/*                                                                           */
/*   Copyright (C) 2005, Northwestern University                             */
/*   See COPYRIGHT notice in top-level directory.                            */
/*                                                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>     /* strtok() */
#include <sys/types.h>  /* open() */
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>     /* getopt() */

int      _debug;
#include "kmeans.h"

/*---< usage() >------------------------------------------------------------*/
static void usage(char *argv0, float threshold) {
    char *help =
// TODO >> modified by VL: added new option "-c centers_filename "
        "Usage: %s [switches] -i filename -c centers_filename -n num_clusters\n"
// TODO << end of the modification
        "       -i filename    : file containing data to be clustered\n"
// TODO >> modified by VL: added new option
		"       -z centers_filename    : file containing the initial centers (default: first num_clusters objects)\n"
// TODO << end of the modification
        "       -b             : input file is in binary format (default no)\n"
        "       -n num_clusters: number of clusters (K must > 1)\n"
        "       -t threshold   : threshold value (default %.4f)\n"
        "       -o             : output timing results (default no)\n"
        "       -q             : quiet mode\n"
        "       -d             : enable debug mode\n"
        "       -h             : print this help information\n";
    fprintf(stderr, help, argv0, threshold);
    exit(-1);
}

/*---< main() >-------------------------------------------------------------*/
int main(int argc, char **argv) {
           int     opt;
    extern char   *optarg;
    extern int     optind;
           int     isBinaryFile, is_output_timing, verbose;

           int     numClusters, numCoords, numObjs;
           int    *membership;    /* [numObjs] */
           char   *filename;
// TODO >> modified by VL: new variable representing the centers file name
           char   *centers_filename;
// TODO << end of the modification
           float **objects;       /* [numObjs][numCoords] data objects */
           float **clusters;      /* [numClusters][numCoords] cluster center */
           float   threshold;
           double  timing, io_timing, clustering_timing;

    /* some default values */
    _debug           = 0;
    verbose          = 1;
    threshold        = 0.001;
    numClusters      = 0;
    isBinaryFile     = 0;
    is_output_timing = 0;
    filename         = NULL;
// TODO >> modified by VL: initialization of the new variable
    centers_filename = NULL;
// TODO << end of the modification

// TODO >> modified by VL: added the letter z
    while ( (opt=getopt(argc,argv,"p:i:z:n:t:abdohq"))!= EOF) {
// TODO << end of the modification
        switch (opt) {
            case 'i': filename=optarg;
                      break;
// TODO >> modified by VL: initialize centers filename
            case 'z': centers_filename=optarg;
                      break;
// TODO << end of the modification
            case 'b': isBinaryFile = 1;
                      break;
            case 't': threshold=atof(optarg);
                      break;
            case 'n': numClusters = atoi(optarg);
                      break;
            case 'o': is_output_timing = 1;
                      break;
            case 'q': verbose = 0;
                      break;
            case 'd': _debug = 1;
                      break;
            case 'h':
            default: usage(argv[0], threshold);
                      break;
        }
    }

    if (filename == 0 || numClusters <= 1) usage(argv[0], threshold);

    if (is_output_timing) io_timing = wtime();

    /* read data points from file ------------------------------------------*/
    objects = file_read(isBinaryFile, filename, &numObjs, &numCoords);
    if (objects == NULL) exit(1);

    if (is_output_timing) {
        timing            = wtime();
        io_timing         = timing - io_timing;
        clustering_timing = timing;
    }

    /* start the timer for the core computation -----------------------------*/
    /* membership: the cluster id for each data object */
    membership = (int*) malloc(numObjs * sizeof(int));
    assert(membership != NULL);

// TODO >> modified by VL: initialize variable "clusters"
    /* allocate a 2D space for returning variable clusters[] (coordinates of cluster centers) */
    clusters    = (float**) malloc(numClusters *             sizeof(float*));
    assert(clusters != NULL);
    clusters[0] = (float*)  malloc(numClusters * numCoords * sizeof(float));
    assert(clusters[0] != NULL);
    int i,j;
    for (i=1; i<numClusters; i++)
        clusters[i] = clusters[i-1] + numCoords;

    // possibly load the centers from a file
    if (centers_filename != NULL)
    {	int num_centers, num_coords;
    	clusters = file_read(0, centers_filename, &num_centers, &num_coords);
    	// no control over the numbers of centers and coordinates
    }
	// otherwise, pick the first numClusters elements of objects[] as initial cluster centers
    else
    {	for (i=0; i<numClusters; i++)
    		for (j=0; j<numCoords; j++)
    			clusters[i][j] = objects[i][j];
    }
// TODO << end of the modification

// TODO >> modified by VL: added "clusters" as a parameter representing the initial centers
    clusters = seq_kmeans(objects, numCoords, numObjs, numClusters, clusters, threshold, membership);
// TODO << end of the modification

    free(objects[0]);
    free(objects);

    if (is_output_timing) {
        timing            = wtime();
        clustering_timing = timing - clustering_timing;
    }

    /* output: the coordinates of the cluster centres ----------------------*/
    file_write(filename, numClusters, numObjs, numCoords, clusters,
               membership, verbose);

    free(membership);
    free(clusters[0]);
    free(clusters);

    /*---- output performance numbers ---------------------------------------*/
    if (is_output_timing) {
        io_timing += wtime() - timing;
        printf("\nPerforming **** Regular Kmeans (sequential version) ****\n");

        printf("Input file:     %s\n", filename);
// TODO >> modified by VL: display centers filename
        if(centers_filename!=NULL)
        	printf("Centers file:     %s\n", centers_filename);
// TODO << end of the modifications
        printf("numObjs       = %d\n", numObjs);
        printf("numCoords     = %d\n", numCoords);
        printf("numClusters   = %d\n", numClusters);
        printf("threshold     = %.4f\n", threshold);

        printf("I/O time           = %10.4f sec\n", io_timing);
        printf("Computation timing = %10.4f sec\n", clustering_timing);
    }

    return(0);
}

