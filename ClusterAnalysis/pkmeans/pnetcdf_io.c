/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   File:         pnetcdf_io.c                                              */
/*   Description:  This program reads data points from a file using PnetCDF  */
/*                 that implements a simple k-means clustering algorithm     */
/*   Input file format:                                                      */
/*                 netCDF file                                               */
/*                 three arrays: color, edge, and texture                    */
/*   netcdf images {                                                         */
/*   dimensions:                                                             */
/*           num_elements_color = 17695 ;                                    */
/*           num_coordinates_color = 9 ;                                     */
/*           num_elements_edge = 17695 ;                                     */
/*           num_coordinates_edge = 18 ;                                     */
/*           num_elements_texture = 17695 ;                                  */
/*           num_coordinates_texture = 20 ;                                  */
/*   variables:                                                              */
/*           float color(num_elements_color, num_coordinates_color) ;        */
/*           float edge(num_elements_edge, num_coordinates_edge) ;           */
/*           float texture(num_elements_texture, num_coordinates_texture) ;  */
/*                                                                           */
/*   Author:  Wei-keng Liao                                                  */
/*            EECS Department, Northwestern University                       */
/*            email: wkliao@ece.northwestern.edu                             */
/*                                                                           */
/*   Copyright (C) 2005, Northwestern University                             */
/*   See COPYRIGHT notice in top-level directory.                            */
/*                                                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mpi.h>
#include <pnetcdf.h>
#include "kmeans.h"

#ifdef _PNETCDF_BUILT

/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", ncmpi_strerror(e)); return NULL;}
#define ERR2(e) {printf("Error: %s\n", ncmpi_strerror(e)); return -1;}

/*---< pnetcdf_read() >------------------------------------------------------*/
float** pnetcdf_read(char     *filename,      /* input netCDF file name */
                     char     *varname,       /* data object name in netCDF */
                     int      *numObjs_ptr,   /* no. data objects (local) */
                     int      *numCoords_ptr, /* no. coordinates */
                     MPI_Comm  comm)
{
    float    **objects;
    int        i, len, divd, rem;
    int        rank, nproc;
    int        ncid, ndims, dimids[2], varid, retval;
    MPI_Offset starts[2], subsizes[2];
    MPI_Offset numObjs, numCoords;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nproc);

    if ((retval = ncmpi_open(comm, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid)))
        ERR(retval);

    if ((retval = ncmpi_inq_varid(ncid, varname, &varid)))
        ERR(retval);

    if ((retval = ncmpi_inq_varndims(ncid, varid, &ndims)))
        ERR(retval);
    if (ndims != 2) {
        printf("Error: read ndims(%d) from netCDF file (should be 2)\n",ndims);
        return NULL;
    }

    if ((retval = ncmpi_inq_vardimid(ncid, varid, dimids)))
        ERR(retval);

    if ((retval = ncmpi_inq_dimlen(ncid, dimids[0], &numObjs)))
        ERR(retval);

    if ((retval = ncmpi_inq_dimlen(ncid, dimids[1], &numCoords)))
        ERR(retval);

    if (_debug) printf("numObjs=%lld numCoords=%lld\n",numObjs,numCoords);

    if (numObjs <= 0 || numCoords <= 0) {
        if (rank == 0)
            printf("Error: file format (%s) numObjs=%lld numCoords=%lld\n",filename,numObjs,numCoords);
        ncmpi_close(ncid);
        MPI_Finalize();
        exit(1);
    }
    if (numObjs < nproc) {
        if (rank == 0)
            printf("Error: number of data points must be larger than the number of MPI processes.\n");
        ncmpi_close(ncid);
        MPI_Finalize();
        exit(1);
    }

    divd = numObjs / nproc;
    rem  = numObjs % nproc;
    len  = (rank < rem) ? rank*(divd+1) : rank*divd + rem;
    starts[0] = len;
    starts[1] = 0;

    /* adjust numObjs to be local size */
    numObjs = (rank < rem) ? divd+1 : divd;

    /* allocate space for data points */
    objects    = (float**)malloc(numObjs           * sizeof(float*));
    assert(objects != NULL);
    objects[0] = (float*) malloc(numObjs*numCoords * sizeof(float));
    assert(objects[0] != NULL);
    for (i=1; i<numObjs; i++)
        objects[i] = objects[i-1] + numCoords;

    subsizes[0] = numObjs;
    subsizes[1] = numCoords;

    if ((retval = ncmpi_get_vara_float_all(ncid, varid, starts, subsizes, objects[0])))
        ERR(retval);

    if ((retval = ncmpi_close(ncid)))
        ERR(retval);

    *numObjs_ptr   = numObjs;
    *numCoords_ptr = numCoords;

    return objects;
}


/*---< pnetcdf_write() >-----------------------------------------------------*/
int pnetcdf_write(char      *filename,     /* input file name */
                  int        save_in_new_file,  /* 0 or 1 */
                  int        numClusters,  /* no. clusters */
                  int        numObjs,      /* no. data objects */
                  int        numCoords,    /* no. coordinates (local) */
                  float    **clusters,     /* [numClusters][numCoords] centers*/
                  int       *membership,   /* [numObjs] */
                  int        totalNumObjs, /* total no. data objects */
                  MPI_Comm   comm,
                  int        verbose)
{
    int   rank, nproc, divd, rem;
    char  outFileName[1024];
    int        ncid, dimids[2], dim_num_obj, clusters_varid, membership_varid, retval;
    MPI_Offset start, count;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nproc);

    /* output: the coordinates of the cluster centres ----------------------*/
    /* only proc 0 matters this, because clusters[] are the same across all proc */

    /* to save the results in a new netCDF file */
    if (save_in_new_file) {
        if (strcasecmp(filename+strlen(filename)-3, ".nc") == 0) {
            strcpy(outFileName, filename);
            outFileName[strlen(filename)-3] = '\0';
            strcat(outFileName, ".cluster_centres.nc");
        }
        else
            sprintf(outFileName, "%s.cluster_centres.nc", filename);

        if (rank == 0 && verbose) {
            printf("Writing coordinates of K=%d cluster centers to file \"%s\"\n",
                   numClusters, outFileName);

            printf("Writing membership of N=%d data objects to file \"%s\"\n",
                   totalNumObjs, outFileName);
        }

        /* Create the file. The NC_CLOBBER parameter tells netCDF to
         * overwrite this file, if it already exists.*/
        if ((retval = ncmpi_create(comm, outFileName, NC_CLOBBER | NC_64BIT_OFFSET, MPI_INFO_NULL, &ncid)))
            ERR2(retval);

        /* Define the dimensions. NetCDF will hand back an ID for each. */
        if ((retval = ncmpi_def_dim(ncid, "num_clusters", numClusters, &dimids[0])))
            ERR2(retval);

        if ((retval = ncmpi_def_dim(ncid, "num_coordinates", numCoords, &dimids[1])))
            ERR2(retval);

        if ((retval = ncmpi_def_dim(ncid, "num_elements", totalNumObjs, &dim_num_obj)))
            ERR2(retval);

        /* Define the clusters variable. The type of the variable in this case is
         * NC_FLOAT (4-byte float). */
        if ((retval = ncmpi_def_var(ncid, "clusters", NC_FLOAT, 2, dimids, &clusters_varid)))
            ERR2(retval);

        /* Define the membership variable. The type of the variable in this case is
         * NC_INT (4-byte integer). */
        if ((retval = ncmpi_def_var(ncid, "membership", NC_INT, 1, &dim_num_obj, &membership_varid)))
            ERR2(retval);

        /* End define mode. This tells netCDF we are done defining
         * metadata. */
        if ((retval = ncmpi_enddef(ncid)))
            ERR2(retval);
    }
    else { /* add new variables into existing netCDF file */
    }

    /* write cluster centers */
    if ((retval = ncmpi_put_var_float_all(ncid, clusters_varid, *clusters)))
        ERR2(retval);

    /* write membership variable */
    divd  = totalNumObjs / nproc;
    rem   = totalNumObjs % nproc;
    start = (rank < rem) ? rank*(divd+1) : rank*divd + rem;
    count = numObjs;
    if (_debug) printf("%2d: start=%lld count=%lld\n",rank,start,count);

    if ((retval = ncmpi_put_vara_int_all(ncid, membership_varid, &start, &count, membership)))
        ERR2(retval);

   /* Close the file. This frees up any internal netCDF resources
    * associated with the file, and flushes any buffers. */
   if ((retval = ncmpi_close(ncid)))
      ERR2(retval);

    return 1;
}

#endif
