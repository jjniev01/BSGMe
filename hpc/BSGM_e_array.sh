#!/bin/bash
#PBS -N BSGM_e_test
#PBS -l walltime=3:00:00
#PBS -l nodes=1:ppn=16


module load proj/4.9.3
module load gdal/1.10.1/gcc
module load geos/3.4.2
module load gcc/4.8.1
module load R/3.4.2

# argfile is a text file with space delimited arguments in each row and each row representing a new job. 
# The first argument should be the three letter ISO and the second argument should be the year. Both 
# arguments should be strings as indicated by '' 
INFILE=$PBS_O_WORKDIR/argfile_bsgme
#  Retrieve the row of arguments from row i as indicated by the job array id
ARGRUN=$(awk "NR==$PBS_ARRAYID" $INFILE)

TMPDIR=$PBS_O_WORKDIR/tmp/

TMP=$TMPDIR
TEMP=$TMPDIR

export TMPDIR TMP TEMP

Rscript --no-restore --no-save --slave /scratch/jjn1n15/BSGMe/BSGMe_array.R $ARGRUN
