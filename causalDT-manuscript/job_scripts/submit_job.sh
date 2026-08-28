#!/bin/bash

#$ -m abe
#$ -pe smp 24
#$ -q long
#$ -N job_name

module load R

cd ../
Rscript meals/${1}.R "${@:2}"
