#!/bin/bash

#$ -m abe
#$ -pe smp 24
#$ -q long
#$ -N job_name
#$ -t 1-6

module load R

cps=(0 1e-5 1e-4 1e-3 1e-2 1e-1)

cd ../
Rscript meals/${1}.R "${@:2}" --cp ${cps[$SGE_TASK_ID-1]}
