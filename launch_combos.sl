#!/usr/bin/env /bin/bash

#SBATCH -A im3
#SBATCH -n 1
#SBATCH -c 8
#SBATCH -p shared
#SBATCH -t 168:00:00
#SBATCH -J drat_combos
#SBATCH --mail-type=ALL
#SBATCH --mail-user=travis.thurber@pnnl.gov
#SBATCH --array=1-27

module purge

module load gcc/11.2.0
module load R/4.0.2

Rscript drat_combos.R $SLURM_ARRAY_TASK_ID

