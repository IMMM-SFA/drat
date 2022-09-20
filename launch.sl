#!/usr/bin/env /bin/bash

#SBATCH -A im3
#SBATCH -N 1
#SBATCH -p slurm
#SBATCH -t 72:00:00
#SBATCH -J drat
#SBATCH --mail-type=ALL
#SBATCH --mail-user=travis.thurber@pnnl.gov

module purge

module load gcc/11.2.0
module load R/4.0.2

srun R < drat.R --no-save

