#!/bin/bash
#SBATCH --job-name=2013
#SBATCH --output=cl_runs/2013_%A_%a.out
#SBATCH --error=cl_runs/2013_%A_%a.out
#SBATCH --time=36:00:00
#SBATCH --partition=sandyb
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --array=1-16

module load java
module load R

# Print this sub-job's task ID
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

Rscript find_dl.r data/2013/gcm21_i_m_8_n.rds 250 11 "selected days; with lasso; depth=10; hidden=200"

