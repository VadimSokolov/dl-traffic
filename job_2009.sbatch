#!/bin/bash
#SBATCH --job-name=2009
#SBATCH --output=cl_runs/2009_%A_%a.out
#SBATCH --error=cl_runs/2009_%A_%a.out
#SBATCH --time=36:00:00
#SBATCH --partition=sandyb
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --array=2-21

module load java
module load R

# Print this sub-job's task ID
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

Rscript find_dl.r ../../data/2009/gcm21_i_tf_20.rds 2000 $SLURM_ARRAY_TASK_ID "use only d.train"


