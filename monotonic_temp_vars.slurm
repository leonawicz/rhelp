#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=1
#SBATCH --nodes=1
####SBATCH --nodelist=atlas09
####SBATCH --exclude=atlas01,atlas03,atlas04,atlas05,atlas06
#SBATCH --account=snap
#SBATCH -p main
#SBATCH --job-name=qc

#### $1 gcm index i and $2 rcp index j, e.g., i=1, j=1
Rscript /workspace/UA/mfleonawicz/tmpDir/monotonic_temp_vars.R $1 $2
