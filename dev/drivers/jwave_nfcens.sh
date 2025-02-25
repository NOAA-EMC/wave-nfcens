#PBS -N wave_nfcens
#PBS -j oe
#PBS -S /bin/bash
#PBS -q dev             
#PBS -A VERF-DEV
#PBS -l walltime=00:30:00
#PBS -l select=1:ncpus=1:mem=50GB
#PBS -l debug=true

set -x
export OMP_NUM_THREADS=1
export HOMEwave_nfcens=/lfs/h2/emc/vpppg/noscrub/${USER}/wave-nfcens
source $HOMEwave_nfcens/versions/run.ver

module reset
module load prod_envir/2.0.6
module load prod_util/2.0.14
module load intel/${intel_ver}
module load libjpeg/${libjpeg_ver}
module load wgrib2/${wgrib2_ver}

export COMOUT=/lfs/h2/emc/vpppg/noscrub/${USER}/nfcens_output
export DATAROOT=/lfs/h2/emc/ptmp/${USER}/nfcens_test
export jobid=jwave_nfcens_${PBS_JOBID}

export cyc=00

export SENDCOM=YES
export SENDDBN=NO
export SENDECF=NO
export KEEPDATA=YES

# Run job script
${HOMEwave_nfcens}/jobs/JWAVE_NFCENS

exit

