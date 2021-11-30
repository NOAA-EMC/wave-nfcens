#!/bin/bash
###############################################################################
#                                                                             #
# Compiles all codes, moves executables to exec and cleans up                 #
#                                                                             #
#                                                               March, 2020   #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #

# Load modulefile
#  module purge
#  module use ../modulefiles_wave_nfcens
#  module load build_gwes_sorc.module  
#  module list
source ../versions/build.ver

outfile=`pwd`/make.all.out
rm -f ${outfile}

module purge
source ../modulefiles/build_waves_nfcens.module

# 1. Preparations: seek source codes to be compiled

  fcodes=nfcombwave_ensemble

  echo " FORTRAN codes found: "$fcodes

  outfile=`pwd`/make_code_wave_nfcens.out
  rm -f ${outfile}

if [ ! -d "../exec" ]; then
   echo 'Creating exec directory'
   mkdir ../exec
fi

# 2. Create executables

  for code in $fcodes
  do
    echo " Making ${code} " >> ${outfile}
    cd ${code}.fd 
    make clean > ${outfile} 2>> ${outfile}
    module list >> ${outfile} 2>> ${outfile}
    make >> ${outfile}
    echo " Moving ${code} to exec" >> ${outfile}
    mv ${code} ../../exec
    echo " Cleaning up ${code} directory" >> ${outfile}
    make clean
    echo ' ' >> ${outfile}
    cd ..
  done

