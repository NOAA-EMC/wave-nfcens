--#%Module############################################################
--#
--#    - WAVE_NFCENS
--#    - NCEP/FNMOC Combined Wave Ensembles
--#
--####################################################################
--proc ModulesHelp { } {
--  puts stderr "Sets environment variables for WAVE_NFCENS"
--  puts stderr "This module initializes the environment"
--  puts stderr "to build the WAVE_NFCENS software at NCEP"
--}

whatis("WAVE_NFCENS module for compilation")

-- Load Intel Compiler

load("PrgEnv-intel/"..os.getenv("PrgEnv_intel_ver"))
load("craype/"..os.getenv("craype_ver"))
load("intel/"..os.getenv("intel_ver"))

-- Load Supporting Software Libraries

load("w3nco/"..os.getenv("w3nco_ver"))
load("g2/"..os.getenv("g2_ver"))
load("bacio/"..os.getenv("bacio_ver"))
load("jasper/"..os.getenv("jasper_ver"))
load("libpng/"..os.getenv("libpng_ver"))
load("zlib/"..os.getenv("zlib_ver"))
load("libjpeg/"..os.getenv("libjpeg_ver"))

