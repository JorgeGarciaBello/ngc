#!/bin/bash
#gfortran	-O3 -w  -fopenmp settings/*.f90 tools/*.f* packages/*.f* probability_of_transition_in_matter/*.f* daya_for_global_analysis/db_subroutines/*.f* reno_for_global_analysis/reno_subroutines/*.f*  find_out_reactor_flux/*.f* main_global.f90 -L /usr/local/lib -llapack -lblas
#ulimit -s unlimited 
#export KMP_STACKSIZE=5050m
#export OMP_DYNAMIC=FALSE
gfortran	 -w  -fopenmp settings/*.f90 tools/*.f* packages/*.f* packages/newuoa/*.f* packages/fortran_minimizators/*.f* probability_of_transition_in_matter/*.f* daya_for_global_analysis/db_subroutines/*.f* reno_for_global_analysis/reno_subroutines/*.f*  find_out_reactor_flux/*.f* find_out_reactor_flux/daya_bay/*.f* find_out_reactor_flux/daya_bay/newuoa_mod/*.f* main_global.f90 -L /usr/local/lib -llapack -lblas