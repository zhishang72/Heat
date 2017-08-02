# simple make file for 2D heat code by finite difference method
#
# Author: Zhi Shang <zhishang72@gmail.com>  
#
GFORTRAN    = gfortran -O3 -ffpe-summary='none'
MPI_FORTRAN = mpif90 -O3 -ffpe-summary='none'
LD = -lm -lmpi

.SUFFIXES : .o .f90

all: heat

heat : commonModule.o heat.o mpiini.o mpipartition.o \
       init_bc.o jacobi.o mpi3_swap.o mpi2_parallel_io.o 
	$(MPI_FORTRAN) -o $@ commonModule.o heat.o mpiini.o mpipartition.o \
	init_bc.o jacobi.o mpi3_swap.o mpi2_parallel_io.o 

.f90.o :
	$(MPI_FORTRAN) -c $(*F).f90

clean : 
	/bin/rm -f *.o *.dat *.mod heat
