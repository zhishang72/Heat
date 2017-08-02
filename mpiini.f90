! Routine to initialise MPI, get process id and number of processes      
subroutine mpiini
use commons
    integer :: errorcode 
    call mpi_init(mpi_err)
    call mpi_comm_size(mpi_comm_world, nprocs, mpi_err)
    call mpi_comm_rank(mpi_comm_world, procid, mpi_err)
! ioproc located on master process
    ioproc = .false.
    ioproc = procid.eq.master
    if (ioproc) then
       write(*,'(/a,i1,a,i4,a//)') "I am I/O processor at ID: ", procid, & 
            ", there are ", nprocs, " processors in total"
       if (nprocs.ne.nprocx*nprocy) then
          write(*,'(/2a/a,i5)') & 
               ' Error! Number of nodes does not', &
               ' match value specified by nprocx*nprocy', &
               ' in file commonModule.f90,  = ', nprocx*nprocy
          call mpi_abort(mpi_comm_world, errorcode, mpi_err)
       endif
    endif
  return
end
