program heat
use commons
    real :: xlen, ylen, thigh, tlow
    real :: t0, t1, t2, tcpu
    
    data xlen/1.0/, ylen/2.0/
    data thigh/100.0/, tlow/0.0/
    
    call mpiini    
    call mpipartition
    call init_bc(thigh, tlow)
    
! synchronise all processors and computing
    call mpi_barrier(mpi_comm_world, mpi_err)     
    call cpu_time(t1)
    call jacobi(xlen, ylen)
    call cpu_time(t2)
    t0 = t2 - t1
    call mpi_allreduce(t0,tcpu,1,mpi_real,mpi_max,mpi_comm_world,mpi_err)
    if (ioproc) write(*,*)'Maximum CPU execution time is: ',tcpu,'seconds'
! synchronise all processors and exit
    call mpi_barrier(mpi_comm_world, mpi_err)
    
    call mpi2_parallel_io
    call mpi_finalize(mpi_err)
stop
end
