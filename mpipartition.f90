subroutine mpipartition
use commons
! Local variables
    logical, dimension(mpi_dim) :: periodic
    integer :: direction, shift
    logical :: reorder

! Initialize data for domain partitioning. Defaults are:
! Partitioning is periodic in all dimensions (periodic = .false.)
! CPUs are reordered in the grid for proximity (reorder = .false.)
    dims(1)     = nprocx
    dims(2)     = nprocy
    periodic(1) = .false.
    periodic(2) = .false.
    reorder     = .true.

! Create the new virtual connectivity grid
    call mpi_cart_create_f08(mpi_comm_world,mpi_dim,dims,periodic, &                      
                             reorder,mpi_comm_cart,mpi_err)
    call mpi_comm_rank(mpi_comm_cart,cart_proc,mpi_err)  
    ioproc = .false.    
    ioproc = cart_proc.eq.master

!-------- Determine neighbours of this processor -------------------------------
! MPI_CART counts dimensions using 0-based arithmetic so that
! direction = 0 -> px  |  direction = 1 -> py  
    shift     = 1
    direction = 0
    call mpi_cart_shift(mpi_comm_cart,direction,shift,west,east,mpi_err)
    direction = 1
    call mpi_cart_shift(mpi_comm_cart,direction,shift,bot,top,mpi_err)

! Get this processor ID and coordinates within the virtual grid
    call mpi_cart_coords(mpi_comm_cart,cart_proc,mpi_dim,mpi_coords,mpi_err)
return
end 
