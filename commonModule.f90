module commons 
use mpi_f08
implicit none 
    integer, parameter :: master  = 0 
    integer, parameter :: mpi_dim = 2
! nprocx,nprocy.....mpi processors along x and y
    integer, parameter :: nprocx = 2, nprocy = 4
! imax, jmax.....grid size (number of points) in x,y  
    integer, parameter :: imax = 64,  jmax = 128     
!    integer, parameter :: imax = 128, jmax = 256  
!    integer, parameter :: imax = 256,  jmax = 512 
!    integer, parameter :: imax = 512,  jmax = 1024
!    integer, parameter :: imax = 1024, jmax = 2048
    integer, parameter :: nhalo = 1
    integer, parameter :: xhalo = nhalo, yhalo = nhalo
    integer, parameter :: nxp = (imax+nprocx-1)/nprocx, nyp = (jmax+nprocy-1)/nprocy 
! Temperature in domain
    real, dimension(1-xhalo:nxp+xhalo, 1-yhalo:nyp+yhalo) :: tmp
!------------------------------------------------------------------------------------!
!---Define MPI type for new Cartisian communicator
    logical :: ioproc
    type(mpi_comm) :: mpi_comm_cart
    integer :: procid,nprocs,cart_proc,mpi_err
    integer, dimension(mpi_dim) :: dims, mpi_coords
!---Define MPI type for Cartisian shift neighbor
    integer :: east = mpi_proc_null, west = mpi_proc_null, top = mpi_proc_null, bot = mpi_proc_null
end module commons
