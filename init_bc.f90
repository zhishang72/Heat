subroutine init_bc(thigh, tlow)
use commons
    real :: thigh, tlow
    integer :: x, y
! initial guess
    tmp(:,:) = tlow
! boundary conditions    
    if (nxp*mpi_coords(1) .eq. 0) tmp(1,:) = thigh
    if (nyp*mpi_coords(2) .eq. 0) tmp(:,1) = thigh 
    if (nxp*(mpi_coords(1)+1) .eq. imax) tmp(nxp,:) = tlow  
    if (nyp*(mpi_coords(2)+1) .eq. jmax) tmp(:,nyp) = tlow  
return
end
