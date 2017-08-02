!************************************************************************!
subroutine jacobi(xlen, ylen)
use commons
    integer :: x, y, iter, itermax
    integer :: xstart, xstop, ystart, ystop  
    real :: sc, dt, dt1, dt2
    real :: xlen, ylen, dx, dy, tol, error, errorsum 
    real :: diagx, diagy, weightx, weighty
    real, dimension(1-xhalo:nxp+xhalo,1-yhalo:nyp+yhalo) :: tmp0
    
    data xstart/1/, ystart/1/
    xstop = nxp
    ystop = nyp    
    data sc/3.0/, dt1/0.1/
    data itermax/800000/, iter/0/
    data errorsum/0.1/, tol/1.0e-3/

    dx  = xlen/dble(imax)
    dy  = ylen/dble(jmax)
    dt2 = 0.25*(min(dx,dy)**2)/sc

! Taking a good step for convergence 
    if (dt1.ge.dt2) then
       dt = dt2
    else 
       dt = dt1
    endif  
                                       
    diagx = - 2.0 + dx*dx/(2.0*sc*dt)
    diagy = - 2.0 + dy*dy/(2.0*sc*dt)
    weightx = sc * dt/(dx*dx)
    weighty = sc * dt/(dy*dy)
    
    do while ((errorsum .gt. tol) .and. (iter .lt. itermax))
       iter = iter + 1
       if (mod(iter,1000) == 0 .and. ioproc) then
          write(*,*)'iter = ',iter,', error = ',errorsum
       endif
! Copy back the computed value : t  <-- t^(n+1) 
!                                t0 <-- t^n       
       tmp0(:,:) = tmp(:,:)

       call mpi3_swap(tmp0)  
  
! The stencil of the explicit operator for the heat equation
! on a regular rectangular grid using a five point finite difference
! scheme in space is :
!
! |                                    weightx * t[x-1][y]                                    |
! |                                                                                           |
! | weighty * t[x][y-1]   (diagx * weightx + diagy * weighty) * t[x][y]   weightx * t[x][y+1] |  
! |                                                                                           |
! |                                    weighty * t[x+1][y]

  ! Perform an explicit update on the points within the domain
       if (nxp*mpi_coords(1) .eq. 0) xstart = 2 
       if (nyp*mpi_coords(2) .eq. 0) ystart = 2
       if (nxp*(mpi_coords(1)+1) .eq. imax) xstop = nxp - 1 
       if (nyp*(mpi_coords(2)+1) .eq. jmax) ystop = nyp - 1 
       do y = ystart, ystop
          do x = xstart, xstop
             tmp(x,y) = weighty *( tmp0(x-1,y) + tmp0(x+1,y) + tmp0(x,y)*diagy) +  &
                        weightx *( tmp0(x,y-1) + tmp0(x,y+1) + tmp0(x,y)*diagx) 
          enddo
       enddo
! Compute at the 2_norm of the "residual" 
       error = 0.0
       do y = 1, nyp
          do x = 1, nxp
             error = error + (tmp0(x,y) - tmp(x,y))**2
          enddo
       enddo
       call mpi_allreduce(error,errorsum,1,mpi_real,mpi_sum,mpi_comm_world,mpi_err)
       errorsum = sqrt(errorsum)
    enddo
return
end
