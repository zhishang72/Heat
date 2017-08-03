integer function indx(i,j)
use commons, only : nxp, xhalo
    integer :: i,j
    indx = (i+j*(nxp+2*xhalo))*sizeof(real)
return
end
!**************************************************************
!*                                                            *
!*            Subroutine for swapping halo points             *
!*                                                            *
!**************************************************************
subroutine mpi3_swap(a)
use commons 
    real, dimension(1-xhalo:nxp+xhalo,1-yhalo:nyp+yhalo) :: a    
    integer, dimension(4) :: counts
    type(mpi_datatype), dimension(4) :: types
    type(mpi_datatype) :: bot_type, top_type, west_type, east_type 
    integer(kind=mpi_address_kind), dimension(4) :: sdispls, rdispls    
    
    call mpi_type_vector_f08(nyp,1,(nxp+2*xhalo),mpi_real,west_type,mpi_err)
    call mpi_type_vector_f08(nyp,1,(nxp+2*xhalo),mpi_real,east_type,mpi_err)    
    call mpi_type_commit_f08(west_type,mpi_err)
    call mpi_type_commit_f08(east_type,mpi_err)
    
    counts = (/1, 1, nxp, nxp/) 
    types = (/west_type, east_type, mpi_real, mpi_real/) 
    
    sdispls = (/indx(1,1),       indx(nxp,1),       indx(1,1),       indx(1,nyp)/)
    rdispls = (/indx(1-xhalo,1), indx(nxp+xhalo,1), indx(1,1-yhalo), indx(1,nyp+yhalo)/)
    
    call mpi_neighbor_alltoallw_f08(a,counts,sdispls,types, &
                                    a,counts,rdispls,types, &
                                    mpi_comm_cart,mpi_err)
    call mpi_type_free_f08(west_type,mpi_err) 
    call mpi_type_free_f08(east_type,mpi_err) 
return
end
