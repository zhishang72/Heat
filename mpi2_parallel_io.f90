subroutine mpi2_parallel_io
use commons   
    integer, parameter :: maxfilename = 64
    character*(maxfilename) :: filename = 'output_parallel.dat'
!  Variables needed for MPI-IO 
    type(mpi_file) :: fh
    type(mpi_status) :: status
    integer (kind=mpi_offset_kind) :: disp = 0
    type(mpi_datatype) :: filetype, mpi_subarray
    integer, dimension(mpi_dim) :: glbsizes, locsizes, subsizes, starts
    
    subsizes(:) = [nxp, nyp]  
    glbsizes(:) = [imax, jmax]
    locsizes(:) = [nxp+2*xhalo, nyp+2*yhalo]
    starts(:) = [nxp*mpi_coords(1), nyp*mpi_coords(2)]
!  Create a global size of whole array with the individual sbuarray to output file       
    call mpi_type_create_subarray(mpi_dim, glbsizes, subsizes, starts, &
                                  mpi_order_fortran, mpi_real, &
                                  filetype, mpi_err)
    !if (mpi_err /= MPI_SUCCESS) write(*,*) 'Write error on rank ', cart_proc    
    call mpi_type_commit(filetype, mpi_err)
!  Create a local size of array with the individual sbuarray to output file    
    starts(:) = 1  
    call mpi_type_create_subarray(mpi_dim, locsizes, subsizes, starts, &
                                  mpi_order_fortran, mpi_real, &
                                  mpi_subarray, mpi_err)
    call mpi_type_commit(mpi_subarray, mpi_err)
!  Open the file for creating and writing only and attach to file handle fh
!  No IO hints are passed since MPI_INFO_NULL is specified
    call mpi_file_open_f08(mpi_comm_cart, filename, mpi_mode_create+mpi_mode_wronly, &
                           mpi_info_null, fh, mpi_err)
!  Set view for this process using appropriate datatype
    call mpi_file_set_view(fh, disp, mpi_real, filetype, 'native', &
                           mpi_info_null, mpi_err)
!  Write all the data for this process (ie NXP*NYP floats)
    call mpi_file_write_all_f08(fh, tmp, 1, mpi_subarray, status, mpi_err) 
!  Close file
    call mpi_file_close(fh, mpi_err)
    call mpi_type_free(mpi_subarray,mpi_err)
    call mpi_type_free(filetype,mpi_err)
return
end
