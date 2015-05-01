  program hello
   include 'mpif.h'
   integer :: iproc=-1
   call mpi_init(ifail)
   call mpi_comm_rank (mpi_comm_world,iproc,ifail)
   print*,'hello world from', iproc
   call mpi_finalize(ifail)
   end 
