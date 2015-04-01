program data_structures
!
!   Example of a set of Railway Stations to test data structures
!   For the 2nd Year Undergraduates
!   by  Dr. D. Kidger  3rd Oct 1995

type station
  character :: name*20
  real :: dist, total_dist
  integer :: platforms
  logical :: disabled
end type station

type (station), allocatable :: mystops (:)

open (10,file='blackp.dat')

read (10,*) nstations
allocate (mystops(nstations))
do i=1,nstations
  read (10,*) mystops(i)%name, mystops(i)%dist, mystops(i)%platforms, mystops(i)%disabled
enddo

d = 0.
do i=1,nstations
  d = d + mystops(i)%dist
  mystops(i)%total_dist = d
enddo

write (*,'(a)')    &
 '   Station      No. of     Disabled    Dist from        Dist from    ',   &
 '   Name        Platforms    Access     Last Station    First Station ',   &
 '---------------------------------------------------------------------'

do i=1,nstations
  write (*,'(A15 ,i7 ,l10 ,2g20.2)')  &
  mystops(i)%name, mystops(i)%platforms, mystops(i)%disabled   &
  , mystops(i)%dist, mystops(i)%total_dist
enddo

end program data_structures


