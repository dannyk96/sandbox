   program magic_square
!
!  This Calculates a Magic square
!   ie. one whose rows and columns 
!     all add up the same number
!
!   Version 1 :   This does *not* use subroutines 
!                    -> see Version 2 next week
!
!       D. Kidger  17-10-95
!

implicit none
integer, allocatable :: a(:,:)           !- the array
integer i,j,n, ix,iy, ix1, iy1, ntot     !- some variables


!-------------------------- input the matrix size --------------------------
print*, "Enter the order 'n' of the square-matrix that you want"
read*,n
 if (mod(n,2) /= 1) stop 'That was not an Odd number !'

 ntot = n*n
 allocate ( a(n,n) )

!----------------------- compute the magic-square --------------------------
 a = 0          !- start with a blank array
 ix=n/2+1          !- start at the top-centre of the box
 iy=n
 do i=1,ntot       !---- loop the numbers to insert into the table -----
   a(ix,iy) = i
   ix1 = mod(ix,n) +1
   iy1 = mod(iy,n) +1
   if (a(ix1,iy1).ne.0)  then       !- already filled so use the
     ix1 = ix                       !- space below instead
     iy1 = mod (iy-1,n)             ! (do we need to test this one too ?)
   endif
   ix = ix1                         !- update the valid 'new' location
   iy = iy1
 enddo

!----------------------- print out the table --------------------------
 do j=n,1,-1
   write (*,'(99i4)') &
   (a(i,j),i=1,n)
 enddo

 write (*,'(99a4)')  &
    ( '----',i=1,n)
   write (*,'(99i4)') &
   (sum(a(i,:)),i=1,n)
 
!----------------------- finish using the array -------------------------
  deallocate (a)

  end program magic_square



