Program Ruritania_Stamps
!
!  This solves the 'Ruritania Postage Stamps' problem
!


!  integer, parameter :: n=7     !/* number of different stamps */
  integer,allocatable :: set(:), best_set(:)
  integer :: best_total, ic
  logical :: finished 

  character fmt*25

  do n=2,50
    allocate (set(0:n),best_set(0:n))
  write (fmt,'(a,i2,a)') '(i4,a,',n+1,'i4,a,i5,a,i5)'

  call setup (set,n)
  print*,'n=',n
  best_total = 0 
  finished = .false.

  do ic = 1,huge(ic)
    max_value = Get_Max_Val (set,n)
    if (max_value > best_total) then
      best_set   = set
      best_total = max_value
    endif
!    print*,set(0),set(1),set(2)

!    write(*,fmt) ic, '  set = ',set,' max val = ',Max_value &
!  &    ,' best = ',best_total
    call Generate_Next_Set (set,n,max_value,best_total,finished)
    if (finished) exit
  enddo
  print*,ic,' combinations were tried:'
  print*,' The best set of stamps was', best_set
  print*,' With a maximum value of :',best_total


   deallocate (set,best_set)
  enddo   !- try ALL sets !

contains
!-----------------------------------------------------------------------

 Integer Function Get_Max_Val (set,n) result (max_val)
!
!  ... Just assume 3 stamps for now .. generalise later ..
!
 integer :: set(0:n)
 logical :: postage(0:3*set(n)+1) 

 max_val = -1

 postage = .false.
 do i=0,n
   do j=i,n
     do k=j,n
       postage(set(i)+set(j)+set(k)) = .true.
     enddo
   enddo
 enddo
 do i=1,3*set(n)+1
   if (.not.postage(i)) then
     max_val = i-1
     return
   endif
 enddo

 End Function Get_Max_Val

!-----------------------------------------------------------------------
 Subroutine Generate_Next_Set (set,n, max_value,best_total,finished)
!
! This will produce the next set of stamps to try 
! with 'finished' set to .true. when no more sets are possible 
!
   logical :: finished
   integer :: set(0:n), n, best_total

! if (0==1) then

   if (set(n) > max_value) then   !- speed up the search
     do i=n,1,-1
       if (set(i-1) < max_value) exit
     enddo
!    i = i + 2                 !/* fudge up i->i+1 */
  3 set(i) = max_value+1
    do j=i+1,n
      set(j) = 3*set(j-1) +1       !/* reset values to the right */
    enddo

  else  !------------------------------------------------

 2 do i=n,2,-1
     if (set(i) /= set(i-1)+1) goto 1       !/* room to decrement? */
   enddo
   finished = .true.
   return

 1 set (i) = set(i) - 1                       !/* decrement */
       do j= i+1,n                        !/*        and        */
         set(j) = 3*set(j-1) + 1      !/* reset values to the right */
       enddo

     if (3*set(n) <= best_total) then        !
       set(n) = set(n-1) + 1
       goto 2
     endif

  endif        !- 2 methods of modifying the set

  End Subroutine Generate_Next_Set
!-----------------------------------------------------------------------
 Subroutine Setup (set,n)
!
!  gives the initial value for the set of stamps
!
   integer :: set(0:n)
   set(0) = 0
   set(1) = 1
   do i=2,n
     set(i) = 3*set(i-1) + 1
   enddo
 End subroutine setup
!-----------------------------------------------------------------------
End Program Ruritania_Stamps
!-----------------------------------------------------------------------

