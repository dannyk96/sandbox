program snowflake
!
!        Creates a 'showflake' fractal
!     Test driver program for my Advancing Front data structures
!        Dan Kidger 16-3-97

!   Method:
!     1. create the 3 original coords -> a ring
!     2. turn them into the inital front
!     3. loop   eg. to 3*2^level
!     4.   choose which edge to build on
!            usu = RH of the last 'new'
!            note can inhibit if depth>max.
!     5.   abstract points A and B, and length len
!     6.   construct new point C as .866*len 'up' from 'mid'
!     7.   replace old with AC
!     8.   insert CB as a new.
!     9. end loop
!
    use segments        !- the 'front' data structures and handlers
    use plot          !- Plotting routines
    implicit none       
    type (point) :: a,b,c, mid, d,e,f
    type (one_seg) :: base
    integer,parameter :: igc=10000
    real :: gc(2,igc)          !- coords of all the points
    integer:: ring(200)            !- the points in the initial front

    integer:: npoints,i,level,iseg,iseg2,iseg3,iloop,nextseg, ilevel
    integer :: isnow, ilength,imark,idepth
    real   :: fract

!   fract=0.5
!   level = 6            
   call into_graphics()
    write(*,'(a)') " 'Blancmange' fractal creator - Dan Kidger 16-3-97"
    print*,'How many points (3)'     ; read*,npoints
    print*,'Type (1=blob,2=snowflake,3=blancmange,4=square wave)';  read*,isnow
    print*,'Fractional height (0.5)' ; read*,fract
    print*,'Recursion level (6)'     ; read*,level

    call base_polygon (npoints)
!   print*, 'GC=',gc(:,1:npoints)
    call ring_to_front2 (ring,npoints,gc,igc,.true.) 
!   call ring_to_front2 (ring,npoints,gc,igc,.false.) 
!   call into_graphics()
    print*,'front length=',lfront
!--- plot the initial front ---
    call get_mesh_extent (gc,igc, npoints)
! seems to be a problem here - If I call into_graphics at the start of 
! the program, all is ok, but if I defer until here, then it seems to 
! have lost all the graphics drivers !
!   call into_graphics ()   !- this calls PGPLOT rather than Salford

    do i=1,lfront
!     print*,i,front(i)
      a = front(i)%v(1)
      b = front(i)%v(2)
      call plot_edge(a,b, 14)
    enddo
!--- loop and build elements
    iseg = 1                !- start fromt here
    do ilevel = 1,level   
       print*,'*** iterating for level=',ilevel,' of', level,' passes'
!      do iseg=1,lfront
!        write(*,'(i5,a,i5,a,i5)') &
!        front(iseg)%p(1),' <-', iseg, ' ->',front(iseg)%p(2) 
!      enddo
      ilength = front_size(1,imark,idepth)
      print*,' front_length=', front_size_2d(1), 'front_size(1,imark,idepth)=',front_size(1,imark,idepth)
!     print*,'   front_length=', ilength
      print*,'  imark=',imark,' max. depth=',idepth
      print*,' hit <CR> to continue'
      read*    !- pause
      do iseg=1,lfront
        if (front(iseg)%idepth >ilevel) cycle
!     if (iloop>1) iseg = base%p(1)   !- step to the left
!     if (iloop>1) iseg = nextseg   !- step to the left
      base = front(iseg)
      a = base%v(1)
      b = base%v(2)
      call plot_edge(a,b,1)
      mid%x = (a%x+b%x)/2.
      mid%y = (a%y+b%y)/2.

!--- 1:blob function
      if (isnow.eq.1) then
      c%x = mid%x + fract*base%len * base%dx
      c%y = mid%y + fract*base%len * base%dy
      call put_seg(iseg, a,c, 0,base%idepth+1, 0, base%p(2))
      call put_seg(0   , c,b, 0,base%idepth+1, base%p(1), iseg)
      call plot_edge(a,c,15)
      call plot_edge(c,b,15)

!--- 2:snowflake function                         
      elseif (isnow.eq.2) then
      c%x = (0.66667*a%x +0.33333*b%x)
      c%y = (0.66667*a%y +0.33333*b%y)
      d%x = mid%x + fract*base%len * base%dx   
      d%y = mid%y + fract*base%len * base%dy
      e%x = (0.33333*a%x +0.66667*b%x)
      e%y = (0.33333*a%y +0.66667*b%y)
      call put_seg(iseg, a,c, 0,base%idepth+1, 0, base%p(2))
      iseg2=0
      call put_seg(iseg2   , c,d, 0,base%idepth+1, 0, iseg)
      iseg3=iseg2
      iseg2=0
      call put_seg(iseg2   , d,e, 0,base%idepth+1, 0, iseg3)
      iseg3=iseg2
      iseg2=0
      call put_seg(iseg2   , e,b, 0,base%idepth+1, base%p(1), iseg3)

      call plot_edge(a,c,15)
      call plot_edge(c,d,15)
      call plot_edge(d,e,15)
      call plot_edge(e,b,15)

!--- 3:blancmange function
      elseif (isnow.eq.3) then
      c%x = mid%x 
      c%y = mid%y + fract*base%len
      call put_seg(iseg, a,c, 0,base%idepth+1, 0, base%p(2))
      call put_seg(0   , c,b, 0,base%idepth+1, base%p(1), iseg)
      call plot_edge(a,c,15)
      call plot_edge(c,b,15)

!--- 4: 'square wave'                         
      elseif (isnow.eq.4) then
      c%x = (0.66667*a%x +0.33333*b%x)
      c%y = (0.66667*a%y +0.33333*b%y)
      d%x = c%x + fract*base%len * base%dx   
      d%y = c%y + fract*base%len * base%dy
      f%x = (0.33333*a%x +0.66667*b%x)
      f%y = (0.33333*a%y +0.66667*b%y)
      e%x = f%x + fract*base%len * base%dx   
      e%y = f%y + fract*base%len * base%dy

      call put_seg(iseg, a,c, 0,base%idepth+1, 0, base%p(2))
      iseg2=0
      call put_seg(iseg2   , c,d, 0,base%idepth+1, 0, iseg)
      iseg3=iseg2
      iseg2=0
      call put_seg(iseg2   , d,e, 0,base%idepth+1, 0, iseg3)
      iseg3=iseg2
      iseg2=0
      call put_seg(iseg2   , e,f, 0,base%idepth+1, 0, iseg3)
      iseg3=iseg2
      iseg2=0
      call put_seg(iseg2   , f,b, 0,base%idepth+1, base%p(1), iseg3)

      call plot_edge(a,c,15)
      call plot_edge(c,d,15)
      call plot_edge(d,e,15)
      call plot_edge(e,f,15)
      call plot_edge(f,b,15)

      else
        stop 'Unknown shape function'
      endif

      nextseg=base%p(1)
    enddo    

    enddo    !- loop levels

!   ..now print out the ring as a polyline?
    read*
    stop

contains
!-------------------------------------------------------------------------
   subroutine base_polygon (npoints)
!
!    Builds the initial 'triangle' of points
!
     implicit none
     integer,intent(inout):: npoints           !- turned to ABS(npoints)
!    real :: xc=20.,yc=20.,radius = 10.
     real :: xc=0.,yc=0.,radius = 20.
     integer:: i
     real :: delta,ang

     delta= 360./real(npoints)
     npoints = abs(npoints)
     do i=1,npoints
       ang = delta/2. + (i-1)*delta 
       ang = ang*3.14159265/180.     !- to radians
       ring(i) = i
       gc(1,i) = xc - radius*sin(ang)
       gc(2,i) = yc - radius*cos(ang)
     enddo
   end subroutine base_polygon

!-------------------------------------------------------------------------
end program snowflake
!-----------------------------------------------------------------------

!--------------------------------------------------------------------------

