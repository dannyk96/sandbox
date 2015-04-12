
module plot
    use segments            !- cos of type POINT
    integer,private :: ixo,iyo
    real,private    :: xmag,ymag
    logical         :: graphics_avail =.false.

contains
!-------------------------------------------------------------------------
   subroutine into_graphics ()
!
!   this enters graphics mode using the PGPLOT library 
!
      integer pgopen
!     call vga@()    - for Salford Fortran's graphics library 
      
      ipgstat=  pgopen ('?')
!     ipgstat=  pgopen ('/XWIN')
!     ipgstat=  pgopen ('/XTERM')
      graphics_avail = (ipgstat >0)
      if (.not.graphics_avail) then
        print*, 'warning : pgplot graphics not available'
        return
      endif
      call pgenv (0.,640., 0.,480.,0,1)

   end subroutine into_graphics 
!-----------------------------------------------------------------------
      subroutine into_vga_graphics ()
!
!   This create a 1024x768 graphics window on the screen
!   failing that a 800x600, and 640x480 modes are tried.
!      Dan Kidger  20-7-97
!
!   Maybe I should store the modes to try in a list
!   and try both with and without the vesa interface?
!
!   15-2-98 If windows, then create a 400x400 window?
!   3-8-00 now use PGPLOT in preference so no longer needed.
!
      integer*2 :: ifail
      integer :: modes(12)=(/ 1024,768,16, 800,600,256, &
                               640,490,16, 320,200,256/)
      integer imode,ii

!     call use_vesa_interface@()          !- more generic ?
!     do imode=1,4
      do imode=2,4
         ii=(imode-1)*3+1
         ixres = modes(ii)
         iyres = modes(ii+1)
         ncols = modes(ii+2)
         ifail=0
!        call graphics_mode_set@(ixres,iyres,ncols,ifail)
         if (ifail==0) exit
      enddo
      if (ifail/=0) print*,' failed to set graphics mode'

#ifdef SALFORD
      call vga@()      !- last resort ?
      call graphics_mode_set@(400,400,16,ifail) !- windows ?

!------ clear the screen to a given colour ----
      CALL SET_PALETTE@   (0,0)
      CALL SET_VIDEO_DAC@ (0, 0/4, 10/4, 60/4)    !- background colour
      CALL CLEAR_SCREEN_AREA@ (INT(0),INT(0),   &
     &                       INT(ixres),INT(iyres),int(0))
#endif

      print*,'<> Entering', ixres,'x',iyres,' Video mode'
    end subroutine into_vga_graphics


!-------------------------------------------------------------------------
   subroutine plot_edge (a,b,icol)
!
!    plots an edge segment.
!
    implicit none
    type (point) :: a,b
    real :: x1,y1,x2,y2
    integer ::icol
    if (.not.graphics_avail) return 
#ifdef DEBUG
    print*,'plot at',ixo+xmag*a%x,iyo+ymag*a%y
#endif
#ifdef SALFORD
   call draw_line@ (nint(ixo+xmag*a%x), nint(iyo+ymag*a%y), &
  &                 nint(ixo+xmag*b%x), nint(iyo+ymag*b%y),  icol)
#elif defined PGPLOT
    x1=ixo+xmag*a%x; y1=iyo+ymag*a%y
    x2=ixo+xmag*b%x; y2=iyo+ymag*b%y
#ifdef DEBUG
    print*, "drawing:", x1,y1, " ->",x2,y2 
#endif

    call PGSCI  (icol)
    call PGMOVE (x1,y1) 
    call PGDRAW (x2,y2)
#endif

   end subroutine plot_edge 

!-----------------------------------------------------------------------
      subroutine get_mesh_extent (gc,igc, nn)
!
!   Simply calculates the Bounding Box of the mesh
!    .. hence the plotting scale (from DANLIB)
!
      implicit none
      integer igc,nn
      real xmin,xmax,ymin,ymax, r,fact,datax,ar, dx,dy
      REAL GC(2,IGC)
      integer:: iresx = 640, iresy = 480
      AR = REAL(IRESX) / REAL(IRESY)       

!--- maxima and minima, hence size of bounding box -- 
      xmin = minval(gc(1,1:nn))
      xmax = maxval(gc(1,1:nn))
      ymin = minval(gc(2,1:nn))
      ymax = maxval(gc(2,1:nn))
      dx = xmax-xmin
      dy = ymax-ymin
      write(*,'(A,2(a,2g12.3))') 'Data range:', &
     &  'x-min/max=', xmin,xmax,' y-min/max=', ymin,ymax

!---------------------- normalize the data -----------------------------
!     FACT = 0.05         !- shrink factor
      fact = 0.15
      fact=0.30
      R = (1.-2.*FACT)    !- resulting size
      DATAX = MAX (DX,DY*AR)   !- scale factor
!     print*,' datax=', datax
!.. these next few lines are still wrong !
!     xmag =     r * iresx * (xmax-2*xmin) / datax
      xmag =     r * iresx / datax
!     xmag =       iresx * (fact+r*(xmax-xmin-xmin) / datax)
!     ymag =      -iresx * (fact+r*(ymax-ymin-ymin) / datax)
      ymag =      -xmag
      ixo =       iresx*(-fact/2.-xmin/datax)
      iyo = iresy-iresx*(-fact/2.-ymin/datax)

      ixo=iresx/2
      iyo=iresy/2


      print*,' ixo,iyo =', ixo,iyo
      print*,' xmag,ymag =', xmag,ymag

      END subroutine get_mesh_extent
  END MODULE PLOT
