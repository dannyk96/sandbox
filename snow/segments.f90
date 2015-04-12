
!-----------------------------------------------------------------------
!                             SEGMENTS module
!   Routines for manipulating an 'advancing front' of edge/triangle segments
!                     Dr. Dan Kidger   d.kidger@man.ac.uk
!
!   This is n-ndimensional and is designed to be optimised for a 3d 'sheel' 
!    front of 3-node triangles
!
!   Module DataStructures:
!       POINT : the x,y,(z) and node number of a point
!     ONE_SEG : a segements 2,(3) POINTS, edge length & normal vector
!       FRONT : the complete polygonal front as a set of ONE_SEG
!
!   Entrypoints:
!    RING_TO_FRONT2 : creates the original front from a set of X,Y coords
!    PUT_SEG        : adds a segment to the first *vacant* space
!    DELETE_SEG     : removes a segment, leaving a 'vacent space'
!    GET_NEXT_SEG   : steps to the next segment to the left/right
!    FRONT_SIZE_2D  : simplex 2d method for counting the #segments in a ring
!    NO_OF_FRONTS   : determines the no of independant fronts (via FRONT_SIZE)
!    FRONT_SIZE     : stack-based 'recursive' parse of all segemnts (2d/3d)
!      PUSH_SEG       : puts an segment onto the FIFO stack
!      POP_SEG        : returns a segment off the FIFO stack
!      TIDY_STACK     : garbage collection of space at the base the stack
!      GET_UNIQUE_MARK: simply returns a unique number (to label a front with)
!    FIND_NODE_IN_FRONT: returns which segment and its 'depth' (so sort?)
!
!    routines for 
!
!
!
!
!-----------------------------------------------------------------------
 module segments
!
!    Defines the data structure used to store the front segments in.
!    plus the subroutines that act upon it.
!      c. Dr. D. Kidger 16-3-97
!
   implicit none
!------ a single point ------
   type point
     real:: x,y !,z       !  coord
     integer::n           !  node number
   end type point
! eg.  type (point) :: a,b,mid,c,n,m

!------- a segment in the front -------
!  integer,parameter :: tri_type=3   !- or 2 for a line structure
   type one_seg
     type(point)::v(2)        !- the vertex numbers (+coords) of the 2 ends
     real :: len              !- the length of the edge
     real :: dx,dy !,dz       !- the normal (unit) vector to the edge
     integer :: idepth        !- 'depth' from the boundary   (=0)
     integer :: mark          !- used to mark a closed ring with a unique id.
     integer :: p(2)          !- pointer to the left/right segments
     integer :: iel           !- which element is 'underneath'
   end type one_seg
  
!---------- the whole front database ----------
   integer, parameter          :: mfront=20000
!  type (one_seg), allocatable :: front(:)
   type (one_seg)              :: front(0:mfront)  !0 = for bug-fixing

   integer         :: lfront           !- the current length of the front
!  integer,private :: ihole            !- pointer to the first available hole
   integer:: ihole            !- pointer to the first available hole

!------- the FIFO stack of 'sweeping'  of the front -------
! 2d will only ever need a stack length of 3
   integer,parameter :: max_stack=1000
   integer :: stack(2,max_stack)

contains

!-----------------------------------------------------------------------
   subroutine ring_to_front2 (ring,nring,gc,igc,closed) 
!
!     Simply creates the initial front from the list of nodes in RING()
!      called from ADVANCING_FRONT
!       Dan Kidger 18-3-96
!  3-8-00 seems to be some sort of malloc error here
!
      implicit none
      integer,intent(in):: nring, igc
      real   ,intent(in):: gc(2,igc) 
      integer,intent(in):: ring(nring)
      logical,intent(in):: closed         !- flag to 'close' the ring.

      type(point):: p1,p2
      integer i,j, i1,i2, iel, idepth, ileft,iright, iseg

      lfront= 0                    !- inital size
      ihole = 1                    !- insertion point
      do i = 1,nring
         j = mod(i,nring)+1        !- the 'next' node/segment
!        k = mod(i-1+nring,nring)  !- the previous seg/node
         i1= ring(i)               !- so the 2 nodes
         i2= ring(j)
         p1 = point (gc(1,i1),gc(2,i1), i1)  !- add their coords too
         p2 = point (gc(1,i2),gc(2,i2), i2)
         print*,'seg #',i,' p1=',p1,' p2=',p2
!-- the two neighbouring segments --
!  as we work we have 2 free ends.
!   only the last segemnt will close the ring
         ileft =i-1; if (i==1)     ileft=0
         iright=0  ; if (closed .and. i==nring) iright=1
! DJK 3-8-00 why do we not need to clean the pointers 
! i.e. fix the non-last iright?

         iel=0                     !- cos there are no elements underneath
         idepth=0                  !- cos it is the original surface
! DJK 3-8-00 swapped first of these two lines back in
!        iseg=i
         iseg=0
         call put_seg (iseg, p1,p2, iel,idepth, iright,ileft)
!        call put_seg (0, p1,p2, iel,idepth, iright,ileft)
      enddo
      return
      end subroutine ring_to_front2

!-----------------------------------------------------------------------
     subroutine put_seg (iseg, p1,p2, iel,idepth, iright,ileft)
!
!    Adds a segment record to the database 
!    - also updates the left and right-hand pointers
!        Dan Kidger 16-3-97 
!
!     - if iseg >0 then it is explicit
!       if iseg =0 then we auto-choose the next available hole
!     - IEL should be set the element that is being added - so records
!       which element lies *under* this segment (optional)
!     - IDEPTH is the distance that this segment is from the original boundary
!       (eg just used for colour-coding plots)
!
     integer :: iseg,  ileft,iright, iel,idepth
     type (point) :: p1,p2
     real dx,dy, len  !,dz
! bug was in teh next line: if iseg is passed as an actual '0' then cant do iseg=foo here!
     if (iseg==0) then                   !- auto fill the next hole
       iseg = ihole
       if (ihole>lfront) then           !- already at the end of the list
         lfront= lfront+1
         ihole = lfront+1               !- next hole
       else
         !print*,front(iseg)%iel
         ihole = abs(front(iseg)%iel)   !- next hole 
       endif
     else
       if (iseg>lfront) then
         lfront = lfront+1
         ihole = lfront+1
       endif
     endif

!    print*,'writing to segment #',iseg,' ileft=',ileft,' iright=',iright
     front(iseg)%v(1) = p1
     front(iseg)%v(2) = p2
     front(iseg)%iel = iel
     front(iseg)%idepth = idepth
!     print*,'p1=',p1,' p2=',p2

!     dx = front(iseg)%x(2)-front(iseg)%x(1)     !- vector of the edge
!     dy = front(iseg)%y(2)-front(iseg)%y(1)
!    dz = front(iseg)%z(2)-front(iseg)%z(1)
     dx = front(iseg)%v(2)%x-front(iseg)%v(1)%x
     dy = front(iseg)%v(2)%y-front(iseg)%v(1)%y
!- for 3d get 2 edge vectors :
!    then cross-product to get the normal unit vector and area :-)
!    norm() = (/ dx3*dy2-dx2*dy3, dx1*dy3-dx3*dy1,dx2*dy1-dx1*dy2 /)
     len = sqrt( dx*dx + dy*dy)              !- cf a 3d area.
     if (len.lt.1.e-20) stop 'zero edge length detected'
     front(iseg)%len = len

!- really for a line the normal is the dot-product of (dx,dy,0) with (0,0,1)
!- *then* scale to a unit vector.
     front(iseg)%dx = -dy/front(iseg)%len               !- normal vector
     front(iseg)%dy =  dx/front(iseg)%len

!--- point to the adjoining segments ---
!- note node#1 is on the left, but toucher #1 is on the right
!  (this makes proper sense in 3d)
     front(iseg)%p(1)  = iright  
     front(iseg)%p(2)  = ileft
!--- update them too (only if not 'hanging') --
     if (ileft>0)  front(ileft)%p(1) = iseg   
     if (iright>0) front(iright)%p(2)= iseg   
   end subroutine put_seg

!-----------------------------------------------------------------------
    subroutine delete_seg (iseg)
!
!    Zaps the given segment, and updates the pointer
!    to make this 'the first available hole'.
!     Dan Kidger 16-3-97

      integer :: iseg
!      type (point) :: pnul = (0.,0., 0)
!      call put_seg (iseg, pnul,pnul, -ihole,0, 0,0)    !- but nul L/R pointers?

      if (iseg==lfront) then        !-- can just shrink the list length
        lfront = lfront-1
      else
        front(iseg)%iel = -ihole    !- point to the next hole
        front(iseg)%v(1) = point(0.,0., 0)  !(optional) clear the record
        front(iseg)%v(2) = point(0.,0., 0)
      endif                       
      ihole = iseg                  !- current hole is here
    end subroutine delete_seg

!-----------------------------------------------------------------------
   function get_next_seg (iseg_old,idir) result(iseg)
!
!    Gets the 'next' segment 
!      idir = -1,0,1 for left,auto,right segemnt
!     (actualy 3 toucher in 3d so use '2' too ?
!     I need someway of spoting when we have looped all the front.
!     either:
!       1: count the front size before we start so a finite loop.
!       2: mark segs as we parse them so skip if 'seen alrady'

!    for 3d:
!      auto is the only appropriate way
!      we need a stack
!      init with the 3 adjacents

!      if stack = empty : exit with iseg=0
!      get a new seg off the top of the stack and go to it
!      update the stack:
!        loop the 2 (of the 3) possible edges 
!          if toucher is 'free' then push onto the top of the stack, else skip
!
!
!      to go 'alternately', we need to use the 'oldest' members of the stack
!      first. ie.
!       push_stack:
!      store with each item on the stack its distance from the
!      'origin' seg. To 'push' a toucher, need to insert into the list
!      at the 'correct' point:
!      so:
!      assume at the end: but scan back if dist of top is < me
!      ie. dist=lowest is at the stack front(iseg) so always take this one
!      ( can leave it to pop_stack to return 'stack empty' )
!         init_stack:
!      given a base segment, choose a new unique number (use old++ ?)
!      mark the facet as 'done'
!      loop the 3 touchers and add to the stack
!      init could just push a single face onto the stack ? yes.
!

!    To count the size of the front:
!      pick any segment : 2/3 touchers on the stack & set idist=0
!      init the stack (and so give this sweep a 'unique' number
!        loop the 'get_next_seg' until returned is iseg = 0 (ifail)
!      done.
!     (cf a method to compute the toal area/volume of the front.
!     so we can check that is it is not diverging!

     integer :: idir, iseg, iseg_old
     integer :: iflip=1

     iseg = iseg_old     !- start from the last point
     select case (idir)
     case (-1)           !- to the left
       iseg  = front(iseg)%p(1)

     case (1)            !- to the right
       iseg  = front(iseg)%p(2)

     case (0)            !- auto
       if (iflip == -1) then    
         iseg  = front(iseg)%p(1)
       else
         iseg  = front(iseg)%p(2)
       endif 
       iflip = -iflip      !- ready for next time
     end select

     return
   end function get_next_seg 

!-----------------------------------------------------------------------
    function front_size_2d (ibase) result (isize)
!
!    Calcuates the number of segemnts in the current 'ring'
!     Simplex method - only for a 2d 'lines'
!     Dan Kidger 16-3-97
!
      integer,intent(in) :: ibase
      integer :: isize,iseg,iseg2

      iseg= ibase
      do isize=1,9999
        iseg2 = get_next_seg (iseg,1) 
        iseg = iseg2
        if (iseg == ibase) return
      enddo
      stop 'Error in FRONT_SIZE'
    end function front_size_2d

!-----------------------------------------------------------------------
    function no_of_fronts () result (nfronts)
!
!    Calcuates the number of seperate fronts that currently exist
!    (there is only only front, unless it bisects)
!       Dan Kidger 16-3-97
!    Maybe I should return pointers to a segment on the 'longest' ring
!    and to one on the shortest. Then we can 'tidy up loose ends' faster
!    or even parallelise the process? 
!
     implicit none
     integer :: nfronts
     integer:: isize,ibase,imark,idepth

     nfronts=0             !- assume none yet
     imark = -1            !- duumy to trip on the first pass
     do ibase=1,lfront     !- loop all possible base segments
       if (front(ibase)%mark == imark) cycle  !- already been done
       isize=front_size (ibase,imark,idepth)
       nfronts = nfronts+1
     enddo
     end function no_of_fronts

!-----------------------------------------------------------------------
    function front_size (ibase,imark,idepth) result (isize)
!
!    Calcuates the number of segemnts in the current 'ring'
!      it does this by maintaining a FIFO stack of available edges.
!     Dan Kidger 16-3-97
!

      integer, intent(in)  :: ibase   !- starting segment
      integer, intent(out) :: imark   !- the mark it was given
      integer, intent(out) :: idepth  !- max 'depth' from base segment
      integer :: iseg,isize,ic

!---------- loop forever looking for segments ---------
      do ic=1,99999
        call next_seg (ibase,ic, iseg,imark,idepth)
        if (iseg==-1) exit          !-all done
! how do I plot (in a different module) form this module ?
!        call plot_edge (front(iseg)%v(1),front(iseg)%v(2),imark)
      enddo   
      isize = ic
      return
    end function front_size

!-----------------------------------------------------------------------
    subroutine next_seg (ibase,ic, iseg,imark,idepth) 
!
!    Returns the next segment in the 'sweeping' of the front
!      it does this by maintaining a FIFO stack of available edges.
!     Dan Kidger 17-3-97
!
      integer,intent(in) :: ibase     !- starting 'base' segment
      integer,intent(in) :: ic        !- =1 on the first pass
      integer, intent(out) :: iseg    !- the returned segment
      integer, intent(out) :: imark   !- the mark it was given
      integer, intent(out) :: idepth  !- 'depth' from base segment
      integer :: iseg2,isize,level,i
      integer :: ipush     !- current stack end
      integer :: ipop      !- current stack base

      if (ic==1) then
        ipush=0     !- clear stack 
        ipop=1      !- point to take items from.
        call push_seg (ibase,1)          !- save as level 1(=origin)
        imark = get_unique_mark()        !- what we will label them with
      endif

!     print*,'ipop=',ipop,' ipush=',ipush
   1  continue                   !- loop back point
      if (ipop>ipush) then           !- stack is empty
         iseg=-1
         return
      endif
      call pop_seg (iseg,level)      !- get the next segment
      if (front(iseg)%mark ==imark) goto 1     !- already been done

!-- add this segment to the count.
      front(iseg)%mark = imark
      idepth= level
!     write(*,'(i4,a)',advance='no') iseg,' ->'
!-- loop and push the adjacent segments onto the stack.
      do i=1,2 !- 2 daughters (3 in 3d)
        iseg2 = front(iseg)%p(i)             !- the toucher
        if (front(iseg2)%mark==imark) cycle  !- already been done
        call push_seg (iseg2,level+1)        !- and save
      enddo    
      return

    contains

!----------------------------------------------
    function get_unique_mark () result (imark)
!
!   Simply returns a unique number
!     eg. when marking a closed 'shell' of segments.
!
      implicit none
      integer :: ibase=1000
      integer :: imark
      ibase = ibase+1
      imark = ibase
    end function get_unique_mark

!---------------------------------------------
      subroutine push_seg (iseg,level)
!
!     Adds a pair to the FIFO stack
!
        implicit none
        integer,intent(in) :: iseg,level
        ipush = ipush+1
        if (ipush > max_stack) stop ' stack overfull'
        stack(1,ipush) = iseg
        stack(2,ipush) = level
!       print*,'pushed, iseg=',iseg,' level=',level
      end subroutine push_seg

!---------------------------------------------
      subroutine pop_seg (iseg,level)
!
!     Returns a pair from the *Bottom* of the FIFO stack
!
        implicit none
        integer,intent(out) :: iseg,level
        iseg = stack(1,ipop)
        level = stack(2,ipop)
        ipop = ipop + 1
        if (ipop>=10) &
        call tidy_stack
      end subroutine pop_seg

!---------------------------------------------
      subroutine tidy_stack ()
!
!     Shuffles the stack down to close up the holes 
!      (so ipop=1 again)
!
        implicit none
        integer :: i
        do i=ipop,ipush
          ipush = i-ipop+1
          stack(1,ipush) = stack(1,i)
          stack(2,ipush) = stack(2,i)
        enddo
        ipop = 1
      end subroutine tidy_stack

!======
    end subroutine next_seg
!======
!-----------------------------------------------------------------------
!    subroutine find_node_in_front (inode, ibase,iseg,idist)
!
!   Scans the front recursively, until we find the given node.
!    returns the sgement # and the 'depth' from the parent
!
!   note also we have a need to parse the nodes in the front
!    eg. to find those nodes within a certain distance from 'C'
!
!   if idist=0 then inode is on ibase - this shouldn't happen
!   if idist=1 then inode is one-away - so collapse these 2 segs
!   if idist>2 then the ring will bisect
!  (if idist=2, then we form a double triangle in 2d.)
!   if dist=-1, then inode is NOT in this ring.
!
!    integer, intent(in) :: inode        ! given node
!    integer, intent(in) :: ibase        ! starting segment
!    integer, intent(out):: iseg,idist   ! returned seg# and its 'distance'

!    idist = -1     !- assume not found
!    end subroutine find_node_in_front

!-----------------------------------------------------------------------
end module segments

