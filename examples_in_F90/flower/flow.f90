PROGRAM FLOW_NET_EXAMPLE
!
!
!
  IMPLICIT NONE
  TYPE ELEM
    INTEGER :: N(4)
    INTEGER :: IMAT
  END TYPE ELEM


  INTEGER NDIM,NN,I, NEL,IEL, INODE,ID
  REAL, ALLOCATABLE       :: XY(:,:)  &              ! Nodal coords
                           , HEADS(:) &              ! Head at each node
                           , FLOWS(:)                ! Flow-rate at each node
  TYPE (ELEM),ALLOCATABLE :: NUMS(:)                 ! elements
  REAL                    :: PERM, KP(4,4)           ! flow-matrix
  INTEGER                 :: N_INFLOW, N_OUTFLOW     ! nodes where water
  INTEGER, ALLOCATABLE    :: INFLOW(:), OUTFLOW(:)   ! flows in or out

!------------------------ read-in the mesh -------------------------------
  OPEN (10,FILE='wall2.pl',ACTION='READ')
! OPEN (10,FILE='squre.pl',ACTION='READ')

  READ (10,*) NDIM             !- dummy                   !
  READ (10,*) NN
  ALLOCATE (XY(2,NN))
  DO I=1,NN
    READ (10,*) INODE ,XY(1,INODE) ,XY(2,INODE) 
  ENDDO
  PRINT*, NN,' Nodes read in'

  READ (10,*) NEL
  ALLOCATE (NUMS(NEL))
  DO IEL=1,NEL
!   READ (10,*) IEL ,NOD, NUMS(IEL)%N , NUMS(IEL)%IMAT
    READ (10,*) ID ,ID, NUMS(IEL)
  ENDDO
  PRINT*, NEL,' Elements read in'
  CLOSE (10)

!----------------- create the permeability matrix ------------------
  OPEN (11,FILE='wall2.in',ACTION='READ',STATUS='OLD')
! OPEN (11,FILE='sqperm.dat',ACTION='READ',STATUS='OLD')
  READ (11,*) PERM
  KP (:,1) = (/ 4., -1., -2., -1. /)
  KP (:,2) = (/-1.,  4., -1., -2. /)
  KP (:,3) = (/-2., -1.,  4., -1. /)
  KP (:,4) = (/-1., -2., -1.,  4. /)
  KP = KP *PERM / 6.

!----------- read in the INFLOW and OUTFLOW lists of nodes ----------
  READ (11,*) N_INFLOW
    ALLOCATE (INFLOW(N_INFLOW))
    READ (11,*) INFLOW(:)
  READ (11,*) N_OUTFLOW
    ALLOCATE (OUTFLOW(N_OUTFLOW))
    READ (11,*) OUTFLOW(:)
  CLOSE (11)

!----------------------- solve all the equations --------------------
  ALLOCATE ( HEADS(NN), FLOWS(NN) )
!  CALL SYSTEM_CLOCK (ITICKS, N_PER_SEC,ITICK_MAX)        ! <--- Yuk!
!  HEADS(:) = 100.   ! now done insside the solver
   CALL SOLVE_FLOWNET (KP,NUMS, HEADS, FLOWS, INFLOW,OUTFLOW)

!  FLOWS(:) = 123.   ! now done inside the solver
  CALL CALC_FLOWS (KP, NUMS, HEADS, FLOWS)


! PRINT*,' Solution time ='
! PRINT*,' Number of iterations was ='

  PRINT*,' Total Inflow  of water =', &
    SUM (FLOWS( INFLOW(:)))*60.*60.*24., 'm^3/day'
  PRINT*,' Total Outflow of water =', &
    SUM (FLOWS(OUTFLOW(:)))*60.*60.*24., 'm^3/day'

!----------- plot-out the mesh -----------
!  CALL PLOT_FLOWNET ()

!============ write raw heads out ============
   OPEN (92,FILE='heads.out',action='WRITE')
   WRITE (92,'(I20)')     NN
   DO I=1,NN
     WRITE (92,'(I5,G13.4)')     I,  HEADS(I)
   ENDDO
   CLOSE (92)

!============ write everything out in DANPLOT format ============
   OPEN (91,FILE='flownet.out',action='WRITE')
   WRITE (91,'(/A/A/A/A/, (i5,2G13.4))')   &
   '#        Output from my FLOW-NET Program ', &
   '# This file format is in DANPLOT post-processor format' ,&
   '*TWO_DIMENSIONAL' ,&
   '*NODES', (i,XY(:,i),i=1,nn)
   WRITE (91,'(/A/, (i7,3i2,4i7,i2))')   &
   '*ELEMENTS', (iel,2,4,1, NUMS(iel),iel=1,nel)
   WRITE (91,'(/A/A/, (i5,2G13.4))')   &
   '*DISPLACEMENTS', &
   '# Actually Column 1=heads, Column 2= net inflow/outflows', &
    (i,heads(i),flows(i),i=1,nn)
    CLOSE (91)

    STOP

CONTAINS

  INCLUDE "solver.f90"

END PROGRAM FLOW_NET_EXAMPLE

