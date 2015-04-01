
PROGRAM ERIC_THE_HAMSTER
!
!  This is a template for your program
!   Change EVERYTHING including the comments! :-)
!
!
  IMPLICIT NONE

                                                     ! Nodal coords
                                                     ! Head at each node
                                                     ! Flow-rate at each node
                                                     ! elements
                                                     ! flow-matrix
                                                     ! nodes where water
                                                     ! flows in or out

!------------------------ read-in the mesh -------------------------------
  OPEN (10,FILE='wall2.pl',ACTION='READ')
  READ (10,*) NDIM             !- dummy                   !
!-- etc.

  PRINT*, NN,' Nodes read in'

!-- etc.

  PRINT*, NEL,' Elements read in'
  CLOSE (10)

!----------------- create the permeability matrix ------------------
  OPEN (11,FILE=' ...
  READ (11,*) PERM
  KP (:,1) = (/ 4., -1., -2., -1. /)   !- dont forget to PERM and divide by 6

!----------- read in the INFLOW and OUTFLOW lists of nodes ----------
! dont forget to :    ALLOCATE (INFLOW and OUTFLOW

!----------------------- solve all the equations --------------------
! dont forget to:  ALLOCATE ( HEADS and FLOWS()

!------------- back-calculate the flow-rates ----------------------
  CALL CALC_FLOWS (KP, NUMS, HEADS, FLOWS)


  PRINT*,' Total Inflow  of water =', &        !- ie loop INFLOW 


!============ write raw heads out ============
! ... use something like this for the plotter program
   OPEN (92,FILE='heads.out',action='WRITE')
   WRITE (92,'(I20)')     NN
   DO I=1,NN
     WRITE (92,'(I5,G13.4)')     I,  HEADS(I)
   ENDDO
   CLOSE (92)


!============ write everything out in DANPLOT format ============
!   If you dont understand this section then just delete it !
!
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

!- include the library here.

END PROGRAM ERIC_THE_HAMSTER

