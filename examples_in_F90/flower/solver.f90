!   module tttt
!     type elem
!       integer :: n(4),imat
!     end type elem
!   contains
!----------------------------------------------------------------------
   SUBROUTINE SOLVE_FLOWNET (KP,NUMS, HEADS, FLOWS, INFLOW,OUTFLOW)
!
!  This computes the pressure head over the given nodes
!  INFLOW () and OUTFLOW () need to be set the list of nodes where water
!  enters/leaves the mesh.
!                                       by  Dr. D.J.Kidger
!   Version 1.0  30-11-95
!
    IMPLICIT NONE
    TYPE (ELEM) :: NUMS(:)                           !- the elemnts
    REAL        :: HEADS(:), FLOWS(:), KP(:,:)       !- nodal heads and flows
    INTEGER     :: INFLOW(:), OUTFLOW(:)             !- 'nailed' nodes

!--------- local variables and arrays ---------
    REAL   ,DIMENSION (SIZE(HEADS))  :: P,R,U,D,XNEW    !- workspace vectors
    REAL    :: BIG_SPRING, UP,UP2,DOWN,BIG,BIGGEST,TOL,ALPHA,BETA
    INTEGER :: ITERS

    HEADS(:) = 23.12345                     !- inital guess at the solution
    HEADS(INFLOW)  = 100.                   !- prescribed heads
    HEADS(OUTFLOW) =  10.                   !-      "      "

!---- handle the 'Big-spring' used to 'nail' in Inflow and Outflow heads
    BIG_SPRING = MAXVAL (KP)* 1.E4 
    FLOWS(:) = 0.                           !- net inflow = zero everywhere
    FLOWS(INFLOW)  = HEADS(INFLOW)  * BIG_SPRING    !- Bruce Iron's method of
    FLOWS(OUTFLOW) = HEADS(OUTFLOW) * BIG_SPRING    !- 'Big Springs'         

!   HEADS(:) = 0.001                    !- inital guess at the solution
    HEADS(:) = 0.0

    R(:) = FLOWS(:)                         !- inital 'residuals'
    D(:) = R(:)
    P(:) = D(:)

!------------------------ loop the iterations --------------------------
      TOL = 10.**(-5)              !- adjust as necessary

      Iterations : DO ITERS = 1, SIZE(HEADS)

        CALL CALC_FLOWS (KP, NUMS, P,U)
        U(INFLOW)  = U(INFLOW)  + BIG_SPRING * P(INFLOW)  !- method of
        U(OUTFLOW) = U(OUTFLOW) + BIG_SPRING * P(OUTFLOW) !- 'Big Springs' 
        UP   = DOT_PRODUCT (R,D)
        DOWN = DOT_PRODUCT (P,U)
        ALPHA = UP/DOWN                           !- à = R.D / P.U
        XNEW(:) = HEADS(:) + ALPHA * P(:)
        R(:)    = R(:)     - ALPHA * U(:)
        D(:)    = R(:)
        UP2 = DOT_PRODUCT(R,D)
        BETA = UP2/UP                             !- á = Rnew.Dnew / P.U
        P(:) = D(:) + BETA * P(:)
!        print*,'up=',up,' up2=',up2,' down=',down
!        print*,' alpha=',alpha,' beta=',beta

!--------- now check convergence ----------
        BIG = MAXVAL(ABS(XNEW(:) ))
        BIGGEST = MAXVAL (ABS(XNEW(:) - HEADS(:))) / BIG
!      BIG = 0.
!      BIGGEST = 0.
!      DO I=1,SIZE(HEADS)
!        BIG     = MAX (BIG     ,ABS(HEADS(I)))
!        BIGGEST = MAX (BIGGEST, ABS(HEADS(I)-XNEW(I)))
!      ENDDO
!      BIGGEST = BIGGEST / BIG

        WRITE (*,'(I5,A,F15.2,A,F15.8,F15.1)')   &
          ITERS,': BIG=',BIG,' change =',BIGGEST , BIGGEST /TOL 

        HEADS(:) = XNEW(:)                      !- update the solution
        IF (BIGGEST < TOL) EXIT Iterations
      ENDDO Iterations

   END SUBROUTINE SOLVE_FLOWNET

!----------------------------------------------------------------------
  SUBROUTINE CALC_FLOWS (KP, NUMS, HEADS, FLOWS)
!
!  This calculates the flow-rates (FLOWS) corresponding to the given 
!  presure heads (HEADS).
!  KP () is the flow-matrix for a single element
!  NUMS() is the set of elements
!     Dan Kidger 3-12-95

    IMPLICIT NONE
    TYPE (ELEM) :: NUMS(:)
    REAL        :: HEADS(:), FLOWS(:), KP(:,:)
    INTEGER,DIMENSION(SIZE(KP,1))   :: LIST 
    INTEGER :: IEL

    FLOWS (:) = 0.
    DO IEL = 1, SIZE (NUMS)
      LIST = NUMS(IEL)%N
!     PRINT*,' element #', IEL,' nodes are :', LIST
      FLOWS (LIST) = FLOWS (LIST) + MATMUL ( KP , HEADS(LIST) )
    ENDDO
  END SUBROUTINE CALC_FLOWS
!----------------------------------------------------------------------

!end module tttt
