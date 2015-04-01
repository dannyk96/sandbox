      PROGRAM QUICK_CONTOUR
C
C     This will do a 'quick' 2D contour plot of a mesh to a Postscript file.
c     .. like QP_PS.FOR but  reads a sset of nodal values from an 
c     auxiluary file
C     Simply type "QP_PS" to run
C     Then type "COPY PLOTFILE.PS PRN" to print.
c
C       DJK 31-10-95
C
C
      PARAMETER (IGC=8000,INUMS = 8)
      REAL        GC (2,IGC)                  ! Nodal coordinates 
     &         ,VALS (IGC)                    ! Nodal values to contour
      INTEGER   NUMS (INUMS,IGC)              ! Nodal coords
     &         ,NENS (IGC)                    ! #nodes per element
      PARAMETER (ISC=9)
      REAL SC(ISC,2), COLS(ISC)

      CHARACTER FILE*80, FILE2*80, TEXT*80       !,CMNAM@*80
C---------------------- open the data file------------------------------
      WRITE (*,'(A//T12,A/,T12,a//a)')
     &      CHAR(27)//'[2J'
     & , char(27)//'[1;32m'//'CONT_PS Postscript Contour Plotter' 
     & , char(27)//'[1;33m'//'==================================' 
     & , char(27)//'[1;37m'//'What is the name of your mesh file?'
      READ(*,'(A)') FILE
      OPEN (UNIT=7, FILE=FILE, STATUS='OLD',IOSTAT=IOSTAT)
      IF (IOSTAT.NE.0) THEN
        PRINT*,' *** GIven .PL file does not exist!      <EXITING>'
        STOP
      ENDIF

C------------------------ read the mesh data ---------------------------
      READ(7,*) NODOF
      READ(7,*) NN
      DO I=1,NN
        READ(7,*) ID,(GC(J,ID),J=1,2)
      ENDDO
      READ(7,*) NEL,(ID,NENS(I),(NUMS(J,I),J=1,NENS(I)),ID,I=1,NEL)
      CLOSE (7)

C---------------------- read the contour data --------------------------
c     if (1.eq.0) then
      WRITE (*,'(/T12,A)') char(27)//'[1;37m'//
     & 'What is the name of your contour output file?'
      READ(*,'(A)') FILE2
      OPEN (UNIT=7, FILE=FILE2, STATUS='OLD',IOSTAT=IOSTAT)
      IF (IOSTAT.NE.0) THEN
        PRINT*,' *** GIven .PL file does not exist!      <EXITING>'
        STOP
      ENDIF
      READ(7,*) NN2
      IF (NN2.ne.NN) THEN
        PRINT*,'*** Mesh file has', NN,' nodes but VALUES file has'
     &    ,NN2,'nodes   <EXITING>'
        STOP
      ENDIF

      DO I=1,NN
        READ(7,*) ID, VALS(ID)
      ENDDO
      CLOSE (7)
c     endif
c---------------- # of contours ----------
      WRITE (*,'(/T12,A)') char(27)//'[1;37m'//
     & 'How many Contour bands do you want (eg. 20)'
      READ*,N_CONTS

c-------------------- go into graphics mode ----------------------------
      RESX = 8.*72.     !- A4 paper size ?
      RESY = 11.*72.
      AR = RESX / RESY

C--------------------- maxima and minima -------------------------------
      XMIN =  1.E37            !- or simply MAXVAL and MINVAL in F90
      XMAX = -1.E37
      YMIN =  1.E37
      YMAX = -1.E37
      CMIN =  1.E37
      CMAX = -1.E37
      DO I = 1,NN
        XMAX = MAX (XMAX,GC(1,I))
        XMIN = MIN (XMIN,GC(1,I))
        YMAX = MAX (YMAX,GC(2,I))
        YMIN = MIN (YMIN,GC(2,I))
        CMAX = MAX (CMAX,VALS(I))
        CMIN = MIN (CMIN,VALS(I))
      ENDDO

C-------------------- write the Postscript headers ---------------------
c.. I think I prefer landscape for contour plots.
      OPEN (20,FILE='PLOTFILE.PS')
      WRITE(20,'(A)')  '%!PS-Adobe-2.0 EPSF-1.2'
      WRITE(20,'(4A)') '%%Document title: ',FILE
      WRITE(20,'(A)') 
C    &    ' gsave',                                  !- old transformation
     &    ' 0 setgray  .31 setlinewidth'             !- linewidth (was .3)
      CALL WRITE_PS_MACROS (20)

      WRITE(20,'(A)')
c    &  ' .5  setgray clippath fill'
     &  ' 0.  20 translate 1.3 1.3 scale -90 rotate'
     & ,' 72 8.5 mul neg 0 translate'

C----------------------- 0: TItles -------------------------
      WRITE (20,'(A)') '0 setgray'
      WRITE (20,'(f9.2,a)') 14., ' /Helvetica sf '
      write (text,'(a,a,a)')
     &    'CONT_PS contour plotter, FILE=',FILE(1:LENG(FILE))
      WRITE (20,'(3A,2G13.4,A)')  
     &    ' (',TEXT(1:LENG(TEXT)),') ', resx/2., 72./6.*4  , 'dtc'

      WRITE (20,'(f9.2,a)') 8., ' /Helvetica sf '
      WRITE (TEXT,'(A,i5,A,I5)')  
     &     'NN=',NN,' Nel=',NEL
      WRITE (20,'(3A,2G13.4,A)')  
     &    ' (',TEXT(1:LENG(TEXT)),') ',resx/2., 72./6.*3   , 'dtc'
      WRITE (20,'(f9.2,a)') 6., ' /Helvetica sf '
      WRITE (TEXT,'(A)')  'by Dr. Kidger 21-11-95'
      WRITE (20,'(3A,2G13.4,A)')  
     &    ' (',TEXT(1:LENG(TEXT)),') ',resx, 72./6.*3   , 'dtr'


C---------------------- normalize the data -----------------------------
C.. so that the coords are now in 'pixel' coords.
      FACT = 0.15         !- shrink factor
      R = (1.-2.*FACT)    !- resulting size
c      N_CONTS = 20        !- # of contour bands to draw (typed input?)
c      n_conts = 3

      DATAX = MAX ((XMAX - XMIN),(YMAX - YMIN)*AR)   !- scale factor
      DO I = 1,NN
        GC(1,I) =        RESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
        GC(2,I) =        RESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 
        VALS(I) = (VALS(I)-CMIN) / (CMAX-CMIN) * N_CONTS
      ENDDO 

C----------------------- plot the contours ---------------------------
c.. this will need my GOURAUD interpolater!
      PRINT*,'<> Plotting the element contours'
      DO IEL = 1,NEL
        PRINT*, IEL,'/',NEL, CHAR(27)//'[A'
        WRITE (20,'(A,i5)') '% element No.',IEL
        NEN = NENS(IEL)
        DO I=1,NEN
          i1 = NUMS(I,IEL)
          SC(I,1) = GC (1,I1)
          SC(I,2) = GC (2,I1)
          COLS(I) = VALS (I1)
        ENDDO
C       CALL GOURAUD (SC,ISC,NEN, COLS, N_CONTS,1003,'fill')
        CALL GOURAUD (SC,ISC,NEN, COLS, N_CONTS,1002,'fill')
      ENDDO

C----------------------- plot the mesh-lines ---------------------------
      PRINT*,'<> Plotting the element edges'
      WRITE (20,'(A)')   ' .0 setgray '
      DO IEL = 1,NEL
        NEN = NENS(IEL)
c        WRITE (20,'(10F8.2)') 
c     &   ( GC(1,NUMS(J,IEL)), GC(2,NUMS(J,IEL)), j=1,nen)
c       WRITE (20,'(i5,A)')   nen-1, ' .97 setgray fp'
        WRITE (20,'(10F8.2)') 
     &   ( GC(1,NUMS(J,IEL)), GC(2,NUMS(J,IEL)), j=1,nen)
        WRITE (20,'(I2,A)')   nen-1,  ' dp'
      ENDDO

      WRITE (20,'(a/a)') '%%Trailer', 'grestore showpage'
      CLOSE (20)
      PRINT*,'<> "plotfile.ps" successfully created'
      PRINT*,' '
      PRINT*,'   Type   PSVIEW PLOTFILE.PS         to preview onscreen'
      PRINT*,'   Type   COPY PLOTFILE.PS PRN       to send to printer'


      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_PS_MACROS (IO)
C
C     This simply writes out the Postscript macros as used by DR_PS ()
C      eg. from DANPLOT and DANGINO (+DANSLIP)
C
      WRITE(IO,'(A)') '%%BeginProlog' 
     +,'/sc { setrgbcolor } def' 
     +,'/sf { findfont exch scalefont setfont} def' 
     +,'/Helv { /Helvetica sf} def' 
     +,'/dl { newpath 3 1 roll moveto {lineto} repeat stroke } def' 
     +,'/dp { newpath 3 1 roll moveto {lineto} repeat closepath'//
     +    ' stroke } def' 
     +,'/fp { newpath 3 1 roll moveto {lineto} repeat closepath'//
     +    ' fill } def' 
     +,'/dc { newpath 0 360 arc stroke } def'             !- but diameter ??
c    +,'/fc { newpath 0 360 arc fill } def'
     +,'/fc { newpath pop 4 2 roll 0 360 arc fill } def'

c    +,'/slen {stringwidth pop 0 exch sub 0 } def'        !- -ve text-length
     +,'/dt  { moveto show } def'                    !- text-LH
     +,'/dtc { moveto dup stringwidth pop 2 div'//   !- text-centre
     +    ' 0 exch sub 0 rmoveto show } def'
     +,'/dtr { moveto dup stringwidth pop'//         !- text-right
     +    ' 0 exch sub 0 rmoveto show } def'
     &   ,'%%EndProlog'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DR_PS (TEXT,X,Y,N,ICOL, IOP)
C
C     This writes PostScript Output to unit 55 for lines,fill_areas etc.
C     IOP = 1 un-closed line
C           2    closed line = polygon
C           5   filled polygon  
c       (  10   filled circle  )
C          20   text string   (auto font & scale)
c          21/22 = centred/LHjust text
c..... written 29-11-93 
c      13-8-94 only do setrgbcolor if it has changed

      SAVE
      REAL X(*),Y(*)
      INTEGER N, ICOL,IOP, ICOL_OLD
      REAL*8 RANDOM
      COMMON /PALETTE/PAL
      REAL PAL(3,0:255)
      CHARACTER FORMAT*50, ACTION*5, TEXT*(*) !*80
      LOGICAL PALS
      DATA ICOL_OLD/-99/     !- remember the 'last' colour
      DATA TXT_SIZE/-1./    !- remember the 'last' text size
      DATA PALS/.FALSE./

      IF (.NOT.PALS) THEN
        DO I=2,255
          PAL(1,I) = RANDOM ()
          PAL(2,I) = RANDOM ()
          PAL(3,I) = RANDOM ()
        ENDDO
        PALS=.TRUE.
      ENDIF

      IF (ICOL.NE.ICOL_OLD) THEN
        IF (ICOL.EQ.0) THEN
          WRITE (20,'(a)')  '1 1 1 sc'
        ELSEIF (ICOL.EQ.1) THEN
          WRITE (20,'(a)')  '0 0 0 sc'
        ELSE
          WRITE (20,'(3f5.2,a)') (PAL(J,ICOL),J=1,3), ' sc'
        ENDIF
      ENDIF
      ICOL_OLD = ICOL

      IF (IOP.EQ. 1) THEN
         ACTION = ' dp'        !/* draw-polygon */
      ELSEIF (IOP.EQ. 2) THEN
         ACTION = ' fp'        !/* fill-polygon */
      ELSEIF (IOP.EQ. 3) THEN
         ACTION = ' dl'        !/* draw-line */
      ELSEIF (IOP.EQ.10) THEN
         ACTION = ' fc'        !/* fill-circle */
      ELSEIF (IOP.EQ.20) THEN
         ACTION = ' dt'        !/* draw-text */
      ELSEIF (IOP.EQ.21) THEN
         ACTION = ' dtc'       !/* draw-text-centered */
      ELSEIF (IOP.EQ.22) THEN
         ACTION = ' dtr'       !/* draw-text-RH-just */
      ELSE
         STOP 'unknown opcode in DR_PS'
      ENDIF
c--------------------- handle text strings -----------------------------
c.. 7-4-95 factor-down by 72. (into points) !
c.. nice to split so we define the font only when it changes (cf pencol)
      IF (IOP.ge.20.and.iop.le.29) THEN
        IF (ABS(X(2)-TXT_SIZE) .GT. 1.E-4) THEN
          TXT_SIZE = X(2)   !/72.
          WRITE (20,'(f9.2,a)') X(2), ' /Helvetica sf '
        ENDIF
        WRITE (20,'(3A,2f9.2,A)')       !-x2=font size !
c    +    ' (',TEXT(1:LENG(TEXT)),') ',X(1),Y(1), X(2)/72., ACTION
     &    ' (',TEXT(1:LENG(TEXT)),') ',X(1),Y(1)   , ACTION

c----------------- polygons (use relative coords :-) -------------------
c.. note that MAX_DAT is the max number of tokens on a line
      ELSEIF (ACTION.eq.' dp'.or.ACTION.eq.' fp'
     &    .or.ACTION.eq.' dl') THEN
        N_LINE = 8                   !- max # per line
        N_REP = (2*N-1)/N_LINE       !- number of repeated lines
        N_REM = 2*N-N_REP*N_LINE     !- number on the last line

        IF (N_REP.NE.0) THEN
          WRITE (FORMAT,'(A,I2,A,I2,A,I2,A)')
     &     '(', N_REP,'(',N_LINE,'F9.2/),',   N_REM ,'F9.2, I4,A)'
        ELSE
          WRITE (FORMAT,'(A,I2,A)')
     &    '(',                                N_REM ,'F9.2, I4,A)'
        ENDIF
        WRITE (20,FORMAT) (X(I),Y(I),I=1,N-1), X(n),Y(n)
     &         , N-1, ACTION

c----------------------- all other primitives --------------------------
c.. note that MAX_DAT is the max number of tokens on a line
      ELSE
        N_LINE = 8                   !- max # per line
        N_REP = (2*N-1)/N_LINE       !- number of repeated lines
        N_REM = 2*N-N_REP*N_LINE     !- number on the last line

        IF (N_REP.NE.0) THEN
          WRITE (FORMAT,'(A,I2,A,I2,A,I2,A)')
     &     '(', N_REP,'(',N_LINE,'F9.2/),',   N_REM ,'F9.2, I4,A)'
        ELSE
          WRITE (FORMAT,'(A,I2,A)')
     &     '(',                                N_REM ,'F9.2, I4,A)'
        ENDIF
        WRITE (20,FORMAT) (X(I),Y(I),I=1,N), N-1, ACTION
      ENDIF
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE GOURAUD (SC,ISC,NPTS, COLS, N_CONTS,IOP,type)
C
C     .. should really be called GOURAUD_POLYGON ?
C     this shades a facet  
C     -- decomposing into triangles anchored at the first node
C        8nq's are a special case and are split into 6 tri's nicely
C
C     SC(ISC,*) are the (screen) coords of the 2D polygon of NPTS points
C     COLS(*)   are the values (colour-numbers) at each node 
C     N_CONTS   is used for 'cliping' the range
C     IOP      = the colour code (overloaded to give 'rainbow', 'zebra' etc.
C     TYPE      = 'fill' or 'line' = contour type
c
c     7-2-95  now if colour is constant, dont need to subdivide
c    -- better to add a centre node to *all* non-triangles
C
      REAL SC(ISC,*)  ,COLS(*)        !- given facet and nodal colours
     +  ,XP(3) ,YP(3) ,COL(3)         !- the abstracted triangles
      REAL X(50),Y(50)                !- for direct drawing
      CHARACTER TYPE*4

      INTEGER T8NQ(3,6)
      DATA T8NQ/1,2,8, 7,6,8, 6,8,2, 6,2,4, 2,3,4, 4,5,6/  !- 8nq :-)


      COL_MIN = COLS(1)
      COL_MAX = COLS(1)
      DO I=2, NPTS
         COL_MIN = MIN (COL_MIN, COLS(I))
        COL_MAX = MAX (COL_MAX, COLS(I))
      ENDDO
      ICOL_RANGE = INT(COL_MAX) - INT(COL_MIN)
      IF (ICOL_RANGE.EQ.0) THEN   !--- speed-up if all constant
        ICOL = COL_MIN + 1                !(why add 1?)
        IF (type.NE.'fill') RETURN   !- nothing to draw
C       CALL DRAW_POLY (SC,ISC,NPTS,ICOL,2)  !- a header to DR_PRIM
        CALL EXTRACT_POLY (SC,ISC,NPTS, X,Y)
        CALL GOURAUD_draw (X,Y,NPTS,ICOL, IOP,TYPE)
        RETURN
      ENDIF

      IF (IOP.eq.-1) THEN      !- if 'off' skip
c     .. no-op ...
      ELSEIF (IOP.eq.-2) THEN  !- if 'average' : colour as the mean value
        CC = 0.
        DO I=1,NPTS
          CC = CC + COLS(I)
        ENDDO

        ICOL = CC/NPTS
c       CALL DRAW_POLY (SC,ISC,NPTS,ICOL,2)  !- a header to DR_PRIM
        CALL EXTRACT_POLY (SC,ISC,NPTS, X,Y)
        CALL GOURAUD_draw (X,Y,NPTS,ICOL, IOP,TYPE)
        RETURN
      ELSE                     !-  ie '0'= 'black' etc. etc.

C--------------------------- loop the sub-triangles --------------------
        IF (NPTS.EQ.3) THEN   !- do trinagles directly
          DO I=1,3                
            XP (I) =   SC (I,1)     !- just abstract as 3 lists
            YP (I) =   SC (I,2)   
            COL(I) = COLS (I)
          ENDDO
          CALL GOURAUD_TRI (XP,YP, COL, N_CONTS,IOP,TYPE)
          RETURN
        ENDIF
c----------- method 2 : create a centre-node

        xp_ = 0.
        yp_ = 0.      !-  find the coord & colour of the centre.
        col_ = 0.

        DO I=1,npts
          xp_ = xp_ + sc(i,1)
          yp_ = yp_ + sc(i,2)
          col_ = col_ + cols(i)
        enddo


        XP (3) = xp_  /real(npts)      !- point #3 is always the centre
        YP (3) = yp_  /real(npts)  
        COL(3) = col_ /real(npts)
        DO IT = 1,NPTS              ! ie. NPTS sub-triangles
            i1 = it
            XP (1) =   SC (i1,1)     !- go clockwise :-)
            YP (1) =   SC (i1,2)   
            COL(1) = COLS (i1)
            i2 = mod(it,npts)+1
            XP (2) =   SC (i2,1)     !- go clockwise :-)
            YP (2) =   SC (i2,2)   
            COL(2) = COLS (i2)
          CALL GOURAUD_TRI (XP,YP, COL, N_CONTS,IOP,TYPE)
        ENDDO
        RETURN

c----------- method 1 : minimum number of triangles ----
C       NTRI = NPTS - 2               !-   # triangles  
c        DO IT = 0,NPTS-3
c          IF (NPTS.eq.8) THEN   ! if 8nq then dissect explicitely (9nq?)
c            DO I=1,3                  !- 5nq for 14nb's ? <- sub_facet
c              II = T8NQ(I,IT+1)       !- etc. ??
c              XP (I) =   SC (II,1)
c              YP (I) =   SC (II,2)   
c              COL(I) = COLS (II)
c            ENDDO

c          ELSE       !-- default is a set of triangles from node #1 -----
c            XP (1) =   SC (1,1)    !-cf creation of a central node.
c            YP (1) =   SC (1,2)
c            COL(1) = COLS (1)
c            DO I=2,3
c              XP (I)  =   SC (IT+I,1)
c              YP (I)  =   SC (IT+I,2)
c              COL(I)  = COLS (IT+I)
c            ENDDO
c          ENDIF  !- of 8nq/others

c          CALL GOURAUD_TRI (XP,YP, COL, N_CONTS,IOP,TYPE)
c        ENDDO    !- next triangle

      ENDIF   !- if not 'skipped' or 'averaged'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GOURAUD_TRI (XP,YP,COLS, N_CONTS,IOP,TYPE)
C
C     This contours the given triangle (filled/lines)
C     usu. called from GOURAUD
C     given the triangle in XP,YP with cols COLS
C     ITYPE =1 for filled, 2 for just lines
C     IOP is just passed to select the colours
C     values in COLS are skipped outside  0.->N_CONTS  <- check this
C
      REAL XP(3), YP(3), COLS(3) !- given triangle and nodal values
      REAL X(6),Y(6)             !- the polygon to draw
      INTEGER A,B,C              ! labels of the 3 points
      CHARACTER TYPE*4      !- ='fill' or 'line'
      

      BIS   (VAL,E,F) = MIN(MAX(0., (VAL-E)/(F-E+t)) ,1.)
      RATIO (VAL,G,H) = G + (H-G) * VAL            !- statement funcs
      T = 1.E-4                     !-   = a 'small' number

      A = 1                         !-  find :  'A' the largest value
      B = 2                         !           'B' the middle value
      C = 3                         !           'C' the smallest value

c.. maybe forget A B and C and just make X(1),Y(1),COLS(1) the *lowest*
c.. hence X(2).. and X(3)..

      IF (COLS(A).LT.COLS(C) ) CALL ISWAP (C,A)  !- bubble-sort
      IF (COLS(B).LT.COLS(C) ) CALL ISWAP (B,C)
      IF (COLS(A).LT.COLS(B) ) CALL ISWAP (A,B)

      if (cols(a).lt.cols(b).or.cols(b).lt.cols(c)) 
     & STOP '*** oops wrong order in Gouraud'
      I_LO = MAX (COLS(C),  0.)            !- the lowest contour 
      I_HI = MIN (COLS(A)+1,1.*N_CONTS)      !- the highest contour

C.. hmm I should be very careful about the preceding 2 lines
C--------------- loop from the MIN contour to the MAX contour ----------
c hmm maybe I should be looping ALL the contour values?
c (start-1 to finish+1 to make sure we catch ALL the fills
c cf NCONTS with CI.. ie one causes a calc of the other?

      N = 0                                !- count points per polygon
      N = N + 1
      X(N) =  XP(C)          !- record the first point
      Y(N) =  YP(C)

      DO I=I_LO,I_HI
        RI = REAL (I)

        IF (INT(RI)-1.EQ.INT(COLS(B))) THEN    !- add middle node if necessary
          N = N + 1
          X(N) = XP(B)
          Y(N) = YP(B)
        ENDIF
c                                   (  be careful if COLS(B)=COLS(C) !)
        N = N + 1
        IF (RI.LT.COLS(B)) THEN                  !- edge C -> B
          FAC  = BIS   (RI, COLS(C),COLS(B))
          X(N) = RATIO (FAC, XP(C), XP(B))
          Y(N) = RATIO (FAC, YP(C), YP(B))
        ELSE                                     !- edge B -> A 
          FAC  = BIS   (RI, COLS(B),COLS(A))
          X(N) = RATIO (FAC, XP(B), XP(A))
          Y(N) = RATIO (FAC, YP(B), YP(A))
        ENDIF

        N = N + 1                                !- 'long' edge A->C
        FAC  = BIS   (RI, COLS(C),COLS(A))
        X(N) = RATIO (FAC, XP(C), XP(A))
        Y(N) = RATIO (FAC, YP(C), YP(A))

c--------------- 'fill' and 'edge' the contour bands -------------------
c.. fills: on the first pass; if icol at A is less than the minimum.. then
c     this is the 'below' min band
c.. lines are always drawn between the 'last' two points

        IF (TYPE.EQ.'fill') then                 
          IF (i.eq.i_lo.and.cols(a).lt.0) then     !- on 1st pass
             CALL GOURAUD_draw (X,Y,N,I,IOP, TYPE) 
          ELSE
             CALL GOURAUD_draw (X,Y,N,I,IOP, TYPE)  !- rest-of
          ENDIF
        ELSEIF (TYPE.EQ.'line'.and.i.lt.i_hi) THEN    !- skip last time?
          CALL GOURAUD_draw (X(n-1),Y(n-1),2,I,IOP, TYPE) 
        ENDIF
        X(1) = X(N)       !- copy last 2 nodes to list base
        Y(1) = Y(N)       !- ready for the next contour band
        X(2) = X(N-1)
        Y(2) = Y(N-1)
        N = 2
      ENDDO

c---------------- finally finish off the last polygon ------------------
      N = N + 1
      X(N) =  XP(A)          !- record the first point
      Y(N) =  YP(A)
      IF (TYPE.EQ.'fill') then                 
        IF (cols(c).gt.N_CONTS) then
           CALL GOURAUD_draw (X,Y,N,I,IOP, TYPE) 
        ELSE
c          CALL GOURAUD_draw (X,Y,N,-1,IOP, TYPE) !- 'outside'
        ENDIF
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE ISWAP (I,J)
c
c     this just swaps the 2 integer arguements
c        .. daughter of GOURAUD
c
        K = I
        I = J
        J = K
      END

C----------------------------------------------------------------------
      SUBROUTINE EXTRACT_POLY (SC,ISC,NPTS, X,Y)
C
C     like DRAW_PRIM but just exatracts the polygon as 2 lists
C     it unpacks a 2D list of coords into X() and Y() 
C       .. called from DRAW_POLY and GOURAUD
C     SAVE                  !/ does this make it faster ?
      REAL SC(ISC,*)        ! x,y,(z) coords of the line/polygon
      REAL X(*), Y(*)
      DO I=1,NPTS
        X(I) = SC(I,1)
        Y(I) = SC(I,2)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GOURAUD_draw (X,Y,N,ICOL, IOP,TYPE)
C
C     This is a sub-part of 'GOURAUD' which simply draws the shapes
C     X(),Y() is the polgon of N points
C     ICOL is the colour code
C     IOP chooses which method to use (solid/zebra etc.)
C     IOP is the 'style' : black/coloured/zebra etc.
C     ITYPE =1 for filled and =2 for just the lines
C     hence ILF=2 for filled, 3 for just lines.
C     14-8-92 now IOP<1000 = solid, >1000 = 'multi' (eg +2 = all cols)

      REAL X(*), Y(*)
      CHARACTER TYPE*4      !- ='fill' or 'line'
      CHARACTER T*5        !- dummy for DR_PRIM
                      
      if (type.eq.'fill'.and.n.le.2) return   !- if a zero-area polygon


      IB = 2                !- offset into the colour table
      IB = 16  -1           !- hacked to after the 'standard' colours
      IB = 20    !  -1      !- 20++ leaves me more room

      IF (TYPE.EQ.'line') IPRIM = 3    !- prim type 3 = polyline
      IF (TYPE.EQ.'fill') IPRIM = 2    !- prim type 2 = filled
                       
      J = ICOL/2            !- Ok half the colour
      K = ICOL-2*J          !- either 1=odd, or 2=even
c... hmm so fill will never pick a colour outside 1->16
c     ICOL2 = IB + MOD (ICOL,13)    !- offset into the colour table
      ICOL2 = IB + ICOL             !- offset into the colour table
      IOP2 = IOP-1000
      IF (IOP2.lt.0) then         !- solid colours
        CALL DR_PS (T,X,Y,N,IOP,IPRIM)
      ELSEIF (IOP2.eq.0) THEN                            !- black (obs)
        CALL DR_PS (T,X,Y,N,0,IPRIM)
      ELSEIF (IOP2.eq.1) THEN                        !- white    (obs)
        CALL DR_PS (T,X,Y,N,1,IPRIM)
      ELSEIF (IOP2.eq.2) THEN                        !- coloured
        CALL DR_PS (T,X,Y,N,ICOL2,IPRIM)
      ELSEIF (IOP2.eq.3) THEN                        !- zebra evens
        CALL DR_PS (T,X,Y,N,K,IPRIM)
      ELSEIF (IOP2.eq.4) THEN                        !- zebra odds
        CALL DR_PS (T,X,Y,N,1-K,IPRIM)                             
      ELSEIF (IOP2.eq.5) THEN                        !- stripe with invis (-1) 
        IF (K.EQ.0) CALL DR_PS (T,X,Y,N,ICOL2,IPRIM)
      ELSEIF (IOP2.eq.6) THEN                        !- stripe with 0
        IF (K.EQ.0) CALL DR_PS (T,X,Y,N,J+IB,IPRIM)
        IF (K.EQ.1) CALL DR_PS (T,X,Y,N,0,IPRIM)                              
      ELSEIF (IOP2.eq.7) THEN                        !- stripe with 1 
        IF (K.EQ.0) CALL DR_PS (T,X,Y,N,J+IB,IPRIM)          
        IF (K.EQ.1) CALL DR_PS (T,X,Y,N,1,IPRIM)                              
      ENDIF
      END

C-----------------------------------------------------------------------
c-----------------------------------------------------------------------
     
