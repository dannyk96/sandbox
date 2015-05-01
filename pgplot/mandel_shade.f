      program mandelbrot_shade
      implicit none
      complex c,z_ini,z
      integer pgopen,i,j,k,i_again,isymbol,n_gen,n_fifteenth
      integer idummy,pgband,indx
      real lmd_x_min,lmd_x_max,lmd_y_min,lmd_y_max,lmd_x,lmd_y,abs_z
      real x_pos,y_pos,x1,x2,y1,y2,temp
      character*(1) ch

      isymbol = -1

      write (*,*) 'Plots c such that f(z)=z*z+c divserges.'
      write (*,*)
c     z_ini = (0.5,0.0)
      z_ini = (0.0,0.0)

      if ( pgopen('/xwin') .le. 0 ) stop
      call pgpap(5.0,1.0)
c pgask(0) means not to ask confirmation when erase a page
      call pgask(0)

 100  write (*,*) 'What is the numer of generation for the mapping ?'
      n_gen = 100

      do i=1,n_gen/3
c       call pgscr(i+1,0.0,0.0,i*1.0*3/n_gen) ! Blue
        call pgscr(i+1,i*1.0*3/n_gen,0.0,0.0) ! Red
      enddo
      do i=1,n_gen/3
        call pgscr(i+(n_gen/3)+1,1.0,i*1.0*3/n_gen,0.0)
c       call pgscr(i+(n_gen/3)+1,0.0,i*1.0*3/n_gen,0.0)
      enddo
      do i=1,n_gen/3
        call pgscr(i+(n_gen/3)*2+1,1.0,1.0,i*1.0*3/n_gen)
c       call pgscr(i+(n_gen/3)*2+1,i*1.0*3/n_gen,0.0,0.0)
      enddo


      call pgeras
      call pgsci(1)

      lmd_x_min = -1.0
      lmd_x_max =  1.0
      lmd_y_min = -1.0
      lmd_y_max =  1.0
 
      lmd_x_min = -2.0
      lmd_x_max =  0.5
      lmd_y_min = -1.25
      lmd_y_max =  1.25

 200  call pgenv(lmd_x_min, lmd_x_max, lmd_y_min, lmd_y_max, 1, 0)

      n_fifteenth = n_gen/15
      write(*,*) "MAX:",maxexponent(lmd_x)
      do j=1,500
        lmd_x = lmd_x_min + j*(lmd_x_max-lmd_x_min)/500
        do k=1,500
          lmd_y = lmd_y_min + k*(lmd_y_max-lmd_y_min)/500
c         lambda = (lmd_x,lmd_y)
c         lambda = (1.0,0.0)*lmd_x + (0.0,1.0)*lmd_y
          c = (1.0,0.0)*lmd_x + (0.0,1.0)*lmd_y
          z = z_ini
          call pgbbuf
          do i=1,n_gen
c           z = 4*lambda*z*(1-z)
            z = z*z + c
            abs_z = conjg(z)*z
            if (abs_z.gt.10e+10) then
c             call pgsci(i/n_fifteenth)
c             call pgsci(int(log(abs_z)))
              call pgsci(i)
                if(mod(j,10)==0) then
                !write(*,*) c, z, lmd_y, lmd_x, lmd_y_min, lmd_x_min
              endif
              call pgpt(1,lmd_x,lmd_y,isymbol)
              exit
            endif
          end do
          call pgebuf
        end do
      end do
      call pgsci(1)
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)

      write (*,*) 'Enlarge some parts ? (1=YES;0=NO)'
      i_again=1
      if (i_again.eq.0) goto 102

      write (*,*) 'Now choose a range to enlarge using mouse pointer.'
      write (*,*)
      idummy = pgband (0, 1, 0.5, 0.5, x_pos, y_pos, CH)
      x1 = x_pos
      y1 = y_pos 
      idummy = pgband (2, 1, x1, y1, x_pos, y_pos, CH)
      x2 = x_pos
      y2 = y_pos

      if (x1.gt.x2) then
        temp = x1
        x1 = x2
        x2 = temp
      endif
      if (y1.gt.y2) then
        temp = y1
        y1 = y2
        y2 = temp
      endif

      write (*,*) 'Your choice is: ', ' r = (',x1,'~',x2,' y = (',y1,'~'
     &,y2,')'

      lmd_x_min = x1
      lmd_x_max = x2
      lmd_y_min = y1
      lmd_y_max = y2

      write (*,*) 'What is the numer of generation for the mapping ?'
      n_gen = 100
      goto 200

 102  write (*,*) 'Again ? (1=YES;0=NO)'
      read (*,*) i_again
      if (i_again.eq.1) goto 100
      if (i_again.eq.0) stop
      goto 102

      end

