module parallel_module

type work
   double precision :: a, b, integral
   logical :: halt
end type work

type(work) :: coarray[*]   ! Shared by all images
   
contains

! integrate-- Integration.  Our example is the definite integral of
! f(x) = x.  Your integral or sub-calculation goes here.

  double precision function integrate(a, b)
    double precision a, b

    integrate = 0.5 * (b*b - a*a)
  end function integrate


! worker-- Infinite loop that processes integration requests from the
! master.  All images except #1 end up here.

  subroutine worker
    do
       sync images(1)   ! Wait for image 1 to set up our work
       if (coarray % halt) stop

       coarray % integral = integrate(coarray % a, coarray % b)
       sync images(1)   ! Release image 1 to compute final integral
    enddo
  end subroutine worker


! parallel_integral-- Subroutine for parallel integration of the
! interval (a, b).
  
  double precision function parallel_integral(a, b)
    double precision a, b, h

    if (num_images() == 1) then
       parallel_integral = integrate(a, b)
       return
    endif

! Set up the work for each image

    h = (b - a) / (num_images() - 1)
    do i=2, num_images()
       coarray[i] % halt = .FALSE.
       coarray[i] % a = a + (i-2) * h
       coarray[i] % b = a + (i-1) * h
    enddo

    sync images(*)    ! Release workers
    sync images(*)    ! Wait for workers to finish

!  Collect the results

    parallel_integral = 0.0
    do i=2, num_images()
       parallel_integral = parallel_integral + coarray[i] % integral
    enddo
  end function parallel_integral


! shutdown-- Called by image #1 to cause all other images to stop.

  subroutine shutdown
    do i=2, num_images()
       coarray[i] % halt = .TRUE.
    end do

    sync images(*)
  end subroutine shutdown
end module parallel_module


! Top-level program that calls the parallel integrator

program p
  use parallel_module

  if (this_image() /= 1) call worker

  do i=1, 10
     print *, i, parallel_integral(0.0d0, real(i, kind=8))
  enddo

  call shutdown
end program p

