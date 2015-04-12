program mandel
  character :: line*80,  text*18 = '-.+xXoOOaaaa'
  complex :: z,c
  do j=1,24 ; do i=1,80 ; c = cmplx(i/25.-2.0,j/12.-1.) ; z = 0. 
  do k=1,63; z=z*z+c; if (abs(z)>2.) exit ; enddo
  kk= 1+log(1.*k)/log(2.) ; kk = 1+ k/8 ;line(i:i) = text(kk:kk) ; enddo
  write(*,'(a)') line(1:79) ; enddo
end

