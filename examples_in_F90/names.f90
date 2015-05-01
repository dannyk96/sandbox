
program magic_name                                                 

!   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   |   This is to demonstrate character handling and formats             |
!   |   First we read in the users name                                   |
!   |   then uses it to write out as a 'Magic Square' to the screen       |
!   |                                                                     |
!   |     by Dr. Dan KIdger         1-Oct-1996                            |
!   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

    implicit none
    character*70 name                 !- (70 letters-long should be enough?)
    integer i,j,ln                    !- ln= length of users name

    print*,'Please type in your name (forename and surname)'
    read(*,'(a)') name

    ln = len_trim(name) + 1           !- include the space after the end
    name(ln+1:ln+ln) = name (1:ln)    !- 'double' the name (so 'Eric Eric ')

!----------------- now print out the table ---------------
    do i=1,ln
      write (*,'(T10,99A)')  (name(j:j)//' ',j=i,i+ln-1)
    enddo

end program magic_name           ! that's all folks

 
   

