! 
program Basics
  implicit none
  real(kind=4) :: single
  real(kind=8) :: double
  real(kind=8) :: answer
  real(kind=4), parameter :: SOUND_SPEED = 340.29
  real(kind=8), parameter  :: GLOBAL_SQRT2 = 1.414213562373095
  real(kind=8), external :: squaroot

  answer = (squaroot(2.d0, .true., .true.) )
  print *, answer, " from function"

  call square_root(2.d0, answer, .true., .true.)
  print *, answer, " from subroutine"
  

  !< .lt. > .gt. == .eq. <= .le. >= .ge. /= .ne. == .eqv. .or. .and.

end program Basics



real(kind=8) function squaroot(number, test, progress)
  implicit none
  integer :: attempts
  real(kind=8) :: previous
  real(kind=8), intent(in) :: number
  real(kind=8) :: approx
  logical, intent(in) :: test, progress

  if (number == 0) then
     approx = 0.d0
!     end function guess
  endif

  if (number == 1) then
     approx = 1.d0
!     end function guess
  endif

  approx = 1.d0
  previous = 0.d0
  attempts = 0

  do while (approx /= previous)
     previous = approx
     approx = 0.d5 * (approx + number / approx)
     if (test .eqv. .true.) then
        attempts = attempts + 1
     endif
     if (progress .eqv. .true.) then
        print *, approx
        continue
     endif
  enddo

end function squaroot



subroutine square_root(number, guess, test, progress)
  implicit none
  integer :: attempts
  real(kind=8) :: previous
  real(kind=8), intent(in) :: number
  real(kind=8), intent(out) :: guess
  logical, intent(in) :: test, progress

  if (number == 0) then
     guess = 0.d0
!     end subroutine squareroot
  endif

  if (number == 1) then
     guess = 1.d0
!     end subroutine squareroot
  endif

  guess = 1.d0
  previous = 0.d0
  attempts = 0

  do while (guess /= previous)
     previous = guess
     guess = 0.d5 * (guess + number / guess)
     if (test) then
        attempts = attempts + 1
     endif
     if (progress) then
        print *, guess
        continue
     endif
  enddo

end subroutine square_root
