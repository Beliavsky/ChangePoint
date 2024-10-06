module random
!     Author: Alan Miller
implicit none
private
public :: random_shuffle, random_normal, random_seed_init
integer, parameter :: dp = selected_real_kind(12, 60), sp = kind(1.0)
real(kind=dp), parameter :: pi = 3.141592653589793D0, half_pi = pi/2, half = 0.5_dp
interface random_normal
   module procedure random_normal_scalar,random_normal_vec
end interface
contains
!
function random_normal_vec(n) result(vec)
! return n random normal variates
integer, intent(in) :: n
real(kind=dp)       :: vec(n)
integer             :: i
do i=1,n
   vec(i) = random_normal_scalar()
end do
end function random_normal_vec
!
function random_normal_scalar() result(fn_val)

! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.

real(kind=sp) :: fn_val

!     Local variables
real(kind=dp), parameter :: s = 0.449871_dp, t = -0.386595_dp, a = 0.19600_dp, b = 0.25472_dp,    &
            r1 = 0.27597_dp, r2 = 0.27846_dp
real(kind=dp) :: u, v, x, y, q

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

do
  call random_number(u)
  call random_number(v)
  v = 1.7156_dp * (v - half)

!     Evaluate the quadratic form
  x = u - s
  y = abs(v) - t
  q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
  if (q < r1) exit
!     Reject P if outside outer ellipse
  if (q > r2) cycle
!     Reject P if outside acceptance region
  if (v**2 < -4.0_dp*log(u)*u**2) exit
end do

!     Return ratio of P's coordinates as the normal deviate
fn_val = v/u
end function random_normal_scalar

  subroutine random_shuffle(x)
!------------------------------------------------------------------------------
! Subroutine: random_shuffle
!
! Purpose:
!   Randomly shuffles the elements of the input array using the Fisher-Yates
!   algorithm. The shuffle is performed in place, ensuring each permutation
!   is equally likely.
!
! Arguments:
!   x - The array to shuffle. Must be a one-dimensional real array with
!       intent(INOUT).
!
! Example:
!     call random_shuffle(data)
!------------------------------------------------------------------------------

    real(kind=dp), intent(inout) :: x(:)
    integer :: i, j
    real(kind=dp) :: temp, rand_val

    do i = size(x), 2, -1
      call random_number(rand_val)
      j = int(rand_val * i) + 1
      temp = x(j)
      x(j) = x(i)
      x(i) = temp
    end do
  end subroutine random_shuffle

subroutine random_seed_init(iseed)
  ! Initializes the random number generator seed by setting each seed element 
  ! to a predefined value plus iseed.
  integer, intent(in) :: iseed
  integer, allocatable :: seed(:)
  integer :: seed_size, i
  call random_seed(size=seed_size)
  allocate(seed(seed_size))
  do i = 1, seed_size
    seed(i) = int(1.0e6_dp * real(i, kind=dp)) + iseed
  end do
  call random_seed(put=seed)
end subroutine random_seed_init

end module random
