module split_data_mod
use kind_mod, only: dp
use random, only: random_shuffle
implicit none
private
public :: split_data, compute_test_stats

contains
subroutine mean_and_sse(x, mean, sse)
! Computes the mean and sum of squared errors (SSE) for the input array.
!
! Arguments:
!   x    - Input data array (INTENT(IN))
!   mean - Calculated mean of the array (INTENT(OUT))
!   sse  - Sum of squared errors of the array (INTENT(OUT))
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: mean, sse
integer :: n
n = size(x)
if (n > 0) then
   mean = sum(x)/n
   sse = sum((x-mean)**2)
else
   mean = 0.0_dp
   sse = 0.0_dp
end if
end subroutine mean_and_sse

subroutine split_stats(x, k, mean_1, mean_2, sse_1, sse_2)
! Calculates the means and sum of squared errors for two groups defined by partition k.
!
! Arguments:
!   x      - Input data array (INTENT(IN))
!   k      - Partition index (INTENT(IN))
!   mean_1 - Mean of the first group (INTENT(OUT))
!   mean_2 - Mean of the second group (INTENT(OUT))
!   sse_1  - SSE of the first group (INTENT(OUT))
!   sse_2  - SSE of the second group (INTENT(OUT))
real(kind=dp), intent(in)  :: x(:)
integer      , intent(in)  :: k
real(kind=dp), intent(out) :: mean_1, mean_2, sse_1, sse_2
integer                    :: n
n = size(x)
! set bad values if k is invalid
mean_1 = 0.0
mean_2 = 0.0
sse_1  = -1.0_dp
sse_2  = -1.0_dp
if (k < 2) then
   call mean_and_sse(x, mean_2, sse_2)
else if (k > n) then
   call mean_and_sse(x, mean_1, sse_1)
else
   call mean_and_sse(x(:k-1), mean_1, sse_1)
   call mean_and_sse(x(k:), mean_2, sse_2)
end if
end subroutine split_stats

  subroutine split_data(x, k, xmeans, mse)
! Finds the optimal partition point k that minimizes the total sum of squared
! errors (SSE) for the two resulting groups.
!
! Arguments:
!   x       - Input data array (INTENT(IN))
!   k       - Optimal partition index (INTENT(OUT))
!   xmeans  - Means of the entire data, Group1, and Group2 (INTENT(OUT))
!   mse     - Mean squared errors of the entire data, Group1, and Group2 (INTENT(OUT))
    real(kind=dp), intent(in)  :: x(:)
    integer      , intent(out) :: k          ! Optimal partition
    real(kind=dp), intent(out) :: xmeans(3)  ! Means: [Overall, Group1, Group2]
    real(kind=dp), intent(out) :: mse(3)     ! MSE: [Overall, Group1, Group2]
    real(kind=dp)              :: mean_1, mean_2, sse_1, sse_2, sse, sse_min
    integer :: n, n1, ktry
    logical :: best
    n = size(x)
    if (n < 1) return
    call mean_and_sse(x, xmeans(1), sse)
    mse(1) = sse/n
    if (n < 2) return
    sse_min = -1.0_dp
    do ktry=2, n
       call split_stats(x, ktry, mean_1, mean_2, sse_1, sse_2)
       sse = sse_1 + sse_2
       if (ktry == 2) then
          best = .true.
          sse_min = sse
       else
          best = sse < sse_min
          if (best) sse_min = sse
       end if
!       print "(i6,l4,*(1x,f8.4))",ktry, best, mean_1, mean_2, sse_min, sse, sse_1, sse_2 ! debug
       if (best) then
          k = ktry
          xmeans(2:3) = [mean_1, mean_2]
          n1 = ktry - 1
          mse(2:3) = [sse_1/n1, sse_2/(n-n1)]
       end if
    end do
  end subroutine split_data

  subroutine compute_test_stats(x, k, n, xmeans, mse, test_stat, p_value, n_perm)
!   Calculates the test statistic for the optimal data split and performs a
!   permutation test to determine the p-value, accounting for multiple testing.
!
! Arguments:
!   x          - Original data array (INTENT(IN))
!   k          - Optimal partition index (INTENT(IN))
!   n          - Number of observations (INTENT(IN))
!   xmeans     - Means of the entire data and both groups (INTENT(IN))
!   mse        - Mean squared errors of the entire data and both groups (INTENT(IN))
!   test_stat  - Computed test statistic (INTENT(OUT))
!   p_value    - P-value from permutation test (INTENT(OUT))
!   n_perm     - Number of permutations to perform (INTENT(IN))
    real(kind=dp), intent(in)  :: x(:)
    integer      , intent(in)  :: n
    integer      , intent(out) :: k
    real(kind=dp), intent(out) :: xmeans(3)
    real(kind=dp), intent(out) :: mse(3)
    real(kind=dp), intent(out) :: test_stat, p_value
    integer, intent(in)  :: n_perm
    real(kind=dp), allocatable :: permuted_x(:)
    real(kind=dp) :: perm_test_stat
    integer :: i, count
    real(kind=dp) :: se_diff
    ! Compute observed test statistic
    se_diff = sqrt(mse(2) / (k - 1) + mse(3) / (n - k + 1))
    test_stat = abs(xmeans(2) - xmeans(3)) / se_diff
    ! Initialize
    count = 0
    allocate(permuted_x(n))
    ! Permutation test
    do i = 1, n_perm
      permuted_x = x
      call random_shuffle(permuted_x)
      ! Find optimal split on permuted data
      call split_data(permuted_x, k, xmeans, mse)
      ! Compute test statistic on permuted data
      se_diff = sqrt(mse(2) / (k - 1) + mse(3) / (n - k + 1))
      perm_test_stat = abs(xmeans(2) - xmeans(3)) / se_diff
      if (perm_test_stat >= test_stat) count = count + 1
    end do
    p_value = real(count + 1, kind=dp) / real(n_perm + 1, kind=dp)
  end subroutine compute_test_stats

end module split_data_mod
