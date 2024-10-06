program test_split_data
!------------------------------------------------------------------------------
! Program: test_split_data
!
! Purpose:
!   Tests the split_data and compute_test_stats subroutines by performing
!   experiments with data having a constant mean and with a change point.
!
! Description:
!   - Initializes the random number generator.
!   - Generates random data for two test cases:
!     1. Constant mean across all observations.
!     2. Two segments with different means.
!   - Finds the optimal partition point and computes test statistics.
!   - Outputs the results, including optimal partition, means, MSE, test
!     statistic, and p-value.
!------------------------------------------------------------------------------
use split_data_mod, only: split_data, compute_test_stats
use random, only: random_normal, random_seed_init
implicit none
integer, parameter :: dp = kind(1.0d0)
integer, parameter :: n = 1000, ktrue = n/2 + 1, n1 = ktrue -1, n2 = n - n1
integer, parameter :: n_perm = 100 ! Number of permutations
real(kind=dp), parameter :: true_means(2) = [0.0_dp, 0.2_dp]
integer :: k_optimal, i, iter
real(kind=dp) :: x(n), xmeans(3), mse(3), test_stat, p_value
logical, parameter :: call_random_seed_init = .true.
integer, parameter :: niter_test_1 = 3, niter_test_2 = 3
character (len=*), parameter :: fmt_cr = "(a30,':',*(f12.6))", fmt_ci = "(a30,':',*(1x,i0))"
if (call_random_seed_init) call random_seed_init(123)
! Test 1: Constant Mean
if (niter_test_1 > 0) print *, "Test 1: Constant Mean"
write (*,fmt_cr) "true mean", true_means(1)
print*
do iter=1, niter_test_1
   x = random_normal(n) + true_means(1)
   call split_data(x, k_optimal, xmeans, mse)
   print fmt_ci, "Optimal partition", k_optimal
   print fmt_cr, "Means", xmeans
   print fmt_cr, "MSE", mse
   if (n_perm > 0) then
      call compute_test_stats(x, k_optimal, n, xmeans, mse, test_stat, p_value, n_perm)
      print fmt_cr, "Test Statistic", test_stat
      print fmt_cr, "p-value", p_value
   end if
   print fmt_cr, "Test Statistic", test_stat
   print fmt_cr, "p-value", p_value
   print*
end do
! Test 2: Change Point
if (niter_test_2 > 0) print "(/,a)", "Test 2: Change Point"
write (*,fmt_cr) "true means", true_means
write (*,fmt_ci) "true partition", ktrue
print*
do iter=1, niter_test_2
   do i = 1, n
     if (i < ktrue) then
       x(i) = random_normal() + 5.0_dp  ! First segment mean
     else
       x(i) = random_normal() + 5.4_dp ! Second segment mean
     end if
   end do
   call split_data(x, k_optimal, xmeans, mse)
   print fmt_ci, "Optimal partition k", k_optimal
   print fmt_cr, "means of of true partitions", sum(x(:ktrue-1))/n1, sum(x(ktrue:))/n2
   print fmt_cr, "Means", xmeans
   print fmt_cr, "MSE", mse
   if (n_perm > 0) then
      call compute_test_stats(x, k_optimal, n, xmeans, mse, test_stat, p_value, n_perm)
      print fmt_cr, "Test Statistic", test_stat
      print fmt_cr, "p-value", p_value
   end if
   print*
end do
end program test_split_data
