! circle_class_fun.f90 - a unit test suite for circle_class.f90
!
! funit generated this file from circle_class.fun

module circle_class_fun

 use circle_class

 implicit none

 logical :: noAssertFailed

 public :: test_circle_class

 private

 integer :: numTests          = 0
 integer :: numAsserts        = 0
 integer :: numAssertsTested  = 0
 integer :: numFailures       = 0



! Global variables can be declared here
real, parameter :: radius = 1.5d0
real, parameter :: pi = 3.14159d0
type(circle) :: c

 contains



! Example test using all six assertions
 subroutine funit_assertions

  integer, dimension(2) :: a = (/ 1, 2 /)
  integer, dimension(2) :: b = (/ 1, 2 /)

  ! Assert_Array_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not. all(a==b)) then
      print *, " *Assert_Array_Equal failed* in test funit_assertions &
              &[circle_class.fun:22]"
      print *, "  ", "array a is not b"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Real_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.( (0.9999999e0 &
        +2*spacing(real(0.9999999e0)) ) &
        .ge. &
        (1.0e0) &
            .and. &
     (0.9999999e0 &
      -2*spacing(real(0.9999999e0)) ) &
      .le. &
       (1.0e0) )) then
      print *, " *Assert_Real_Equal failed* in test funit_assertions &
              &[circle_class.fun:23]"
      print *, "  ", "1.0e0 (", &
 1.0e0, &
  ") is not", &
 0.9999999e0,&
 "within", &
  2*spacing(real(0.9999999e0))
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((0.0 &
     +1e-6) &
     .ge. &
     (1e-7) &
             .and. &
     (0.0 &
     -1e-6) &
     .le. &
     (1e-7) )) then
      print *, " *Assert_Equal_Within failed* in test funit_assertions &
              &[circle_class.fun:24]"
      print *, "  ", "1e-7 (",1e-7,") is not", &
 0.0,"within",1e-6
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(1== 5 - 4)) then
      print *, " *Assert_Equal failed* in test funit_assertions &
              &[circle_class.fun:25]"
      print *, "  ", "1 (",1,") is not",  5 - 4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (5 < 4) then
      print *, " *Assert_False failed* in test funit_assertions &
              &[circle_class.fun:26]"
      print *, "  ", "5 < 4 is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(4 == 4)) then
      print *, " *Assert_True failed* in test funit_assertions &
              &[circle_class.fun:27]"
      print *, "  ", "4 == 4 is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine funit_assertions


 subroutine radius_is_stored_properly

  ! Assert_Real_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.( (radius &
        +2*spacing(real(radius)) ) &
        .ge. &
        (1.5d0) &
            .and. &
     (radius &
      -2*spacing(real(radius)) ) &
      .le. &
       (1.5d0) )) then
      print *, " *Assert_Real_Equal failed* in test radius_is_stored_properly &
              &[circle_class.fun:31]"
      print *, "  ", "1.5d0 (", &
 1.5d0, &
  ") is not", &
 radius,&
 "within", &
  2*spacing(real(radius))
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine radius_is_stored_properly


 subroutine area_varies_with_radius

  real :: area
  area = circle_area(c)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((pi*(radius**2) &
     +1e-3) &
     .ge. &
     (area) &
             .and. &
     (pi*(radius**2) &
     -1e-3) &
     .le. &
     (area) )) then
      print *, " *Assert_Equal_Within failed* in test area_varies_with_radius &
              &[circle_class.fun:37]"
      print *, "  ", "area (",area,") is not", &
 pi*(radius**2),"within",1e-3
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine area_varies_with_radius


 subroutine funit_setup
  ! Place code here that should run before each test
  c = circle(radius)
  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown
  ! This code runs immediately after each test
 end subroutine funit_teardown


 subroutine test_circle_class( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call funit_assertions
  call funit_teardown

  call funit_setup
  call radius_is_stored_properly
  call funit_teardown

  call funit_setup
  call area_varies_with_radius
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_circle_class

end module circle_class_fun
