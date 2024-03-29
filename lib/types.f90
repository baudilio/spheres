MODULE spheretypes
  IMPLICIT NONE

  INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(P=6)
  INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(P=8)

  REAL(KIND=SP), PARAMETER :: sPI = 3.141592653589793238462643383279502884197_SP
  REAL(KIND=DP), PARAMETER :: dPI = 3.141592653589793238462643383279502884197_DP

  TYPE snode
    REAL(KIND=SP) :: X, Y, Z
  END TYPE snode

  TYPE dnode
    REAL(KIND=DP) :: X, Y, Z
  END TYPE dnode

  TYPE, EXTENDS(snode) :: atoma
     CHARACTER(Len=1) :: symbol
  END TYPE atoma

  TYPE :: atom
     CHARACTER(Len=1) :: symbol
     REAL(KIND=DP) :: X, Y, Z
  END TYPE atom

END MODULE spheretypes

Module algo
  USE spheretypes, ONLY: wp => SP, nodes => snode
  implicit none

  ! The method of Saff and Kuijlaars arranges the nodes along a spiral
  ! in such a way that the distance between nodes along the spiral is
  ! approximately equal to the distance between coils of the spiral:
contains

  subroutine staffKujilaars(N, node)
    implicit none
    ! - dummy parameters
    integer, intent(in) :: N
    type(nodes), Dimension(N), intent(out) :: node

    ! - Local Variables
    real(wp) :: s
    real(wp) :: dz
    real(wp) :: long = 0.0_wp
    real(wp) :: z
    real(wp) :: r
    integer :: i

    s = 3.6/sqrt(Real(N, wp))
    dz = 2.0/N
    z = 1 - dz/2

    do i = 1, N
      r = sqrt(1.0_wp - z*z)
      node(i)%x = r * cos(long)
      node(i)%y = r * sin(long)
      node(i)%z = z
      z = z - dz
      long = long + s/r
    end do

  end subroutine staffKujilaars

end Module algo

Module fibonacci
  ! https://www.openprocessing.org/sketch/41142
  USE spheretypes, ONLY: wp => SP, nodes => snode, pi => sPI
  implicit none

contains
  subroutine Sphere(samples, randomize, node)
    implicit none
    !-- Dummy parameters
    integer, intent(in) :: samples
    logical, intent(in) :: randomize
    Type(nodes), Dimension(samples), Intent(out) :: node

    ! -- Local variables
    real(wp), Parameter :: increment = pi * (3.0 - sqrt(5.0_wp))
    real(wp) :: offset
    real(wp) :: phi, r
    real(wp) :: x, y, z
    real(wp) :: rnd = 1.0_wp
    integer :: i


    ! ---
    offset = 2.0_wp/samples

    if (randomize) then
      call random_number(rnd)
      rnd = rnd * samples
    end if

    do i = 0, samples-1
      y = (i * offset) - 1.0 + (offset / 2.0)
      r = sqrt(1.0 - y * y)

      phi = increment * MOD(i + rnd, Real(samples,wp))

      x = r * cos(phi)
      z = r * sin(phi)

      node(i+1)%x = x
      node(i+1)%y = y
      node(i+1)%z = z

    end do
  end subroutine Sphere

end Module fibonacci

MODULE bta
!! @article{ORourke1997,
!  title={Computational Geometry Column 31},
!  author={Joseph O'Rourke},
!  journal={SIGACT News},
!  year={1997},
!  volume={28},
!  pages={20-23}
!}

!  @book{10.5555/521378,
!author = {O’Rourke, Joseph},
!title = {Computational Geometry in C},
!year = {1998},
!isbn = {0521640105},
!publisher = {Cambridge University Press},
!address = {USA},
!edition = {2nd}
!}
  USE spheretypes, ONLY: wp => SP, nodes => snode, pi => sPI
  implicit none

contains
  subroutine JORourke(n, node)
    implicit none
    ! -- Dummy args
    integer, Intent(in) :: n
    Type(Nodes), Dimension(n), Intent(out) :: node

    ! -- Local Variables
    Real(WP), Parameter :: dl = 2.39996322973_wp ! pi * (3 - sqrt(5.0_wp))
    real(wp) :: long = 0.0_wp
    Real(wp) :: z, dz
    Real(wp) :: r
    integer :: k

    ! ---

    dz = 2.0_wp / N
    z  = 1.0_wp - dz/2.0

    do k = 1, N
      r = sqrt(1.0_wp - z*z)
      ! r = 1.0_wp ! Cylinder
      node(k)%x = r*cos(long)
      node(k)%y = r*sin(long)
      node(k)%z = z
      z = z - dz
      long = long + dl
    end do

  end subroutine JORourke
end module bta


MODULE bta1
  USE spheretypes, ONLY: wp => SP, nodes => snode, pi => sPI, atom
  implicit none
  PRIVATE
  PUBLIC :: alea, alea2, alea3, sobol

CONTAINS

  SUBROUTINE alea(n, node)
    implicit none
    ! -- Dummy args
    integer, Intent(in) :: n
    Type(Nodes), Dimension(n), Intent(out) :: node


    ! -- Local Variables
    integer :: i
    REAL(wp) :: theta, phi

    ! ---

    DO i = 1, N
       CALL Random_NUMBER(theta)
       CALL Random_NUMBER(phi)
       theta = theta * pi
       phi = 2*pi*phi
       node(i)%x = COS(phi)*SIN(theta)
       node(i)%y = SIN(phi)*SIN(theta)
       node(i)%z = COS(theta)
    end do

  END SUBROUTINE Alea

  SUBROUTINE alea2(n, node)
    implicit none
    ! -- Dummy args
    integer, Intent(in) :: n
    Type(Nodes), Dimension(n), Intent(out) :: node


    ! -- Local Variables
    integer :: i
    REAL(wp) :: x,y,z
    REAL(wp) :: d
    ! ---

    DO i = 1, N
       CALL Random_NUMBER(x)
       CALL Random_NUMBER(y)
       CALL Random_NUMBER(z)
       x = 2*x-1
       y = 2*y-1
       z = 2*z-1
       d = distance(x,y,z)
       node(i)%x = x/d
       node(i)%y = y/d
       node(i)%z = z/d
    end do


  END SUBROUTINE Alea2

  SUBROUTINE alea3(n, node)
    implicit none
    ! -- Dummy args
    integer, Intent(in) :: n
    TYPE(atom), Dimension(n), Intent(out) :: node


    ! -- Local Variables
    integer :: i
    REAL(wp) :: x,y,z
    REAL(wp) :: d
    REAL(wp), PARAMETER :: r = 0.98474502184_wp ! (3/pi)^(1/3)
    REAL(wp), PARAMETER :: scale = 10.0_wp

    ! ---

    DO i = 1, N
       CALL Random_NUMBER(x)
       CALL Random_NUMBER(y)
       CALL Random_NUMBER(z)
       x = 2*x-1
       y = 2*y-1
       z = 2*z-1
       d = distance(x,y,z)
       IF (d <= r) THEN
          node(i)%symbol='C'
       ELSE
          node(i)%symbol='N'
       ENDIF
       d = r / d
       d = scale*d
       node(i)%x = x*d
       node(i)%y = y*d
       node(i)%z = z*d
    end do

  END SUBROUTINE Alea3

  REAL(wp) FUNCTION distance(x,y,z)
    IMPLICIT NONE
    REAL(wp), INTENT(in) :: x,y,z
    distance = SQRT(x*x+y*y+z*z)
  END FUNCTION distance

! Ilya M. Sobol algorithm for modeling rnd variables.
  SUBROUTINE sobol(n, node)
    IMPLICIT NONE

    ! -- Dummy args
    INTEGER, INTENT(in) :: n
    TYPE(Nodes), DIMENSION(n), INTENT(out) :: node

    ! -- Local variables
    INTEGER :: i
    REAL(wp) :: u, v
    REAL(wp) :: phi, sin_theta, cos_theta
    REAL(wp), parameter :: scale = 10.0_wp ! Just a scale factor for the radius of the sphere.

    ! ---
    DO i = 1, N
       CALL Random_NUMBER(u)
       CALL Random_NUMBER(v)
       cos_theta = 2*u - 1
       sin_theta = SQRT(1 - cos_theta**2)
       phi = 2*pi*v
       node(i)%x = scale * COS(phi)*SIN_theta
       node(i)%y = scale * SIN(phi)*SIN_theta
       node(i)%z = scale * COS_theta
    end do

  END SUBROUTINE sobol

END MODULE bta1
