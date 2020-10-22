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
