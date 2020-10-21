PROGRAM main
  USE spheretypes, ONLY : PI => sPi, node => snode
  IMPLICIT NONE

  ! -- Local variables

  INTEGER, PARAMETER :: N = 128
  LOGICAL :: aleatory = .TRUE.
  TYPE(node), Dimension(N) :: nodes


  print *, "All Done - Have a Great Day."
  STOP 0
END PROGRAM main
