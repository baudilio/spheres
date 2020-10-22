PROGRAM main
  USE spheretypes, ONLY : PI => sPi, node => snode
  USE algo, ONLY : staffKujilaars
  Use Fibonacci, only : Sphere
  IMPLICIT NONE

  ! -- Local variables

  INTEGER, PARAMETER :: N = 128
  LOGICAL :: aleatory = .TRUE.
  TYPE(node), DIMENSION(N) :: nodes
  INTEGER :: ilun

  ! - staffKujilaars method
  CALL staffKujilaars(N, nodes)
  !print '(I5/"Staffkujilaars")', N
  !print "('H ',3F9.5)", node
  OPEN(Newunit=ilun, FILE="staffKujilaars.xyz", Action="Write")
  WRITE(ilun, FMT='(I5/"staffKujilaars")') n
  WRITE(ilun, FMT=100) nodes
  CLOSE(ilun)


  ! - Fibonnaci sphere
  CALL Sphere(N, aleatory, nodes)
!  print '(I5/"Fibonacci")', N
!  print "('H ',3F9.5)", node
  OPEN(Newunit=ilun, FILE="fibonacci.xyz", Action="Write")
  WRITE(ilun, FMT='(I5/"Fibonacci")') n
  WRITE(ilun, FMT=100) nodes
  CLOSE(ilun)



100	FORMAT('H ', 3F9.5)
  print *, "All Done - Have a Great Day."
  STOP 0
END PROGRAM main
