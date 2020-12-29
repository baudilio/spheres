PROGRAM main
  USE spheretypes, ONLY : PI => sPi, node => snode
  USE algo, ONLY : staffKujilaars
  USE Fibonacci, ONLY : Sphere
  USE bta, ONLY : JORourke
  USE bta1, ONLY : alea, alea2, sobol
  IMPLICIT NONE

  ! -- Local variables

  INTEGER, PARAMETER :: N = 256
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

  ! - J.O. Rourke
    Call JORourke(n, nodes)
  !print '(I5/"JOrourke")', N
  !print "('H ',3F9.5)", node
  OPEN(Newunit=ilun, FILE="JORouke.xyz", Action="Write")
  !WRITE(ilun, FMT="('H ', 3F9.5)") node
  WRITE(ilun, FMT='(I5/"JORuouke")') n
  WRITE(ilun, FMT=100) nodes
  CLOSE(ilun)

  ! - BTA aleatory distribution method
  CALL Alea2(N, nodes)
  OPEN(Newunit=ilun, FILE="alea.xyz", Action="Write")
  WRITE(ilun, FMT='(I5/"Aleatory")') n
  WRITE(ilun, FMT=100) nodes
  CLOSE(ilun)

  ! - BTA Sobol distribution method
  CALL Sobol(N, nodes)
  OPEN(Newunit=ilun, FILE="sobol.xyz", Action="Write")
  WRITE(ilun, FMT='(I5/"Ilya M. Sobol")') n
  WRITE(ilun, FMT=100) nodes
  CLOSE(ilun)


100	FORMAT('H ', 3F9.5)
  print *, "All Done - Have a Great Day."
  STOP 0
END PROGRAM main
