!     Last change:  MBP  30 Oct 1999    9:27 pm
      SUBROUTINE ANALYT( DEPTH, CP, CS, RHO, MED, N1, FREQ, ATUNIT, TASK )

!     MUNK PROFILE

!     RETURNS
!        CS, CP, RHO
!           FOR FLUID MEDIA AT I*H I = 1, N
!           FOR ELASTIC MEDIA AT I*H I = 0, N
!        AND DEPTHS OF INTERFACES

      IMPLICIT REAL (KIND=8) (A-H, O-Z)

      PARAMETER ( EPS = 0.00737 )
      COMPLEX (KIND=8) :: CP( N1 ), CS( N1 )
      REAL (KIND=8) ::     RHO( N1 )
      CHARACTER  ATUNIT*1, TASK*8

!     Initialization is only important for providing a print-out of
!     the SSP for the user.  Unfortunately it overruns the array
!     RHO( 1 ) so we suppress it:

      IF ( TASK(1:4) == 'INIT' ) RETURN

      N = N1 - 1

      SELECT CASE ( MED )

      CASE (1)   ! THE OCEAN
         H = 5000.0 / N
         DO I = 1, N1
            Z = ( I - 1 ) * H
            X = 2.0 * ( Z - 1300.0 ) / 1300.0
            CP(  I ) = 1500.0 * ( 1.0 + EPS * ( X - 1.0 + EXP( -X ) ) )
            CS(  I ) = 0.0
            RHO( I ) = 1.0
         END DO
         RETURN

         CASE (2)   ! THE FLUID HALF-SPACE
         CP(  1 ) = 1551.91
         CS(  1 ) = 0.0
         RHO( 1 ) = 1.0E20
         RETURN

         CASE( 9 )  ! AN ELASTIC LAYER
         H = 1000.0 / N
         Z = 5000.0
         DO I = 1,N+1
            CP( I ) = 4700.0 + ( Z - 5000.0 ) / 10.0
            CS( I ) = 2000.0 + ( Z - 5000.0 ) / 10.0
            CP( I ) = 4700.0
            CS( I ) = 2000.0
            RHO( I ) = 2.0
            Z = Z + H
         END DO
      END SELECT

      RETURN
      END
