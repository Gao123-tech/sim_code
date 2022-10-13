      PROGRAM TABSSP

!     Tabulate the sound speed profile for use by another program

      IMPLICIT REAL (KIND=4) ( A-H, O-Z )
      real x(2), gradc(2)

!     OPEN( UNIT = 27, FILE = 'tabssp.prt', STATUS = 'UNKNOWN', CARRIAGECONTROL = 'LIST' )

      WRITE( *, * ) 'Depth, N, RMIN, RMAX, NR ='
      READ( *, * ) D, N, RMIN, RMAX, NR

      IF ( NR == 1 ) THEN
         DELTAR = 0.0
      ELSE
         DELTAR = ( RMAX - RMIN ) / ( NR - 1 )
      ENDIF

      DO IR = 1, NR
         R  = RMIN + ( IR - 1 ) * DELTAR
         RM = 1000.0 * R
         WRITE( 27, * ) R

         x(1) = RM
         DO I = 1, N
            x(2) = ( I - 1 ) * D / ( N - 1 )
            CALL ANALYT( x, C, gradc, CRR, CRZ, CZZ, 'TAB' )
!           WRITE( *, '( 6( F9.2, '', '') )' ) x(2), C, 0.0, 1.0, 0.0, 0.0
            WRITE( *, '( F9.2, F9.2 )' ) x(2), C
         END DO

      END DO

      STOP
      END
