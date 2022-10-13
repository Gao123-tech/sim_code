      PROGRAM PLANEWAVE

!     Makes a planewave covariance matrix

      INTEGER COVFIL
      COMPLEX CI
      PARAMETER ( COVFIL = 6, MAXNRD = 4094, PI = 3.141592, CI = ( 0.0, 1.0 ), DEGRAD = PI / 180.0 )

      REAL    RD( MAXNRD ), K0
      COMPLEX, ALLOCATABLE :: D( : ), COV( :, : )
      CHARACTER TITLE*80

!     *** Read covariance matrix ***

      C0    = 1500.0

      WRITE( *, * ) ' Frequency? '
      READ( *, * ) FREQ

      WRITE( *, * ) 'Declination angle?'
      READ( *, * ) THETA

      RD( 3 ) = -999.9
      WRITE( *, * ) 'NRD, RD( 1), ...'
      READ( *, * ) NRD, ( RD( IRD ) , IRD = 1, NRD )
      CALL SUBTAB( RD, NRD )

      WRITE( TITLE, 1000 ) INT( FREQ ), INT( THETA )
 1000 FORMAT( '''Planewave,  ', I2, ' Hz, ', I3, ' degrees''' )

      K0    = 2.0 * PI * FREQ / C0
      THETA = THETA * DEGRAD

      WRITE( COVFIL, * ) TITLE(1:60)
      WRITE( COVFIL, * ) FREQ
      WRITE( COVFIL, * ) NRD

      ALLOCATE( D( NRD ), COV( NRD, NRD ), STAT = IAllocStat )
      IF ( IAllocStat /= 0 ) STOP "Insufficient memory to allocate D, COV matrices"

      DO IRD = 1, NRD
         WRITE( COVFIL, * ) RD( IRD )
      END DO

      D( 1:NRD ) = EXP( CI * K0 * RD( 1:NRD ) * SIN( THETA ) )
      NORMD = SUM( D * CONJG( D ) )
 
      DO I = 1, NRD
         DO J = 1, NRD
            COV( I, J ) = NRD * D( I ) * CONJG( D( J ) ) / NORMD
            WRITE( COVFIL, * ) I, J, COV( I, J )
         END DO
      END DO

      STOP
      END
