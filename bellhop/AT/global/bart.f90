!     Last change:  MPB  24 Nov 2002    6:24 pm
      PROGRAM BART

!     Takes covariance matrix and replica shade file and computes a Bartlett surface

      USE SdRdRMod

      INTEGER REPFIL, COVFIL, AMSFIL
      PARAMETER ( REPFIL = 20, COVFIL = 21, AMSFIL = 25, TOL = 0.0001 )

      INTEGER IDUMMY( 1 )
      REAL    SURF( 1 )

      REAL,    ALLOCATABLE :: RDDAT( : )
      COMPLEX, ALLOCATABLE :: P( :, : ), PB( : ), COV( :, : )

      CHARACTER TITLE*80, TITLEREP*80, PLTTYP*10

      POWERMAX = 0.0
      ISDMAX   = 0  ; IRMAX    = 0

!     *** Read covariance matrix ***

      OPEN( FILE = 'COVFIL', UNIT = COVFIL, STATUS = 'OLD' )

      READ( COVFIL, * ) TITLE
      READ( COVFIL, * ) FREQ
      READ( COVFIL, * ) NRD

      ALLOCATE( COV( NRD, NRD ), RDDAT( NRD ), STAT = IAllocStat )
      IF ( IAllocStat /= 0 ) STOP "Insufficient memory to allocate COV matrix"

      DO IR = 1, NRD
         READ( COVFIL, * ) RDDAT( IR )
      END DO

      DO I = 1, NRD
         DO J = 1, NRD
            READ( COVFIL, * ) I1, J1, COV( I, J )
         END DO
      END DO

!     *** Form power field (SURF = 1.0) ***

!     *** Read replica file header ***

      CALL RDHEAD( REPFIL, 'REPFIL', FREQREP, DELTAR, PLTTYP, XS, YS, THETA, TITLEREP )

      IF ( FREQ /= FREQREP ) THEN
         WRITE( *, * ) 'Mismatch in frequencies'
         STOP
      ENDIF

!     --- Find pointers to receiver depths in replica file

      DO IR = 1, NRD
         IDUMMY = MINLOC( ABS( RD( 1:NRD ) - RDDAT( IRD ) ) )
         IRD( IR ) = IDUMMY( 1 )
         IF ( ABS( RD( IRD( IR ) ) - RD( IR ) ) > TOL ) &
           STOP "No matching depth in replica file"
      END DO

!     *** Form header for power field (SURF = 1.0)

      SURF( 1 )  = 1.0
      NSURF = 1

      CALL HEADER( TITLE, SURF, NSURF, SD, NSD, R, NR, FREQ, ATTEN, PLTTYP, XS, YS, THETA )

      ALLOCATE( P( NRD, NR ), PB( NR ), STAT = IAllocStat )
      IF ( IAllocStat /= 0 ) STOP "Insufficient memory to allocate P, Pb matrices"

      IRECOUT = 6

!     *** Loop over trial source depths ***

      DO IS = 1, NSD

         DO IR = 1, NRD
!           --- read in replica vector
            IRECIN = 6 + NRD * ( IS - 1 ) + IRD( IR )
            READ ( REPFIL, REC = IRECIN ) ( P( IR, IRNG ), IRNG = 1, NR )
         END DO

!        *** Loop over source ranges ***

         DO IR = 1, NR

!           *** form Pb = e* R e taking advantage of symmetry ***

            POWER  = 0.0

            DO I = 1, NRD

               IF ( I < NRD ) THEN
                  POWER = POWER + 2.0 * REAL( CONJG( P( I, IR ) ) * SUM( COV( I, I+1:NRD ) * P( I+1:NRD, IR ) ) )
               END IF

!              --- diagonal terms
               POWER = POWER + CONJG( P( I, IR ) ) * COV( I, I ) * P( I, IR )
            END DO

            SUMREP = DOT_PRODUCT( P( 1:NRD, IR ), P( 1:NRD, IR ) )

!           --- Normalize
            IF( SUMREP /= 0.0 ) THEN
!              --- take sqrt to go from power back to amplitude
!              --- take reciprocal to get proper sign in plot

               IF ( TITLE(1:12) /= 'Unnormalized' ) POWER = POWER / SUMREP

!              --- make note of peak location
               IF ( POWER > POWERMAX ) THEN
                  POWERMAX = POWER
                  ISDMAX   = IS
                  IRMAX    = IR
               ENDIF

               PB( IR ) = 1.0 / SQRT( POWER )
            ELSE
               PB( IR ) = 0.0
            ENDIF
         END DO        ! next source range

!        --- Write out Bartlett power vector
         IRECOUT = IRECOUT + 1
         WRITE( AMSFIL, REC = IRECOUT ) ( PB( IR ), IR = 1, NR )

      END DO        ! next source depth

!      WRITE( *, * ) 'Peak power   = ', POWERMAX
!      WRITE( *, * ) 'Source depth = ', SD( ISDMAX )
!      WRITE( *, * ) 'Source range = ', R(   IRMAX )

      WRITE( *, * ) R( IRMAX ), SD( ISDMAX )

      STOP
      END
