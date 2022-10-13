!     Last change:  MPB  24 Nov 2002    6:25 pm
      PROGRAM CAPON

!     Takes covariance matrix and replica shade file and computes a Capon (MLM) surface
!     !!! This version is not up to date with changes to rdhead and the use of the SRDRD module

      USE SdRdRMod
      INTEGER REPFIL, COVFIL, AMSFIL
      PARAMETER ( REPFIL = 20, COVFIL = 21, AMSFIL = 25 MXPHONES = 50, TOL = 0.0001, EPS = 1.0E-5 )

      INTEGER IDUMMY( 1 )
      REAL    SURF( 1 )
      COMPLEX P( MXPHONES, MAXNR ), PC( MAXNR ), T
      CHARACTER TITLE*80, TITLEREP*80, PLTTYP*10

      INTEGER, ALLOCATABLE :: IRDREP( : )
      REAL,    ALLOCATABLE :: RDREP( : )
      COMPLEX, ALLOCATABLE :: COV( :, : ), Y( : )

      POWERMAX = 0.0
      ISDMAX   = 0
      IRMAX    = 0

!     *** Read covariance matrix ***

      OPEN( FILE = 'COVFIL', UNIT = COVFIL, STATUS = 'OLD' )

      READ( COVFIL, * ) TITLE
      READ( COVFIL, * ) FREQ
      READ( COVFIL, * ) NRD

      ALLOCATE( COV( NRD, NRD ), Y( NRD ), RDREP( NRD ), IRDREP( NRD ), STAT = IAllocStat )
      IF ( IAllocStat /= 0 ) STOP "Insufficient memory to allocate COV matrix"

      DO IRD = 1, NRD
         READ( COVFIL, * ) RD( IRD )
      END DO

      DO I = 1, NRD
         DO J = 1, NRD
            READ( COVFIL, * ) I1, J1, COV( I, J )
         END DO
      END DO

!     *** Diagonal loading for stability of inversion ***

      DO I = 1, NRD
         COV( I, I ) = COV( I, I ) + EPS
      END DO

!     *** Factor the covariance matrix into L L* form

      CALL CHOLESKI( COV, NRD, NRD )

!     *** Form power field (SURF = 1.0) ***

!     *** Read replica file header ***

      CALL RDHEAD( REPFIL, 'REPFIL', FREQREP, DELTAR, PLTTYP, XS, YS, THETA, TITLEREP )

      IF ( FREQ /= FREQREP ) THEN
         WRITE( *, * ) 'Mismatch in frequencies'
         STOP
      ENDIF

!     --- Find pointers to receiver depths in replica file

      DO IRD = 1, NRD
         IDUMMY = MINLOC( ABS( RDREP( 1:NRDREP ) - RD( IRD ) ) )
         IRDREP( IRD ) = IDUMMY( 1 )
         IF ( ABS( RDREP( IRDREP( IRD ) ) - RD( IRD ) ) > TOL ) STOP "No matching depth in replica file"
      END DO

!     *** Form header for power field (SURF = 1.0)

      SURF( 1 )  = 1.0
      NSURF = 1

      CALL HEADER( TITLE, SURF, NSURF, SD, NSD, R, NR, FREQ, ATTEN, PLTTYP, XS, YS, THETA )

      IRECOUT = 6

!     *** Loop over trial source depths ***

      DO ISD = 1, NSD

         DO IRD = 1, NRD
!           --- read in replica vector
            IRECIN = 6 + NRD * ( ISD - 1 ) + IRDREP( IRD )
            READ ( REPFIL, REC = IRECIN ) ( P( IRD, IR ), IR = 1, NR )
         END DO

!        *** Loop over trial source ranges *** 
         DO IR = 1, NR

!           --- compute power of replica vector
            SUMREP = 0.0
            DO I = 1, NRD
               SUMREP = SUMREP + P( I, IR ) * CONJG( P( I, IR ) )
            END DO

!           *** form Pc = e* R^(-1) e taking advantage of symmetry ***

!           --- start with y = L^(-1) P
            Y( 1 ) = P( 1, IR ) / COV( 1, 1 )
            DO I = 2, NRD
               T = 0.0
               DO J = 1, I-1
                  T = T + CONJG( COV( J, I ) ) * Y( J )
               END DO
               Y( I ) = ( P( I, IR ) - T ) / COV( I, I )
            END DO

!           --- compute y*y
            POWER  = 0.0
            DO I = 1, NRD
               POWER = POWER + Y( I ) * CONJG( Y( I ) )
            END DO

            POWER = SUMREP / POWER  ! Capon is (x*x) / (x*Rx)

!           --- make note of peak location
            IF ( POWER > POWERMAX ) THEN
               POWERMAX = POWER
               ISDMAX   = ISD
               IRMAX    = IR
            ENDIF

!           --- Normalize
            IF( POWER /= 0.0 ) THEN
!              --- take sqrt to go from power back to amplitude
!              --- take reciprocal to get proper sign in plot
               PC( IR ) = 1.0 / SQRT( POWER )
            ELSE
               PC( IR ) = 0.0
            ENDIF
         END DO

!        --- Write out Capon power vector
         IRECOUT = IRECOUT + 1
         WRITE( AMSFIL, REC = IRECOUT ) ( PC( IR ), IR = 1, NR )

      END DO        ! next source depth

      WRITE( *, * ) 'Peak power   = ', POWERMAX
      WRITE( *, * ) 'Source depth = ', SD( ISDMAX )
      WRITE( *, * ) 'Source range = ', R(   IRMAX )

      STOP
      END
