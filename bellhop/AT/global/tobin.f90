!     Last change:  MPB  22 Dec 2003    8:32 pm
      PROGRAM TOBIN

!     Converts ASCII format to binary format

      INTEGER SHDFIL, ASCFIL
      PARAMETER ( SHDFIL = 25, ASCFIL = 5 )

      REAL,    ALLOCATABLE :: R( : ), RD( : ), SD( : )
      COMPLEX, ALLOCATABLE :: P( : )
      CHARACTER TITLE*80, PLTTYP*10, FileName*6

!     *** Read the header data ***

      READ( ASCFIL, '( A80 )' ) TITLE
      READ( ASCFIL, * ) PLTTYP, XS, YS, THETA
      READ( ASCFIL, * ) FREQ, NSD, NRD, NR, atten

      ALLOCATE( SD( NSD ), RD( NRD ), R( NR ), P( NR ) )

      READ( ASCFIL, '( 6E13.5 )' ) ( SD( I ), I = 1, NSD )
      READ( ASCFIL, '( 6E13.5 )' ) ( RD( I ), I = 1, NRD )
      READ( ASCFIL, '( 6E13.5 )' ) (  R( I ), I = 1, NR  )

!     *** Write the header to the SHDFIL ***

      FileName = 'SHDFIL'
      CALL WriteHeader( FileName, TITLE, SD, NSD, RD, NRD, R, NR, FREQ, ATTEN, PLTTYP, XS, YS, THETA )

!     *** Main loop: convert pressure surfaces for each S/R combo ***

      DO ISD = 1, NSD
         IREC = 6 + ( ISD - 1 ) * NRD

         DO IRD = 1, NRD
            IREC = IREC + 1
            READ( ASCFIL, '( 6E13.5 )' ) ( P( J ), J = 1, NR )
            WRITE( *, * ) ISD, IRD, SD( ISD ), RD( IRD )
            WRITE( SHDFIL, REC = IREC ) ( P( J ), J = 1, NR )
         END DO

      END DO

      STOP
      END
