!     Last change:  MPB  23 Dec 2003   12:17 pm
      PROGRAM TOASC

!     Converts shade file to ASCII format

      USE SdRdRMod

      INTEGER SHDFIL, ASCFIL
      PARAMETER ( ASCFIL = 26 )

      COMPLEX, ALLOCATABLE ::   P( : )
      CHARACTER Title*80, PlotType*10, FileName*80

!     *** Read header ***

      SHDFIL = 25
      FileName = ' '
      CALL ReadHeader( SHDFIL, FileName, Title, Freq, Atten, DELTAR, PlotType, xs, ys, theta )
      ALLOCATE( P( NR ) )

!     *** Write header ***

      OPEN( FILE = 'ASCFIL', UNIT = ASCFIL, FORM = 'FORMATTED' )

      WRITE( ASCFIL, '( A80 )' ) TITLE
      WRITE( ASCFIL, * ) '''', PlotType, '''', XS, YS, THETA
      WRITE( ASCFIL, * ) FREQ, NSD, NRD, NR, atten
      WRITE( ASCFIL, '( 6E13.5 )' ) ( SD( I ), I = 1, NSD )
      WRITE( ASCFIL, '( 6E13.5 )' ) ( RD( I ), I = 1, NRD )
      WRITE( ASCFIL, '( 6E13.5 )' ) (  R( I ), I = 1, NR  )

!     *** Main loop: convert pressure surfaces for each s/r combo ***

      DO IS = 1, NSD
         IREC = 6 + ( IS - 1 ) * NRD

         DO IR = 1, NRD
            IREC = IREC + 1
            READ( SHDFIL, REC = IREC ) P
            WRITE( *, * ) IS, IR, SD( IS ), RD( IR )
            WRITE( ASCFIL, '( 6E13.5 )' ) P
         END DO

      END DO

      CLOSE( ASCFIL )

      STOP
      END
