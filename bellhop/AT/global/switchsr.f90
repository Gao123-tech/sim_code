!     Last change:  MPB  23 Dec 2003   12:15 pm
      PROGRAM SWITCHSR

!     Switches the sources and receivers in a file

      USE SdRdRMod

      INTEGER INFIL, OUTFIL 
      PARAMETER ( INFIL = 26,  OUTFIL = 25 )

      COMPLEX, ALLOCATABLE :: P( : )
      CHARACTER TITLE*80, PlotType*10, FileName*6

!     *** Read replica file header ***

      CALL ReadHeader( INFIL, 'INFIL', Title, Freq, Atten, DELTAR, PlotType, xs, ys, theta )
      ALLOCATE( P( NR ) )

!     *** Write new header with SD, RD interchanged ***

      FileName = 'SHDFIL'
      CALL WriteHeader( FileName, title, SD, NSD, RD, NRD, R, NR, Freq, Atten, PlotType, xs, ys, theta )
 
!     *** Loop to read successive slices ***

      DO IS = 1, NSD

         DO IR = 1, NRD
!           --- read in pressure for a particular source/rcvr
            IRECIN  = 6 + NRD * ( IS - 1 ) + IR
            READ (  INFIL, REC = IRECIN ) ( P( IRNG ), IRNG = 1, NR )
            WRITE( *, * ) 'SD = ', SD( IS ), 'RD = ', RD( IR )

!           --- Write it out in a new record position
            IRECOUT = 6 + NSD * ( IR - 1 ) + IS
            WRITE( OUTFIL, REC = IRECOUT ) ( P( IRNG ), IRNG = 1, NR )
         END DO

      END DO

      CLOSE( OUTFIL )

      STOP
      END
