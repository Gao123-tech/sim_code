!     Last change:  MPB  23 Dec 2003   12:18 pm
      PROGRAM POD

!     Converts a complex pressure field into a probability of detection

      USE SdRdRMod

      INTEGER PRTFIL, SHDFIL, PODFIL
      PARAMETER ( INPFIL = 5, PRTFIL = 6, SHDFIL = 26, PODFIL = 25 )

      REAL, ALLOCATABLE :: TL( : )
      COMPLEX, ALLOCATABLE :: P( : )
      CHARACTER title*80, PlotType*10, FileName*6

      WRITE( PRTFIL, * ) 'FOM, SIGMA?'
      READ(  INPFIL, * )  FOM, SIGMA

!     *** Read/write header ***

      CALL ReadHeader( SHDFIL, 'SHDFILIN', Title, Freq, Atten, DELTAR, PlotType, xs, ys, theta )
      ALLOCATE( P( NR ), TL( NR ) )

      FileName = 'SHDFIL'
      CALL WriteHeader( FileName, title, SD, NSD, RD, NRD, R, NR, Freq, Atten, PlotType, xs, ys, theta )
      
!     Read in pressure field and convert to transmission loss

      DO ISR = 1, NSD * NRD
         IREC =  ISR + 6
         READ ( SHDFIL, REC = IREC ) ( P( J ), J = 1, NR )

!        --- Convert to probability of detection
         DO J = 1, NR
            PSQ = P( J ) * CONJG( P( J ) )
            TL( J ) = 200.0
            IF ( PSQ > 1.0E-20 ) TL( J ) = -10.0 * ALOG10( PSQ )
            P( J ) = TRANSIT( FOM - TL( J ), SIGMA )
         END DO

!        --- Write out the POD
         WRITE( PODFIL, REC = IREC ) ( P( J ), J = 1, NR )
      END DO

      STOP
      END
