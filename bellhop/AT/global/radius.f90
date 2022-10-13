!     Last change:  MPB  23 Dec 2003   12:18 pm
      PROGRAM RADIUS

!     Converts complex pressure to a detection radius

      USE SdRdRMod

      INTEGER INPFIL, PRTFIL, SHDFIL, FOMFIL
      PARAMETER ( INPFIL = 5, PRTFIL = 6, SHDFIL = 26, FOMFIL = 25, MXNFOM = 501 )

      REAL      FOM( MXNFOM ), RAD( MXNFOM )
      REAL,    ALLOCATABLE :: TL( : )
      COMPLEX, ALLOCATABLE :: P( : )
      CHARACTER TITLE*80, PlotType*10, SHDFILIN*80, FileName*6
      SHDFILIN = ' '

      WRITE( PRTFIL, * ) 'SIGMA?'
      READ(  INPFIL, * ) SIGMA
      SIGMA  = ABS( SIGMA )

      WRITE( PRTFIL, * ) 'NFOM, FOMMIN, FOMMAX'
      READ(  INPFIL, * )  NFOM, FOMMIN, FOMMAX

      DELFOM = ( FOMMAX - FOMMIN ) / ( NFOM - 1 )
      DO IFOM = 1, NFOM
         FOM( IFOM ) = FOMMIN + ( IFOM - 1 ) * DELFOM
      END DO

!     *** Read/write header ***

      CALL ReadHeader( SHDFIL, SHDFILIN, Title, Freq, Atten, DELTAR, PlotType, xs, ys, theta )
      ALLOCATE( P( NR ), TL( NR ) )

      FileName = 'SHDFIL'
      CALL WriteHeader( FileName, title, SD, NSD, RD, NRD, FOM, NFOM, Freq, Atten, PlotType, xs, ys, theta )
 
!     Read in pressure field and convert to transmission loss

      DO ISR = 1, NSD * NRD
         IREC =  ISR + 6
         READ ( SHDFIL, REC = IREC ) SRCD, RCVRD, ( P( IR ), IR = 1, NR )

!        --- convert pressure to TL

         DO IR = 1, NR
            PSQ = P( IR ) * CONJG( P( IR ) )
            TL( IR ) = 200.0
            IF ( PSQ > 1.0E-20 ) TL( IR ) = -10.0 * ALOG10( PSQ )
         END DO

         ! For each FOM, compute detection radius

         DO IFOM = 1, NFOM
            RAD( IFOM ) = 0.0
            DO IR = 2, NR
               RAD( IFOM ) = RAD( IFOM ) + ( R( IR ) - R( IR - 1 ) ) * TRANSIT( FOM( IFOM ) - TL( IR ), SIGMA )

               ! Glimpse range:
               ! IF ( TRANSIT( FOM( IFOM ) - TL( IR ), SIGMA ) >= 0.5 ) RAD( IFOM ) = R( IR )
            END DO
         END DO

         ! Write out the radius
         WRITE( FOMFIL, REC = IREC ) SRCD, RCVRD, ( RAD( IFOM ) / 1000.0, 0.0, IFOM = 1, NFOM )
      END DO

      STOP
      END
