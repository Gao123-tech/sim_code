!     Last change:  MPB  23 Dec 2003   12:06 pm
      PROGRAM TONRL

!     Converts SACLANT (CDR, BDR) format to NRL (SHD) format

      INTEGER CDRFIL, BDRFIL
      PARAMETER ( CDRFIL = 28, BDRFIL = 29, MAXNZ = 500, MAXNR = 4094, MAXNRZ = 300000 )

      REAL R( MAXNR ), RD( MAXNZ ), SD( MAXNZ ), TL( MAXNR )
      COMPLEX P( MAXNRZ )
      CHARACTER TITLE*80, FileName*6, PlotType*10

!     Read in CDR header

      OPEN( FILE = 'CDRFIL', UNIT = CDRFIL, STATUS = 'OLD' )
      OPEN( FILE = 'BDRFIL', UNIT = BDRFIL, STATUS = 'OLD' )

      READ( CDRFIL, * )

      READ( CDRFIL, '( A80 )' ) TITLE
      WRITE( *, * ) TITLE

      READ( CDRFIL, * )
      READ( CDRFIL, * )
      READ( CDRFIL, * ) R(1)
      READ( CDRFIL, * ) R(2)
      WRITE( *, * ) 'RMIN = ', R(1), ' RMAX = ', R(2)

      READ( CDRFIL, * )
      READ( CDRFIL, * )
      READ( CDRFIL, * )
      READ( CDRFIL, * )
      READ( CDRFIL, * )

      READ( CDRFIL, * ) RD(1)
      READ( CDRFIL, * ) RD(2)
      WRITE( *, * ) 'ZMIN = ', RD(1), ' ZMAX = ', RD(2)

      READ( CDRFIL, * )
      READ( CDRFIL, * )

      READ( CDRFIL, * ) NR
      READ( CDRFIL, * ) NRD
      WRITE( *, * ) 'NR = ', NR, ' NRD = ', NRD

      READ( CDRFIL, * )
      READ( CDRFIL, * )
      READ( CDRFIL, * ) FLAGRC
      READ( CDRFIL, * )
      READ( CDRFIL, * )

      READ( CDRFIL, * ) SD(1)
      READ( CDRFIL, * )
      READ( CDRFIL, * )
      READ( CDRFIL, * ) FREQ
      WRITE( *, * ) 'FREQ = ', FREQ, ' SD = ', SD( 1 )

      NSD = 1
      RD( 3 ) = -999.9
      CALL SUBTAB( RD, NRD )

      R( 3 ) = -999.9
      CALL SUBTAB( R,  NR )

      FileName = 'SHDFIL'
      PlotType = '          '
      xs = 0.0
      ys = 0.0
      theta = 0.0
      CALL WriteHeader( FileName, title, SD, NSD, RD, NRD, R, NR, Freq, Atten, PlotType, xs, ys, theta )
 
!     ------ Some header junk
      READ( BDRFIL, * ) ( TL( J ), J = 1, 28 )

!     ***** Read by row or by column *****

      IF ( FLAGRC == 0.0 ) THEN

!     *** Read in TL data ***

         IREC = 6
         DO I = 1, NRD
            WRITE( *, * ) I
            READ( BDRFIL, * ) ( TL( J ), J = 1, NR )

            DO J = 1, NR
               K = J
               P( K ) = 0.0
               IF ( TL(J) < 200.0 ) P( K ) = CMPLX( SQRT( 10.0**( -TL(J) / 10.0 ) ), 0.0 )
            END DO

!           ------ Write it out
            CALL WRTFLD( P, 1, NR, IREC )
         END DO

      ELSE

!        *** Read in TL data ***

         DO J = 1, NR
            WRITE( *, * ) J
            READ( BDRFIL, * ) ( TL(I), I = 1, NRD )

            DO I = 1, NRD
               K = ( J - 1 ) *  NRD + I
               P( K ) = 0.0
               IF ( TL( I ) < 200.0 ) P( K ) = CMPLX( SQRT( 10.0**( -TL(I) / 10.0 ) ), 0.0 )
            END DO

         END DO

!        *** Write it out ***

         IREC = 5
         CALL WRTFLD( P, NRD, NR, IREC )

      ENDIF

      STOP
      END
