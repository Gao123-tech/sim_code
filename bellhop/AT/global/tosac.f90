!     Last change:  MPB  23 Dec 2003   12:19 pm
      PROGRAM TOSAC

!     Converts NRL format to SACLANT format

      USE SdRdRMod

      INTEGER PLPFIL, PRTFIL, SHDFIL, CDRFIL, BDRFIL
      PARAMETER ( PLPFIL = 5, PRTFIL = 6, SHDFIL = 25, CDRFIL = 28, BDRFIL = 29, MAXNRZ = 110000 )

      INTEGER, ALLOCATABLE :: IDUMMY( : )
      REAL    TL( MAXNRZ )
      COMPLEX P( MAXNRZ )
      CHARACTER TITLE*80, OPTION*80, PLTTYP*10
      REAL, ALLOCATABLE :: TLRAW( : ), TLSM( : )

!     *** Read header ***

      CALL ReadHeader( SHDFIL, ' ', Title, Freq, Atten, DELTAR, PLTTYP, XS, YS, THETA )
      ALLOCATE( TLRAW( NR ), TLSM( NR ) )

      R(1:NR) = R(1:NR) / 1000.0

      READ( PLPFIL, * ) OPTION
      READ( PLPFIL, * ) SRCD
      READ( PLPFIL, * ) DR3DB

      TLMIN = 200.0
      TLMAX = 0.0
      IREC  = 6

!     --- Reduce NRD to fit in window
      NRDACT = NRD
      READ( PLPFIL, * ) DEPTH
      DO IRCVR = 1, NRD
         IF ( RD( IRCVR ) <= DEPTH ) IHOLD = IRCVR
      END DO
      NRD = IHOLD

!     *** Calculate source depth index ***

      ALLOCATE( IDUMMY( NSD ) )
      IDUMMY = MINLOC( ABS( SD(1:NSD) - SRCD ) )
      IS = IDUMMY( 1 )

!     --- Check to see if within tolerance

      IF ( FREQ == 0.0 ) THEN
         TOL = 0.001
      ELSE
         TOL = 1500.0 / ( 6.0 * FREQ )  ! one wavelength
      ENDIF

      IF ( ABS( SD( IS ) - SRCD ) > TOL ) THEN
         WRITE( PRTFIL, * ) 'Sources exist at the following depths:'
         WRITE( PRTFIL, * ) ( SD( I ), I = 1, NSD )
         CALL ERROUT( PRTFIL, 'F', 'PLOTFIELD', 'No source at specified depth' )
      ENDIF

!     *** Output header ***

      OPEN( FILE = 'CDRFIL', UNIT = CDRFIL )
      OPEN( FILE = 'BDRFIL', UNIT = BDRFIL )

      RMIN = R( 1  )
      RMAX = R( NR )
      NPX  = NR
      NPY  = NRD
      NX   = NR
      NY   = NRD

      XLEN   = 18.0
      XLEFT  = RMIN
      XRIGHT = RMAX
      XSCALE = ( RMAX-RMIN ) / XLEN
      XINC   = ( RMAX-RMIN ) / 5.0
      X1     = XLEFT
      XL     = XRIGHT

      YLEN   = 12.0
      YUP    = RD( 1  )
      YDOWN  = RD( NRD )
      YSCALE = ( YDOWN - YUP ) / YLEN
      YINC   = ( YDOWN - YUP ) / 5.0

      RECUP = YUP
      RECLO = YDOWN

      READ( PLPFIL, * ) TLMIN, TLMAX, TLINT
      ZMIN  = TLMIN
      ZMAX  = TLMAX
      ZSTEP = TLINT
      SDEP  = SD( IS )

      CALL CONDRW( TITLE,NPX,NPY,NX,NY,XLEFT,XRIGHT &
     &   ,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN &
     &   ,ZMAX,ZSTEP,FREQ,SDEP,RECUP,RECLO,X1,XL,TLSM )

!     Loop over receivers

      IREC = 6 + ( IS - 1 ) * NRDACT

      DO I = 1, NRD
         IREC = IREC + 1
         WRITE( *, * ) IREC
         READ( SHDFIL, REC = IREC ) ( P( ( I - 1 ) * NR + J ), J = 1, NR )

         DO J = 1, NR
            JJ = ( I - 1 ) * NR + J
            IF ( OPTION(1:2) == 'DB' ) THEN
               PSQ = P( JJ ) * CONJG( P( JJ ) )
               TLRAW( J ) = 200.0
               IF ( PSQ > 1.0E-20 ) TLRAW(J) = -10.0 * ALOG10( PSQ )
            ELSE
               TLRAW( J ) = REAL( P( JJ ) )
            ENDIF
         END DO

!        *** Smooth ***

         NROWS = 1
         CALL SMOOTH( TLRAW, TLSM, NROWS, 1, NR, DELTAR, DR3DB )
         WRITE( 29, 444 ) ( TLSM( L ), L = 1, NR )
 444     FORMAT( ' ',6E13.4 )

         DO J = 1, NR
            JJ = ( I - 1 ) * NR + J
            TL( JJ ) = TLSM( J )
            TLMAX = MAX( TL( JJ ), TLMAX)
            TLMIN = MIN( TL( JJ ), TLMIN)
         END DO
      END DO

      STOP
      END

!**********************************************************************C

      SUBROUTINE CONDRW(TITLE,NPX,NPY,NX,NY,XLEFT,XRIGHT,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN, &
        ZMAX,ZSTEP,FREQ,SD,RECUP,RECLO,X1,XL,PX)
      DIMENSION SECTOR(28),PX(1)
      CHARACTER*50 FILENM
      CHARACTER*4 TITLE(20),TITLEX(20),TITLEY(20)
      DATA X1PL,Y1PL/2.,2.0/,HGTPT,HGTC,LABPT,NDIV,NARC/0.1,0.14,-3,1,5/,LABC,LWGT/-1,-1/,NSM/0/
      DATA DUMMY /0./
      DATA TITLEX /'Rang','e (k','m)  ',17*'    '/
      DATA TITLEY /'Dept','h (m',')   ',17*'    '/
!
!   FORMATS
 401  FORMAT(' ',F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE X AXIS')
 402  FORMAT(' ',F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE Y AXIS')
403   FORMAT(' ',F15.4,3X,'  DIVX ' )
404   FORMAT(' ',F15.4,3X,'  DIVY ' )
405   FORMAT(' ',F15.4,3X,'  FLAGRC ' )
406   FORMAT(' ',F15.4,3X,'  RDUP ' )
407   FORMAT(' ',F15.4,3X,'  RDLO ' )
408   FORMAT(' ',F15.4,3X,'  SOURCE DEPTH (M) ' )
 409  FORMAT(' ',F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE X AXIS ' )
 410  FORMAT(' ',F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE Y AXIS ' )
  411 FORMAT(' ',F15.4,3X,'  FREQUENCY (HZ)' )
  412 FORMAT(' ',F15.4,3X,'  DUMMY ' )
  413 FORMAT(' ',F15.4,3X,'  CAY ' )
  414 FORMAT(' ',F15.4,3X,'  NRNG ' )
  415 FORMAT(' ',F15.4,3X,'  ZMIN ' )
  416 FORMAT(' ',F15.4,3X,'  ZMAX ' )
  417 FORMAT(' ',F15.4,3X,'  ZINC ' )
  418 FORMAT(' ',F15.4,3X,'  X ORIGIN OF PLOT IN INCHES ' )
  419 FORMAT(' ',F15.4,3X,'  DUMMY ' )
  420 FORMAT(' ',F15.4,3X,'  Y ORIGIN OF PLOT IN INCHES ' )
  421 FORMAT(' ',F15.4,3X,'  NSM   ' )
  422 FORMAT(' ',F15.4,3X,'  HGTPT ' )
  423 FORMAT(' ',F15.4,3X,'  HGTC ' )
  424 FORMAT(' ',F15.4,3X,'  LABPT ' )
  425 FORMAT(' ',F15.4,3X,'  NDIV ' )
  426 FORMAT(' ',F15.4,3X,'  NARC ' )
  427 FORMAT(' ',F15.4,3X,'  LABC ' )
  428 FORMAT(' ',F15.4,3X,'  LWGT ' )
  800 FORMAT('CONDR,FIP,FMT,CPX,COL,VTT')
 801  FORMAT(A50)
  850 FORMAT(20A4)
  900 FORMAT(1X,F15.4,3X,'  XLEFT',/,F15.4,4X,'  XRIGHT',/,F15.4,3X,'   XSCALE',/,F15.4,4X,'  XINC')
  901 FORMAT(1X,F15.4,3X,'  YUP',/,F15.4,4X,'  YDOWN',/,F15.4,3X,'   YSCALE',/,F15.4,4X,'  YINC')
  950 FORMAT(' ',F15.4,1X,'    RMIN',/,F15.4,2X,'    RMAX')

      WRITE(28,800)
      WRITE(28,850)TITLE
      SECTOR( 1:28 ) = 0.0

      SECTOR(1)=NPX

!   SECTOR(4) IS A FLAG WHICH IS SET TO ZERO IN THE RANGE
!   DEPENDENT VERSION OF SNAP FOR ALL SECTORS EXCEPT THE LAST
!   ONE. HERE IS USED TO INDICATE THAT THIS IS THE LAST SECTOR

      SECTOR(4)=1.0

      WRITE(29,444) (SECTOR(L),L=1,28)
 444  FORMAT(' ',6E13.4)

      INQUIRE(UNIT=29,NAME=FILENM)
      WRITE(28,801) FILENM

      IF (ABS(XL-X1) < 1.0) THEN
      TITLEX(2)='e (m'
      TITLEX(3)=')   '
      DIVX=1E0
      ELSE
      TITLEX(2)='e (k'
      TITLEX(3)='m)  '
      DIVX=1E-3
      END IF
      DIVY=1E0
      CAY=5.
      NRNG=5
      FLAGRC=0.
      WRITE(28,850)TITLEX

      R1=X1*1.0E3
      R2=XL*1.0E3
      WRITE(28,950)R1,R2

      AX1=XLEFT*1.0E3
      AX2=XRIGHT*1.0E3
      AX3=XSCALE*1.0E3
      AX4=XINC*1.0E3

      WRITE(28,900)AX1,AX2,AX3,AX4
      WRITE(28,850)TITLEY
      WRITE(28,901)YUP,YDOWN,YSCALE,YINC
      WRITE(28,401)FLOAT(NPX)
      WRITE(28,402)FLOAT(NPY)
      WRITE(28,403)DIVX
      WRITE(28,404)DIVY
      WRITE(28,405)FLAGRC
      WRITE(28,406)RECUP
      WRITE(28,407)RECLO
      WRITE(28,408)SD
!   NUMBER OF GRID POINTS ALONG THE X AXIS
      WRITE(28,409)FLOAT(NX)
!   NUMBER OF GRID POINTS ALONG THE Y AXIS
      WRITE(28,410)FLOAT(NY)
      WRITE(28,411)FREQ
      WRITE(28,412)DUMMY
      WRITE(28,413)CAY
      WRITE(28,414)FLOAT(NRNG)
      WRITE(28,415)ZMIN
      WRITE(28,416)ZMAX
      WRITE(28,417)ZSTEP
! X ORIGIN  OF PLOT IN INCHES
      WRITE(28,418)X1PL
      WRITE(28,419)DUMMY
! Y ORIGIN  OF PLOT IN INCHES
      WRITE(28,420)Y1PL
      WRITE(28,421)FLOAT(NSM)
      WRITE(28,422)HGTPT
      WRITE(28,423)HGTC
      WRITE(28,424)FLOAT(LABPT)
      WRITE(28,425)FLOAT(NDIV)
      WRITE(28,426)FLOAT(NARC)
      WRITE(28,427)FLOAT(LABC)
      WRITE(28,428)FLOAT(LWGT)

      RETURN
      END
!**********************************************************************C
       SUBROUTINE CONDRB(NP1,NP2,NPX,PX)
       DIMENSION PX(1)

       DO 1000 I=NP1,NP2
       PX(I-NP1+1)=PX(I)
1000   CONTINUE
       WRITE(29,444) (PX(L),L=1,NPX)
 444  FORMAT(' ',6E13.4)

       RETURN
       END
