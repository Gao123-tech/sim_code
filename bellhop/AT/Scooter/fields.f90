PROGRAM FIELDS

  !  Compute pressure from Green's function.

  ! The transform parameters are chosen to satisfy certain sampling requirements.

  ! It is assumed that the user chose the maximum possible 
  ! spacing in computing g(k) and so DeltaK is not allowed to be any larger.

  ! The other part of the kernel is the exp(ikr) term.
  ! This must be sampled on the k-axis finely enough to have about 6 pts per wavelength.

  ! DeltaR implies KMax. KMax is the upper limit of integration and beyond the last point computed.
  ! This assumes the user had chosen KMAX as small as possible while still covering the support of G(k).

  ! Note 'WNO' is used for original data and 'K' for interpolated data

  INTEGER   FLPFil, PRTFil
  PARAMETER ( FLPFil = 5, PRTFil = 6, PI = 3.14159265, MaxNWno =  1000000, MaxNsd = 501, MaxNrd = 2001 )
  REAL      Wno( MaxNWno ), RD( MaxNrd ), SD( MaxNsd ), Atten, Freq
  COMPLEX   G( MaxNWno )
  CHARACTER PlotTitle*80, Option*80
  REAL,    ALLOCATABLE :: R( : ), WnoInterp( : )
  COMPLEX, ALLOCATABLE :: Ginterp( : ), P( : ), Temp( : )

  WRITE( PRTFil, * )
  WRITE( PRTFil, * ) 'FIELDS: Transforms Green''s function FILE, producing pressure'
  ! *** Begin by reading in the data ***

    Option = 'RPO'            ! default option
    READ( FLPFil, * ) Option
    SELECT CASE ( Option(1:1) )
    CASE ( 'R' )
       WRITE( PRTFil, * ) 'Cylindrical coordinates/point source'
    CASE ( 'X' )
       WRITE( PRTFil, * ) 'Cartesian coordinates/line source'
    CASE DEFAULT
       CALL ERROUT( PRTFil, 'F', 'FIELDS', 'Option(1:1) should select Line vs. point source' )
    END SELECT

    SELECT CASE ( Option(2:2) )
    CASE ( 'P' )
       WRITE( PRTFil, * ) 'Using only positive part of wavenumber spectrum'
    CASE ( 'N' )
       WRITE( PRTFil, * ) 'Using only negative part of wavenumber spectrum'
    CASE ( 'B' )
       WRITE( PRTFil, * ) 'Using both positive and negative part of wavenumber spectrum'
    CASE DEFAULT
       CALL ERROUT( PRTFil, 'F', 'FIELDS', 'Option(2:2) should select positive, negative, or both parts of the spectrum' )
    END SELECT


    READ( FLPFil, * ) RMinKM, RMaxKM, Nrr

    RMin = 1000.0 * RMinKM
    RMax = 1000.0 * RMaxKM

    ! for a point source, take us a bit away from the source
    IF ( Option(1:1) == 'R' .AND. RMin <= RMax / 100000 ) THEN
       RMin = RMax / 100000
       WRITE( PRTFil, * ) 'RMin bumped to ', RMin
    END IF

    DeltaR = ( RMax - RMin ) / ( Nrr - 1 )

    ! *** Read the header records from GRNFil ***

    CALL RDHEAD( PlotTitle, SD, Nsd, MaxNsd, RD, Nrd, MaxNrd, Wno, NWno, MaxNWno, DeltaK, Atten, Freq )

    ! Set up for transform: need DelWno, NT
    ! DeltaK is what scooter used; DelWno is for interpolation
    ! If DeltaR is too big, take submultiple

    WnoMAX = Wno( 1 ) + 2.0 * PI / DeltaR
    IRatiodeltar = 1
    IF ( WnoMAX < Wno( NWno ) ) THEN
       IRatio = ( Wno( NWno ) - Wno( 1 ) ) / ( WnoMAX - Wno( 1 ) ) + 1
       DeltaR = DeltaR / IRatiodeltar
       Nrr    = IRatiodeltar * ( Nrr - 1 ) + 1
       WnoMAX = Wno( 1 ) + 2.0 * PI / DeltaR
       WRITE( PRTFil, * ) 'Number or ranges, Nrr, increased so that wavenumber limit exceeds kMax used by SCOOTER', Nrr
    END IF

    ! Compute NT based on DeltaK (= 1/RMax) sampling requirement

    NT2 = RMax * ( WnoMAX - Wno( 1 ) ) / ( 2.0 * PI )       ! DeltaK limit
    IF ( NT2 > Nrr ) THEN
       WRITE( PRTFil, * ) 'NT bumped to', NT2
       WRITE( PRTFIL, * ) 'Thus we are zero filling the wavenumber spectrum to effectively interpolate on a finer grid'
    END IF
    NT = MAX( Nrr, NT2 )

    ! bump Nt if necessary to make sure deltakInterp is not coarser than deltak grid
    deltak       = ( wno( NWno ) - wno( 1 ) ) / NWno;
    deltakInterp = 2 * PI / ( Nt * deltar );
    IF ( deltakInterp > deltak ) THEN
       IRatio = deltakInterp / deltak;
       Nt     = IRatio * Nt;
       WRITE( PRTFil, * ) 'Transform size, Nt, bumped to ensure deltak sampling is fine enough', Nt
    END IF

    ! Parameters when N must be a power of 2

    NT = 2 **( INT( LOG10( REAL( NT ) ) / 0.301 ) + 1 )
    DelWno = 2.0 * PI / ( NT * DeltaR )

    WRITE( PRTFil, * ) 'NT  used = ', NT
    WRITE( PRTFil, * ) 'Nrr used = ', Nrr
    WRITE( PRTFil, * )
    WRITE( PRTFil, * ) 'DeltaK = ', DeltaK
    WRITE( PRTFil, * ) 'DelWno = ', DelWno
    WRITE( PRTFil, * ) 'DeltaR = ', DeltaR
    WRITE( PRTFil, * )

    ALLOCATE( R( NT ), WnoInterp( NT ), Ginterp( NT ), P( NT ), Temp( 2 * NT ), Stat = IAllocStat )
    IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFil, 'F', 'FIELDS', 'Insufficient memory to allocate WnoInterp, Ginterp, ...' )


    ! Compute optimal stabilizing attenuation.
    ! Atten is the value used by SCOOTER
    ! AttInt is the optimal value for the interpolated k-values

    AttInt = Atten

    IF ( PlotTitle(1:5) == 'SPARC' ) THEN
       WRITE( PRTFil, * ) 'SPARC RUN: AttInt SET TO ZERO'
       AttInt = 0.0
       IF ( Atten /= 0.0 ) CALL ERROUT( PRTFil, 'F', 'FIELDS', 'Stabilizing attenuation must vanish' )
    ENDIF

    R( 1:Nrr ) = RMin + (/ ( J, J=0, Nrr - 1 ) /) * DeltaR   ! Set up vector of range points

    ! Construct shade file

    CALL SHADE( Wno, DelWno, Atten, NWno, G, NT, WnoInterp, Ginterp, &
         &      R, Nrr, RMin, DeltaR, SD, Nsd, RD, Nrd, &
         &      PlotTitle, Freq, AttInt, Option, P, Temp )

    STOP
  END program FIELDS
  !**********************************************************************C
  SUBROUTINE RDHEAD( PlotTitle, SD, Nsd, MaxNsd, RD, Nrd, MaxNrd, RK, NKPts, MaxNWno, DeltaK, Atten, Freq )

    ! Routine to read header from disk file
    ! This routine is essentially the same as at/misc/ReadHeader except that the range vector is replaced
    ! by a wavenumber vector.

    INTEGER GRNFil, PRTFil
    PARAMETER ( GRNFil = 20, PRTFil = 6 )
    REAL      RK( * ), RD( * ), SD( * )
    CHARACTER PlotTitle*80, PlotType*10

    OPEN( FILE = 'GRNFIL', UNIT = GRNFil, STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED', RECL = 10, IOSTAT = IOStat )
    IF ( IOSTAT /= 0 ) CALL ERROUT( PRTFil, 'F', 'FIELDS:RDHEAD', 'Unable to open GRNFIL' )
    READ( GRNFil, REC = 1 ) LRECL
    CLOSE( GRNFil )
    OPEN( FILE = 'GRNFIL', UNIT = GRNFil, STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED', RECL = 4 * LRECL )

    ! Read data

    READ( GRNFil, REC = 1 ) LRECL, PlotTitle
    READ( GRNFil, REC = 2 ) PlotType, XS, YS, THETA
    READ( GRNFil, REC = 3 ) Freq, Nsd, Nrd, NKPts, Atten

    IF ( Nsd > MaxNsd ) CALL ERROUT( PRTFil, 'F', 'FIELDS:RDHEAD', 'Too many source depths  ' )
    IF ( Nrd > MaxNrd ) CALL ERROUT( PRTFil, 'F', 'FIELDS:RDHEAD', 'Too many receiver depths' )

    READ( GRNFil, REC = 4 ) ( SD( I ), I = 1, Nsd )
    READ( GRNFil, REC = 5 ) ( RD( J ), J = 1, Nrd )

    IF ( NkPts > MaxNWno ) CALL ERROUT( PRTFil, 'F', 'FIELDS:RDHEAD', 'Too many k-space points' )

    READ( GRNFil, REC = 6 ) RK( 1 : NkPts )  ! vector of k-space points
    DeltaK = Rk( 2 ) - Rk( 1 )

    RETURN
  END SUBROUTINE RDHEAD
  !**********************************************************************C
  SUBROUTINE SHADE( Wno, DelWno, Atten, NWno, G, NT, WnoInterp, Ginterp, &
       &   R, Nrr, RMin, DeltaR, SD, Nsd, RD, Nrd, &
       &   PlotTitle, Freq, AttInt, Option,  P, Temp )

    ! Performs the transforms to convert the Green's function file to a shade file

    ! Expects
    !    Wno, NWno: The wavenumber data
    !    SD, RD: Source depth, receiver depth data
    ! Returns
    !    Nothing

    INTEGER    PRTFil, GRNFil, SHDFil
    PARAMETER  ( PRTFil = 6, GRNFil = 20, SHDFil = 25 )
    REAL       R( * ), WnoInterp( * ), SD( * ), RD( * ), Wno( * )
    COMPLEX    G( * ), Ginterp( * ), P( Nrr ), Temp( * )
    CHARACTER  PlotTitle*80, Option*80, PlotType*10, FileName*6

    FileName = 'SHDFIL'
    PlotType = '          '

    CALL WriteHeader( FileName, PlotTitle, SD, Nsd, RD, Nrd, R, Nrr, Freq, Atten, PlotType, 0.0, 0.0, 0.0 )

    ! *** Loop over all source/rcvr combos ***

    DO ISD = 1, Nsd
       WRITE( PRTFil, * ) 'Transform for source depth: ', SD( ISD )

       DO IRD  = 1, Nrd

          ISR    = ( ISD - 1 ) * Nrd + IRD   ! Index of source/receiver
          IRec = 6 + ISR

          READ( GRNFil, REC = IRec ) G( 1 : NWno )
          CALL INTERP( Wno, DelWno, Atten, AttInt, G, NWno, WnoInterp, Ginterp, NT, Option )   ! *** Interpolate ***

          ! *** Evaluate either Fourier or Hankel transform ***

          IF ( Option(1:1) == 'X' ) THEN
             CALL FTS( NT, WnoInterp( 1 ), DelWno, AttInt, RMin, Nrr, DeltaR, Ginterp, Temp, P )
          ELSE
             CALL HTS( NT, WnoInterp( 1 ), DelWno, AttInt, RMin, Nrr, DeltaR, Ginterp, Temp, P, Option )
          ENDIF

          WRITE( SHDFil, REC = IRec ) P   ! Write out the field ***

       END DO   ! next receiver depth
    END DO   ! next source depth

    RETURN
  END SUBROUTINE SHADE
  !**********************************************************************C
  SUBROUTINE INTERP( Wno, DelWno, Atten, AttInt, G, NWno, WnoInterp, Ginterp, NT, Option )

    ! Produces an evenly sampled kernel from input G

    INTEGER   PRTFil, ISize, ICenter, IRight
    COMPLEX   CI
    PARAMETER ( PRTFil = 6, CI = ( 0.0, 1.0 ), ISize = 3 )
    REAL      Wno( * ), WnoInterp( * )
    COMPLEX   G( * ), Ginterp( * ), WnoT2, X( 9 ), F( 9 ), PADE, PC
    CHARACTER ErrorMessage*80, Option*80

    ! *** Initialize interpolation data

    ICenter = ISize / 2 + 1   ! counter to mark center of abscissas used for interpolation
    IRight  = ISize
    New     = ISize

    X( 1:ISize ) = ( Wno( 1:ISize ) + CI * Atten )**2
    F( 1:ISize ) = G( 1:ISize )

    ! *** Main loop: take steps of DelWno along the k-axis ***

    DO I = 1, NT
       WnoInterp( I ) = Wno( 1 ) + ( I - 1 ) * DelWno

       ! Desirable/possible to advance interpolation window?
       DO WHILE ( WnoInterp( I ) > Wno( ICenter ) .AND. IRight < NWno )
          ICenter = ICenter + 1
          IRight  = IRight  + 1
          New     = MOD( New, ISize ) + 1   ! this is the next open slot

          X( New ) = ( Wno( IRight ) + CI * Atten ) ** 2
          F( New ) = G( IRight )
       END DO

       ! Interpolate or zero fill
       IF ( WnoInterp( I ) <= Wno( NWno ) ) THEN
          WnoT2 = ( WnoInterp( I ) + CI * AttInt )**2
          IF ( Option(3:3) == 'O' ) THEN
             Ginterp( I ) = PC(   WnoT2, X, F, ISize, ErrorMessage )   ! polynomial
          ELSE
             Ginterp( I ) = PADE( WnoT2, X, F, ISize, ErrorMessage )   ! Pade
          ENDIF

          IF ( ErrorMessage(1:6) /= '      ' ) WRITE( PRTFil, * ) ErrorMessage
       ELSE
          Ginterp( I ) = 0.0
       ENDIF
    END DO

    RETURN
  END SUBROUTINE INTERP
