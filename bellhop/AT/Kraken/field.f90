PROGRAM FIELD

  ! Generate file of replica vectors

  USE SdRdRMod

  INTEGER, PARAMETER :: FLPFil = 5, PrtFil = 6, ShdFil = 25, MaxM = 10000  ! MaxM also occurs in Eval, EvalAD, EvalCM
  COMPLEX   ck( MaxM )
  CHARACTER Opt*50, SHDTitle*80, Title*80, Comp*1, PlotType*10, FileName*6
  REAL,    ALLOCATABLE :: rr( : ), rProf( : )
  COMPLEX, ALLOCATABLE :: phiS( :, : ), phiR( :, : ), P( :, : )

  SHDTitle(1:1) = '$'

  READ( FLPFil, * ) SHDTitle
  READ( FLPFil, * ) Opt
  READ( FLPFil, * ) MLimit

  Comp   = Opt( 3:3 )
  MLimit = MIN( MaxM, MLimit )

  ! *** Read profile ranges ***

  READ( FLPFil, * ) NProf
  ALLOCATE( rProf( NProf + 1 ), Stat = IAllocStat )   ! NProf + 1 profiles (one added at `infinite' range)

  IF ( NProf > 2 ) rProf( 3 ) = -999.9
  READ( FLPFil, * ) ( rProf( I ), I = 1, NProf )

  IF ( IAllocStat /= 0 ) THEN
     WRITE( PrtFil, * ) 'NProf = ', NProf
     CALL ERROUT( PrtFil, 'F', 'FIELD', 'Too many profiles' )
  ENDIF

  ! EVALAD/EVALCM need a profile at zero range
  IF ( rProf( 1 ) /= 0.0 ) CALL ERROUT( PrtFil, 'F', 'FIELD', 'The first profile must be at 0 km' )

  CALL SUBTAB( rProf, NProf )
  CALL RANGES( FLPFil, PrtFil )           ! Read receiver ranges
  zMin = -1.0E10
  zMax = +1.0E10
  CALL SDRD( FLPFil, PrtFil, zMin, zMax ) ! Read source/receiver depths

  ALLOCATE( phiS( MaxM, Nsd ), phiR( MaxM, Nrd ) )

  ! *** Read receiver ranges (offsets from vertical) ***

  READ( FLPFil, * ) Nrr

  IF ( Nrr /= Nrd ) THEN
     WRITE( PrtFil, * ) 'Nrr, Nrd = ', Nrr, Nrd
     CALL ERROUT( PrtFil, 'W', 'FIELD', 'Nrr being set to Nrd' )
     Nrr = Nrd
  ENDIF

  ALLOCATE( rr( Nrr ) )
  IF ( Nrr > 1 ) rr( 2 ) = -999.9
  IF ( Nrr > 2 ) rr( 3 ) = -999.9
  READ( FLPFil, * ) ( rr( IR ), IR = 1, Nrr )

  CALL SUBTAB( rr, Nrd )
  WRITE( PrtFil, * )
  WRITE( PrtFil, * ) 'Number of receivers = ', Nrd
  IF ( Nrd >= 1 ) WRITE( PrtFil,  "( 5G14.6 )" ) ( rr( IR ), IR = 1, MIN( Nrd, 51 ) )

  WRITE( PrtFil, * )

  ALLOCATE ( P( Nrd, Nr ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) STOP "Fatal Error: Insufficient memory to allocate P( Nrd, Nr )"

  !  *** Read in modes ***

  IPROF = 1
  CALL GETMOD( IPROF, ' ', MaxM, sd, Nsd, 'N' , ck, phiS, MSrc, freq, Title )
  CALL GETMOD( IPROF, ' ', MaxM, rd, Nrd, Comp, ck, phiR, MSrc, freq, Title )

  ! Generate header
  FileName = 'SHDFIL'
  IF ( SHDTitle(1:1) == '$' ) SHDTitle = Title
  PlotType = '          '
  atten = 0.0
  CALL WriteHeader( FileName, SHDTitle, sd, Nsd, rd, Nrd, R, Nr, freq, atten, PlotType, 0.0, 0.0, 0.0 )
  iRec = 6

  ! *** MAIN LOOP: For each source evaluate and write the field ***

  DO IS = 1, Nsd
     M = MIN( MLimit, MSrc )   ! Set number of propagating modes

     IF    ( NProf == 1      ) THEN   ! Range-independent case
        CALL EVAL(                 phiS( 1:MSrc, IS ), phiR,     Nrd, R, Nr, rr, ck, M, Opt, P )
     ELSE
        IF ( Opt(2:2) == 'C' ) THEN   ! Range-  dependent case
           ! Coupled mode theory
           CALL EVALCM( rProf, NProf, phiS( 1:MSrc, IS ), phiR, rd, Nrd, R, Nr,     ck, M, Opt, P )
        ELSE
           ! Adiabatic mode theory
           CALL EVALAD( rProf, NProf, phiS( 1:MSrc, IS ), phiR, rd, Nrd, R, Nr,         M, Opt, P )
        ENDIF
     ENDIF

     ! write out the field
     DO I = 1, Nrd
        IRec = IRec + 1
        WRITE( SHDFil, REC = IRec ) ( P( I, J ), J = 1, Nr )
     END DO

  END DO   ! next source depth
  CLOSE( ShdFil )

  STOP
END PROGRAM FIELD
