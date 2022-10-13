PROGRAM BELLHOP

  ! Beam tracing in cylindrical coordinates
  ! Michael B. Porter and Homer P. Bucker

  USE bellMod
  USE RefCoMod
  USE bdryMod
  USE angleMod
  USE SdRdRMod
  USE ArrMod
  USE BeamPatternMod

  ! note ArrivalsStorage of 2000000 is about the max g95 could take in the allocate statement
  INTEGER, PARAMETER    :: SHDFIL = 25, RAYFIL = 21, ArrivalsStorage = 20000000
  REAL,    PARAMETER    :: DegRad = pi / 180.0
  INTEGER   IBPvec( 1 )
  REAL      xs( 2 ), gradc( 2 )
  COMPLEX,  ALLOCATABLE ::   U( :, : )
  COMPLEX   EPS, PICKEPS
  CHARACTER TITLE*80, BotOpt*3, RunType*4, BeamType*3

  CALL CPU_TIME( Tstart )

  ! Read in control data ***

  CALL READIN( TITLE, freq, ISINGL, &
       NIMAGE, IBWIN, deltas, MaxN, zBox, rBox, EPMULT, RLOOP,  &
       TopOpt, DepthT, CPT, RHOT, BotOpt, DepthB, CPB, RHOB, RunType, BeamType )

  CALL READATI(  TopOpt(5:5), DepthT, rBox, PRTFil )   	! READ AlTImetry
  CALL READBTY(  BotOpt(2:2), DepthB, rBox, PRTFil )      ! READ BaThYmetrY
  CALL READRC(   BotOpt(1:1), TopOpt(2:2),  PRTFil ) 	! READ Reflection Coefficients (top and bottom)
  CALL READPAT( RunType(3:3),               PRTFil )      ! Read Source Beam Pattern

  ! for a TL calculation, allocate space for the pressure matrix
  IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN
     ALLOCATE ( U( Nrd, Nr ), Stat = IAllocStat )
     IF ( IAllocStat /= 0 ) &
          CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory for TL matrix: reduce Nr * Nrd'  )
  ELSE
     ALLOCATE ( U( 1, 1 ), Stat = IAllocStat )
  ENDIF

  IF ( SCAN( 'Aa', RunType(1:1) ) /= 0 ) THEN
     MaxNArr = MAX( ArrivalsStorage / ( Nrd * Nr ), 10 )   ! allow space for at least 10 arrivals
     WRITE( PRTFIL, * )
     WRITE( PRTFIL, * ) '( Maximum # of arrivals = ', MaxNArr, ')'

     ALLOCATE ( AArr( Nrd, Nr, MaxNArr ), PhaseArr( Nrd, Nr, MaxNArr ), DelArr( Nrd, Nr, MaxNArr ), &
          SrcAngArr( Nrd, Nr, MaxNArr ), RcvrAngArr( Nrd, Nr, MaxNArr ), &
          NArr( Nrd, Nr ), NTopBncArr( Nrd, Nr, MaxNArr ), NBotBncArr( Nrd, Nr, MaxNArr ), Stat = IAllocStat )
     IF ( IAllocStat /= 0 ) &
          CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory to allocate arrivals matrix; reduce parameter ArrivalsStorage' )
  ELSE
     MaxNArr = 1
     ALLOCATE ( AArr( Nrd, Nr, 1 ), PhaseArr( Nrd, Nr, 1 ), DelArr( Nrd, Nr, 1 ), &
          SrcAngArr( Nrd, Nr, 1 ), RcvrAngArr( Nrd, Nr, 1 ), &
          NArr( Nrd, Nr ), NTopBncArr( Nrd, Nr, 1 ), NBotBncArr( Nrd, Nr, 1 ), Stat = IAllocStat )
  END IF

  omega  = 2.0 * pi * freq

  IF ( Nr > 1 ) THEN
     DeltaR = r( Nr ) - r( Nr - 1 )
  ELSE
     DeltaR = 0.0
  ENDIF

  alpha = DegRad * alpha   ! convert to radians
  Dalpha = 0.0
  IF ( NBeams /= 1 ) Dalpha = ( alpha( NBeams ) - alpha( 1 ) ) / ( NBeams - 1 )  ! angular spacing between beams

  ! *** Loop over source depths ***

  DO IS = 1, Nsd
     xs = (/ 0.0, sd( IS ) /)   ! source coordinate

     IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) U = 0.0    ! For a TL run, zero out pressure matrix
     IF ( SCAN( 'Aa',  RunType(1:1) ) /= 0 ) NArr = 0   ! For an arrivals run, zero out arrival matrix

     CALL SSP( xs, C, gradc, crr, crz, czz, TopOpt, 'TAB' )

     RadMax = 10 * C / freq  ! 10 wavelength max radius

     ! Are there enough beams?
     DalphaOpt = SQRT( C / ( 6.0 * freq * r( Nr ) ) )
     NBeamsOpt = 2 + ( alpha( NBeams ) - alpha( 1 ) ) / DalphaOpt

     IF ( RunType(1:1) == 'C' .AND. NBeams < NBeamsOpt ) THEN
        CALL ERROUT( PRTFIL, 'W', 'BELLHOP', 'Too few beams' )
        WRITE( PRTFIL, * ) 'NBeams should be at least = ', NBeamsOpt
     ENDIF

     ! *** Trace successive beams ***

     DO IBEAM = 1, NBeams


        IF ( ISINGL == 0 .OR. IBEAM == ISINGL ) THEN    ! Single beam run?

           alpha0 = alpha( IBEAM ) * 180.0 / pi   ! take-off angle in degrees
           IBPvec = maxloc( SrcBmPat( :, 1 ), mask = SrcBmPat( :, 1 ) < alpha0 )       ! index of ray angle in beam pattern
           IBP    = IBPvec( 1 )
           IBP = MAX( IBP, 1 )               ! don't go before beginning of table
           IBP = MIN( IBP, NSBPPts - 1 )     ! don't go past end of table
           ! linear interpolation to get amplitudeIsegBot( CrossBot ) )'
           s = ( alpha0 - SrcBmPat( IBP, 1 ) ) / ( SrcBmPat( IBP + 1, 1 ) - SrcBmPat( IBP, 1 ) )
           Amp0 = ( 1 - S ) * SrcBmPat( IBP, 2 ) + S * SrcBmPat( IBP + 1, 2 )

           WRITE( *, * ) 'Tracing beam ', IBEAM, alpha0
           CALL TRACE( deltas, xs, alpha( IBEAM ), Amp0, BeamType, zBox, rBox, BotOpt, RunType )   ! *** Trace a ray ***

           IF ( RunType(1:1) == 'R' ) THEN     ! Write the ray trajectory to RAYFIL
              CALL WRTRAY( alpha0, xv, Trayv, Nsteps, NumTopBnc( Nsteps ), NumBotBnc( Nsteps ), DepthT, DepthB )
           ELSE                                ! *** Compute the contribution to the field ***

              Eps = PICKEPS( BeamType(1:1), omega, C, CZ, alpha( IBEAM ), Dalpha, RLOOP, EPMULT ) ! 'optimal' beam constant

              SELECT CASE ( RunType(2:2) )
              CASE ( 'R' )
                 IBWIN2 = IBWIN **2
                 CALL INFLUR(   U, DeltaR, Eps, alpha( IBeam ), NImage, IBWin2, RunType, RadMax, BeamType )
              CASE ( 'C' )
                 IBWIN2 = IBWIN **2
                 CALL INFLUC(   U, DeltaR, Eps, alpha( IBeam ), NImage, IBWin2, RunType, RadMax, BeamType )
              CASE ( 'S' )
                 CALL INFLUSGB( U,  sd( IS ), alpha( IBeam ), RunType, Dalpha, deltas )
              CASE ( 'B' )
                 CALL INFLUGRB( U,  sd( IS ), alpha( IBeam ), RunType, Dalpha )
              CASE DEFAULT
                 CALL INFLUG(   U,  sd( IS ), alpha( IBeam ), RunType, Dalpha )
              END SELECT

           END IF
        END IF
     END DO ! Next beam

     ! *** write results to disk ***

     IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN   ! TL calculation
        CALL SCALEP( Dalpha, cV( 1 ), R, U, Nrd, Nr, RunType, TopOpt, freq )
        IRec  = 6 + Nrd * ( IS - 1 )
        DO I = 1, Nrd
           IRec = IRec + 1
           WRITE( SHDFil, REC = IRec ) ( U( I, J ), J = 1, Nr )
        END DO

     ELSE IF ( RunType(1:1) == 'A' ) THEN   ! arrivals calculation, ascii
        CALL WRTARRASC( R, Nrd, Nr, TopOpt, freq, RunType(4:4) )
     ELSE IF ( RunType(1:1) == 'a' ) THEN   ! arrivals calculation, binary
        CALL WRTARRBIN( R, Nrd, Nr, TopOpt, freq, RunType(4:4) )
     END IF

  END DO    ! Next source depth

  ! close all files

  IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN   ! TL calculation
     CLOSE( SHDFIL )
  ELSE IF ( RunType(1:1) == 'A' ) THEN   ! arrivals calculation, ascii
     CLOSE( ARRFIL )
  ELSE IF ( RunType(1:1) == 'a' ) THEN   ! arrivals calculation, binary
     CLOSE( ARRFIL )
  ELSE IF ( RunType(1:1) == 'R' ) THEN
     CLOSE( RAYFIL )
  END IF

  ! Display run time

  CALL CPU_TIME( Tstop )
  WRITE( PRTFIL, "( /, ' CPU Time = ', G15.3 )" ) Tstop - Tstart

  STOP
END PROGRAM BELLHOP

! **********************************************************************!

FUNCTION PICKEPS( BeamType, omega, C, CZ, alpha, Dalpha, RLOOP, EpsMult )

  ! Picks the optimum value for epsilon

  INTEGER, PARAMETER :: PRTFIL = 6
  COMPLEX, PARAMETER :: CI = ( 0.0, 1.0 )

  LOGICAL, SAVE :: INIFLG = .TRUE.
  COMPLEX   PICKEPS, EpsOpt
  CHARACTER BeamType*1, TAG*40

  SELECT CASE ( BeamType )

  CASE ( 'F' )
     TAG    = 'Space filling beams'
     HWIDTH = 2.0 / ( ( omega / C ) * Dalpha )
     EpsOpt = CI * 0.5 * omega * HWIDTH ** 2
  CASE ( 'M' )
     TAG    = 'Minimum width beams'
     HWidTH = SQRT( 2.0 * C * 1000.0 * RLOOP / omega )
     EpsOpt = CI * 0.5 * omega * HWIDTH ** 2
  CASE ( 'W' )
     TAG    = 'WKB beams'
     IF ( CZ == 0.0 ) THEN
        EpsOpt = 1.0E10
     ELSE
        EpsOpt = ( -SIN( alpha ) / COS( alpha ** 2 ) ) * C * C / CZ
     ENDIF
  CASE ( 'C' )
     TAG    = 'Cerveny style beam'
  END SELECT

  PICKEPS = EpsMult * EpsOpt

  ! *** On first call write info to prt file ***

  IF ( INIFLG ) THEN
     WRITE( PRTFIL, * ) TAG
     WRITE( PRTFIL, * ) 'HWIDTH  = ', HWIDTH
     WRITE( PRTFIL, * ) 'EpsOpt  = ', EpsOpt
     WRITE( PRTFIL, * ) 'EpsMult = ', EpsMult
     INIFLG = .FALSE.
  END IF

  RETURN
END FUNCTION PICKEPS

! **********************************************************************!

SUBROUTINE TRACE( deltas, xs, alpha, Amp0, BeamType, zBox, rBox, BotOpt, RunType )

  ! Traces the beam corresponding to a particular take-off angle

  USE bellMod
  USE bdryMod
  USE RefCoMod

  INTEGER   IsegTopT( 1 ), IsegBotT( 1 )
  REAL      xs( 2 ), gradc( 2 )
  CHARACTER BotOpt*3, BeamType*3, BC*1, RunType*4
  REAL (KIND=8) :: DbegTop( 2 ), DendTop( 2 ), DbegBot( 2 ), DendBot( 2 )

  ! *** Initial conditions ***

  CALL SSP( xs, C, gradc, crr, crz, czz, TopOpt, 'TAB' )

  NumTopBnc( 1 ) = 0
  NumBotBnc( 1 ) = 0
  cV(        1 ) = C
  xv(     :, 1 ) = xs
  TrayV(  :, 1 ) = (/ COS( alpha ), SIN( alpha ) /) / C
  pV(     :, 1 ) = (/ 1.0, 0.0 /)
  qV(     :, 1 ) = (/ 0.0, 1.0 /)
  tauV(      1 ) = 0.0
  AmpV(      1 ) = Amp0
  PhaseV(    1 ) = 0.0

  ! second component of qv is not used in geometric beam tracing
  ! set I.C. to 0 in hopes of saving run time
  IF (RunType(2:2) == 'G' ) qV( :,  1 ) = (/ 0.0, 0.0 /)

  ! *** identify the top segment above the source

  IsegTopT = MAXLOC( xTop( 1, 1:NatiPts), xTop( 1, 1:NatiPts) <= xs( 1 ) )

  IF ( IsegTopT( 1 ) > 0 .AND. IsegTopT( 1 ) < NatiPts ) THEN
     IsegTop  = IsegTopT( 1 )	! IsegTop MUST LIE IN [ 1, NatiPts-1 ]
  ELSE
     CALL ERROUT( PRTFIL, 'F', 'TRACE', 'Top altimetry undefined above the source' )
  ENDIF

  ! *** identify the bottom segment below the source

  IsegBotT = MAXLOC( xbot( 1, 1:NbtyPts), xbot( 1, 1:NbtyPts) <= xs( 1 ) )

  IF ( IsegBotT( 1 ) > 0 .AND. IsegBotT( 1 ) < NbtyPts ) THEN
     IsegBot  = IsegBotT( 1 )	! IsegBot MUST LIE IN [ 1, NbtyPts-1 ]
  ELSE
     CALL ERROUT( PRTFIL, 'F', 'TRACE', 'Bottom bathymetry undefined below the source' )
  ENDIF

  ! *** Trace the beam ***
  ! (note that REFLECT alters the step index I)

  I = 0
  DbegTop    = xv( :, 1 ) - xTop( :, IsegTop )  ! vector pointing from top    to ray
  DbegBot    = xv( :, 1 ) - xbot( :, IsegBot )  ! vector pointing from bottom to ray
  DistBegTop = DOT_PRODUCT( nTop( :, IsegTop ), DbegTop )
  DistBegBot = DOT_PRODUCT( nBot( :, IsegBot ), DbegBot )

  ! !!! note above distance is really a negative distance (throughout the code)

  IF ( DistBegTop >= 0 .OR. DistBegBot >= 0 ) THEN
     Nsteps = 1
     RETURN       ! source must be within the medium
  END IF

  STEPPING: DO ISTEP = 1, MaxN - 1
     I = I + 1

     CALL STEP( &
          xv( :, I   ), Trayv( :, I   ), pV(  :, I   ), qV( :, I   ), tauV( I   ), AmpV( I   ), PhaseV( I  ), cV( I   ),  &
          xv( :, I+1 ), Trayv( :, I+1 ), pV(  :, I+1 ), qV( :, I+1 ), tauV( I+1 ), AmpV( I+1 ), PhaseV( I+1), cV( I+1 ),  &
          xTop( :, IsegTop ), nTop( :, IsegTop ), &
          xBot( :, IsegBot ), nBot( :, IsegBot ), deltas, TopOpt )
     NumTopBnc( I + 1 ) = NumTopBnc( I )
     NumBotBnc( I + 1 ) = NumBotBnc( I )

     ! *** New altimetry segment? ***

     IF ( xv( 1, I + 1 ) < xTop( 1, IsegTop ) .OR. xv( 1, I + 1 ) > xTop( 1, IsegTop + 1 ) ) THEN
        IsegTopT = MAXLOC( xTop( 1, : ), xTop( 1, : ) < xv( 1, I + 1 ) )
        IF( IsegTopT( 1 ) > 0 .AND. IsegTopT( 1 ) < NatiPts ) IsegTop  = IsegTopT( 1 )	! IsegTop MUST LIE IN [ 1, NatiPts-1 ]
     END IF

     ! *** New bathymetry segment? ***

     IF ( xv( 1, I + 1 ) < xbot( 1, IsegBot ) .OR. xv( 1, I + 1 ) > xbot( 1, IsegBot + 1 ) ) THEN
        IsegBotT = MAXLOC( xbot( 1, : ), xbot( 1, : ) < xv( 1, I + 1 ) )
        IF( IsegBotT( 1 ) > 0 .AND. IsegBotT( 1 ) < NbtyPts ) IsegBot  = IsegBotT( 1 )	! IsegBot MUST LIE IN [ 1, NbtyPts-1 ]
     END IF

     ! *** Reflections? ***
     ! Tests that ray at step i is inside, and ray at step i+1 is outside
     ! to detect only a crossing from inside to outside

     DendTop    = xv( :, I + 1 ) - xTop( :, IsegTop )  ! vector pointing from top    to ray
     DendBot    = xv( :, I + 1 ) - xbot( :, IsegBot )  ! vector pointing from bottom to ray
     DistEndTop = DOT_PRODUCT( nTop( :, IsegTop ), DendTop )
     DistEndBot = DOT_PRODUCT( nBot( :, IsegBot ), DendBot )

     IF      ( DistBegTop < 0.0 .AND. DistEndTop >= 0.0 ) THEN
        BC = TopOpt(2:2)
        CALL REFLECT( I, BeamType, BC, CPT, RHOT, 'TOP', tTop( :, IsegTop  ), nTop( :, IsegTop ), &
             thetaTop, RTop, phiTop, NTopPTS )
        NumTopBnc( I + 1 ) = NumTopBnc( I ) + 1
        DendTop    = xv( :, I + 1 ) - xTop( :, IsegTop )  ! vector pointing from top    to ray
        DendBot    = xv( :, I + 1 ) - xbot( :, IsegBot )  ! vector pointing from bottom to ray
        DistEndTop = DOT_PRODUCT( nTop( :, IsegTop ), DendTop )
        DistEndBot = DOT_PRODUCT( nBot( :, IsegBot ), DendBot )
     ELSE IF ( DistBegBot < 0.0 .AND. DistEndBot >= 0.0 ) THEN  ! test bottom reflection
        BC = BotOpt(1:1)
        CALL REFLECT( I, BeamType, BC, CPB, RHOB, 'BOT', tBot( :, IsegBot  ), nBot( :, IsegBot ), &
             thetaBot, RBot, phiBot, NBotPTS )
        NumBotBnc( I + 1 ) = NumBotBnc( I ) + 1
        DendTop    = xv( :, I + 1 ) - xTop( :, IsegTop )  ! vector pointing from top    to ray
        DendBot    = xv( :, I + 1 ) - xbot( :, IsegBot )  ! vector pointing from bottom to ray
        DistEndTop = DOT_PRODUCT( nTop( :, IsegTop ), DendTop )
        DistEndBot = DOT_PRODUCT( nBot( :, IsegBot ), DendBot )
     END IF

     DBegTop    = DEndTop
     DbegBot    = DEndBot
     DistBegTop = DistEndTop
     DistBegBot = DistEndBot

     ! *** Has the ray left the box, lost its energy, or exceeded storage limit? ***

     IF ( ABS( xv( 1, I + 1 ) ) > rBox .OR. xv( 2, I + 1 ) > zBox .OR. AmpV( I + 1 ) < 0.005 ) THEN
        Nsteps = I + 1
        EXIT STEPPING
     ELSE IF ( I >= MaxN - 3 ) THEN
        CALL ERROUT( PRTFIL, 'W', 'TRACE', 'Insufficient storage for ray trajectory' )
        Nsteps = I
        EXIT STEPPING
     END IF
  END DO STEPPING   ! Next step

  RETURN
END SUBROUTINE TRACE

! **********************************************************************!

SUBROUTINE STEP( &
     x0, Tray0, P0, Q0, tau0, Amp0, Phase0, c0, &
     x2, Tray2, P2, Q2, tau2, Amp2, Phase2, c2, &
     xTop, nTop, xBot, nBot, deltas, TopOpt )

  ! Does a single step along the ray
  ! x denotes the ray coordinate, (r,z)
  ! Tray denotes the scaled tangent to the ray (previously (rho, zeta))

  USE sspMod

  REAL x(2), gradc0( 2 ), gradc1( 2 ), gradc2( 2 ), Amp0, Phase0, Amp2, Phase2
  REAL (KIND=8) :: &
       x0(2), TRay0(2), p0(2), q0(2), tau0, &
       x1(2), Tray1(2), p1(2), q1(2),       &
       x2(2), Tray2(2), p2(2), q2(2), tau2, &
       xTop(2), nTop(2), xBot(2), nBot(2), d( 2 ), e( 2 )
  CHARACTER TopOpt*4, Crossing*8

  ! *** Phase 1 of modified polygon method (an Euler step) ***

  CALL SSP( SNGL( x0 ), c0, gradc0, crr0, crz0, czz0, TopOpt, 'TAB' )

  Layer0 = Layer   	! make note of current layer

  csq0      = c0 * c0
  cnn0_csq0 = crr0 * Tray0(2)**2 - 2.0 * crz0 * Tray0(1) * Tray0(2) + czz0 * Tray0(1)**2

  h = deltas   		! initially set the step h, to the basic one, deltas
  x = x0 + H * C0 * Tray0 ! make a trial step

  ! *** Detect interface or boundary crossing

  Crossing = 'none'
  IF ( Tray0(2) /= 0.0 ) THEN
     IF      ( x(2) < zSSPV( Layer     ) .AND. Layer /=1       ) THEN
        Crossing = 'intabove'
     ELSE IF ( x(2) > zSSPV( Layer + 1 ) .AND. Layer /= NSSP-1 ) THEN
        Crossing = 'intbelow'
     END IF
  END IF

  d = x - xTop              ! vector from top to ray
  IF ( DOT_PRODUCT( nTop, d ) > 0.0 ) Crossing = 'Top'

  d = x - xBot              ! vector from bottom to ray
  IF ( DOT_PRODUCT( NBot, d ) > 0.0 ) Crossing = 'Bottom'

  ! *** Adjust step to land on an interface or boundary

  SELECT CASE ( Crossing )
  CASE ( 'intabove' )
     h = ( zSSPV( Layer     ) - x0(2) ) / ( Tray0(2) * c0 )
  CASE ( 'intbelow' )
     h = ( zSSPV( Layer + 1 ) - x0(2) ) / ( Tray0(2) * c0 )
  CASE ( 'Top' )
     e =  x0 - xTop         ! vector from top to ray origin
     h = -DOT_PRODUCT( e, nTop ) / ( c0 * DOT_PRODUCT( Tray0, nTop ) )
  CASE ( 'Bottom' )
     e =  x0 - xBot         ! vector bottom to ray
     h = -DOT_PRODUCT( e, nBot ) / ( c0 * DOT_PRODUCT( Tray0, nBot ) )
  CASE DEFAULT
  END SELECT

  h     = MAX( h, 1e4 * EPSILON( deltas ) * deltas )   ! make sure we make some motion
  halfh = 0.5 * h   ! first step of the modified polygon method is a half step

  x1    = x0    + halfh * c0 * Tray0
  Tray1 = Tray0 - halfh * gradc0 / csq0
  p1    = p0    - halfh * cnn0_csq0 * q0
  q1    = q0    + halfh * C0        * p0

  ! *** Phase 2 of modified polygon method ***

  CALL SSP( SNGL( x1 ), c1, gradc1, crr1, crz1, czz1, TopOpt, 'TAB' )

  csq1      = c1 * c1
  cnn1_csq1 = crr1 * Tray1(2)**2 - 2.0 * crz1 * Tray1(1) * Tray1(2) + czz1 * Tray1(1)**2

  SELECT CASE ( Crossing )
  CASE ( 'intabove' )
     h = ( zSSPV( Layer0     ) - x0(2) ) / ( Tray1(2) * c1 )
  CASE ( 'intbelow' )
     h = ( zSSPV( Layer0 + 1 ) - x0(2) ) / ( Tray1(2) * c1 )
  CASE ( 'Top' )
     h = -DOT_PRODUCT( e, nTop ) / ( c1 * DOT_PRODUCT( Tray1, nTop ) )
  CASE ( 'Bottom' )
     h = -DOT_PRODUCT( e, nBot ) / ( c1 * DOT_PRODUCT( Tray1, nBot ) )
  CASE DEFAULT
  END SELECT

  h = MAX( h, 1e4 * EPSILON( deltas ) * deltas )   ! make sure we make some motion

  x2     = x0    + h * c1 * Tray1
  Tray2  = Tray0 - h * gradc1 / csq1
  p2     = p0    - h * cnn1_csq1 * q1
  q2     = q0    + h * c1        * p1
  tau2   = tau0  + h / c1
  Amp2   = Amp0
  Phase2 = Phase0

  ! If we crossed an interface, apply jump condition

  CALL SSP( SNGL( x2 ), c2, gradc2, crr2, crz2, czz2, TopOpt, 'TAB' )

  IF ( Layer /= Layer0 ) THEN
     RN = -Tray2( 1 ) ** 2 / Tray2( 2 ) * ( gradc2( 2 ) - gradc0( 2 ) ) / c0   ! needs updating for c(r,z) problem
     p2 = p2 + q2 * RN
  END IF

  RETURN
END SUBROUTINE STEP

! **********************************************************************!

SUBROUTINE REFLECT( I, BeamType, BC, CHS, rhoHS, BOTTOP, tbdry, nbdry, theta, RefC, phi, Npts )

  USE bellMod
  USE sspMod
  USE RefCoMod

  REAL gradc( 2 )
  REAL (KIND=8) :: rhoHS, theInt, rInt, phiInt, tbdry( 2 ), nbdry( 2 ), CN, CS, RM, RN, Tg, Th
  REAL (KIND=8) :: theta( Npts ), RefC( Npts ), phi( Npts )
  COMPLEX gamma1, gamma2, gamma1SQ, gamma2SQ, GK, Refl
  COMPLEX (KIND=8) :: CHS, ch, a, b, d, sb, delta, ddelta
  CHARACTER BeamType*3, BC*1, BotTop*3

  I = I + 1
  NumTopBnc( I + 1 ) = NumTopBnc( I )
  NumBotBnc( I + 1 ) = NumBotBnc( I )

  ! *** here's the geometric part, changing the ray direction ***

  xv( :, I+1 ) = xv( :, I )

  Tg = DOT_PRODUCT( trayv( :, I ), TBdry )  ! component of ray tangent, along boundary
  Th = DOT_PRODUCT( trayv( :, I ), NBdry )  ! component of ray tangent, normal to boundary

  Trayv( :, I+1 ) =  Trayv( :, I ) - 2.0 * Th * NBdry

  ! *** calculate the change in curvature ***
  ! Based on formulas given by Muller, Geoph. J. R.A.S., 79 (1984).

  CALL SSP( SNGL( xv( :, I + 1 ) ), c, gradc, crr, crz, czz, TopOpt, 'TAB' )

  cV( I ) = c
  cn = gradc( 2 ) * Trayv( 1, I )
  cs = gradc( 2 ) * Trayv( 2, I )   ! assumes gradc( 2 ) = cr = 0

  IF ( BOTTOP == 'TOP' ) cn = -cn    ! flip sign for top reflection

  RM = Tg / Th
  RN = RM * ( 4 * cn - 2 * RM * cs ) / c

  !RN2 = -2.0 * RHOV( I + 1 )**2 / ZETAV( I + 1 ) * CZ / C  ! old formula for flat bottom

  SELECT CASE ( BeamType(2:2) )
  CASE ( 'D' )
     RN = 2.0 * RN
  CASE ( 'Z' )
     RN = 0.0
  END SELECT

  pV( :, I + 1 ) = pV( :, I ) + qV( :, I ) * RN
  qV( :, I + 1 ) = qV( :, I )

  ! *** account for phase change ***

  SELECT CASE ( BC )
  CASE ( 'R' )                 ! rigid
     tauV(   I + 1 ) = tauV(   I )
     AmpV(   I + 1 ) = AmpV(   I )
     PhaseV( I + 1 ) = PhaseV( I )
  CASE ( 'V' )                 ! vacuum
     tauV(   I + 1 ) = tauV(   I )
     AmpV(   I + 1 ) = AmpV(   I )
     PhaseV( I + 1 ) = PhaseV( I ) + pi
  CASE ( 'F' )                 ! file
     !theInt1 = RadDeg * ABS( ATAN2( ZETAV( I ), RHOV( I ) ) )   ! angle of incidence
     theInt = RadDeg * ABS( ATAN2( Th, Tg ) )   	! angle of incidence (relative to normal to bathymetry)
     IF ( theInt > 90 ) theInt = 180. - theInt 	! reflection coefficient is symmetric about 90 degrees
     CALL RefCO( theInt, rInt, phiInt, theta, RefC, phi, Npts, PRTFil )
     tauV(   I + 1 ) = tauV(   I )
     AmpV(   I + 1 ) = AmpV(   I ) * rInt
     PhaseV( I + 1 ) = PhaseV( I ) + phiInt
  CASE ( 'A' )                 ! half-space
     !GK     = omega * RHOV( I )   ! horizontal wavenumber
     GK     = omega * Tg   ! wavenumber in direction parallel to bathymetry
     gamma1SQ = ( omega / C   ) ** 2 - GK ** 2
     gamma2SQ = ( omega / CHS ) ** 2 - GK ** 2
     gamma1   = SQRT( -gamma1SQ )
     gamma2   = SQRT( -gamma2SQ )

     Refl = ( rhoHS * gamma1 - gamma2 ) / ( rhoHS * gamma1 + gamma2 )

     IF ( ABS( Refl ) < 1.0E-5 ) THEN   ! kill a ray that has lost its energy in reflection
        tauV(   I + 1 ) = tauV( I )
        AmpV(   I + 1 ) = 0.0
        PhaseV( I + 1 ) = PhaseV( I )
     ELSE
        tauV(   I + 1 ) = tauV( I )
        AmpV(   I + 1 ) = ABS( Refl ) * AmpV(  I )
        PhaseV( I + 1 ) = PhaseV( I ) + ATAN2( AIMAG( Refl ), REAL( Refl ) )

        ! compute beam-displacement Tindle, Eq. (14)
        ! needs a correction to beam-width as well ...
        !  IF ( REAL( gamma2SQ ) < 0.0 ) THEN
        !     RHOW   = 1.0   ! density of water
        !     RHOWSQ  = RHOW  * RHOW
        !     rhoHSSQ = rhoHS * rhoHS
        !     DELTA = 2 * GK * RHOW * rhoHS * ( gamma1SQ - gamma2SQ ) /
        ! &( gamma1 * CI * gamma2 *
        ! &( -RHOWSQ * gamma2SQ + rhoHSSQ * gamma1SQ ) )
        !     RV( I + 1 ) = RV( I + 1 ) + DELTA
        !  END IF

        if ( BeamType(3:3) == 'S' ) then   ! beam displacement & width change (Seongil's version)

           ch = cV( i ) / conjg( chs )
           co = Trayv( 1, i ) * cV( i )
           si = Trayv( 2, i ) * cV( i )
           ck = omega / cV( i )

           a = 2 * rhoHS * (1 - ch * ch )
           b = co * co - ch * ch
           d = rhoHS * rhoHS * si * si + b
           sb = sqrt( b )
           cco = co * co
           ssi = si * si

           delta = a * co / si / ( ck * sb * d )    
           pdelta = real( delta ) / ( cV( i ) / co)

           ddelta = -a / ( ck*sb*d ) - a*cco / ssi / (ck*sb*d) + a*cco / (ck*b*sb*d) &
                -a*co / si / (ck*sb*d*d) * (2*rhoHS*rhoHS*si*co-2*co*si)
           rddelta = -real( ddelta )
           sddelta = rddelta / abs( rddelta )        

           ! displacement and phase change
           xv(   1, i+1 ) = xv( 1,  i+1 ) + real( delta )
           tauV( i+1 )    = tauV( i+1 ) + pdelta

           ! beam width change
           qV( :, i+1 ) = qV( :, i+1 ) + sddelta * rddelta * si * c * pV( :, i )
        endif

     ENDIF
  END SELECT

  RETURN
END SUBROUTINE REFLECT

! **********************************************************************!

SUBROUTINE INFLUR( U, DELTAR, EPS, alpha, NIMAGE, IBWIN2, RunType, RadMax, BeamType )

  ! Computes the beam influence, i.e. 
  ! the contribution of a single beam to the complex pressure

  USE bellMod
  USE SdRdRMod

  LOGICAL   IOK
  INTEGER   KMAHV( MaxN )
  REAL      NA, NB, NSQ
  COMPLEX   PVB( MaxN ), QVB( MaxN ), Q, EPS, contri, U( Nrd, Nr ), gammaV( MaxN ), gamma
  CHARACTER RunType*4, BeamType*3

  ! Note that during reflection imag(q) is constant and
  ! adjacent normals cannot bracket a segment of the TL
  ! line, so no special treatment is necessary

  DS = 2.0*( SIN( omega * xv( 2, 1 ) * Trayv( 2, 1 ) ) )**2   ! Lloyd mirror pattern

  ! *** BEGIN BY FORMING (P, Q) AND KMAH INDEX ***

  ! Note treatment of KMAH index is incorrect for 'Cerveny' style beam width RunType

  IF ( BeamType(1:1) == 'C' ) EPS = CI * ABS( qV( 1, 1 ) / qV( 2, 1 ) )

  PVB( 1 ) = pV( 1, 1 ) + EPS * pV( 2, 1 )
  QVB( 1 ) = qV( 1, 1 ) + EPS * qV( 2, 1 )

  ! Following is sloppy

  gammaV( 1 ) = 0.0
  IF ( QVB( 1 ) /= 0 ) gammaV( 1 ) = PVB( 1 ) / QVB( 1 )
  KMAHV( 1 ) = 1

  DO I = 2, Nsteps
     IF ( BeamType(1:1) == 'C' ) EPS = CI * ABS( qV(1, I) / qV(2, I) )

     PVB( I )    = pV( 1, I ) + EPS * pV( 2, I )
     QVB( I )    = qV( 1, I ) + EPS * qV( 2, I )
     gammaV( I ) = PVB( I ) / QVB( I )
     KMAHV(  I ) = KMAHV( I - 1 )
     CALL BRCUT( QVB( I - 1 ), QVB( I ), BeamType, KMAHV( I ) )

  END DO

  DO id = 1, Nrd     ! Loop over receivers
     ZR = RD( id )

     DO IMAGE = 1, NIMAGE     ! Loop over images
        ir1 = 9999

        DO I = 1, Nsteps    ! Loop over steps

           ! *** Compute ray-centered coordinates, (ZNV, RNV) ***

           ZNV = -Trayv( 1, I ) * cV( I )
           IF ( ABS( ZNV ) < 1.0E-5 ) THEN   ! Check for normal parallel to TL-line
              IOK = .FALSE.
              CYCLE   ! skip to next step on ray
           ENDIF

           SELECT CASE ( IMAGE )     ! Images of beams
           CASE ( 1 )                ! True beam
              RNV = Trayv( 2, I ) * cV( I )
              NB  = ( ZR - xv( 2, I ) ) / ZNV
           CASE ( 2 )                ! Surface reflected beam
              RNV = -Trayv( 2, I ) * cV( I )
              NB  = ( ZR - ( 2.0 * DepthT - xv( 2, I ) ) ) / ZNV
           CASE ( 3 )                ! Bottom reflected beam
              RNV = -Trayv( 2, I ) * cV( I )
              NB  = ( ZR - ( 2.0 * DepthB - xv( 2, I ) ) ) / ZNV
           END SELECT

           RB = xv( 1, I ) + NB * RNV

           ! *** Compute influence for each rcvr ***

           IF ( RB > r( 1 ) ) THEN
              ir2 = MIN( INT( ( RB - r( 1 ) ) / DELTAR ) + 1, Nr )
           ELSE
              ir2 = MIN( INT( ( RB - r( 1 ) ) / DELTAR ), Nr )
           ENDIF

           IF ( ( ir2 > ir1 ) .AND. ABS( xv( 1, I ) - xv( 1, I - 1 ) ) > TINY( xv( 1, I ) ) .AND. IOK ) THEN
              DO ir = ir1 + 1, ir2
                 W = ( r( ir ) - RA ) / ( RB - RA )

                 Q     =     QVB(I-1) + W * (     QVB(I) -     QVB(I-1) )
                 gamma = gammaV(I-1) + W * ( gammaV(I) - gammaV(I-1) )

                 NSQ = ( NA + W * ( NB - NA ) )**2

                 IF ( AIMAG( gamma ) > 0 ) THEN
                    WRITE( PRTFIL, * ) 'Unbounded beam'
                    WRITE( PRTFIL, * ) PVB(I-1), QVB(I-1), PVB(I-1) / QVB(I-1)
                    WRITE( PRTFIL, * ) PVB(I  ), QVB(I  ), PVB(I  ) / QVB(I  )
                    WRITE( PRTFIL, * ) gamma, W
                 ELSE

                    ! Within beam window?
                    IF ( -0.5 * omega * AIMAG( gamma ) * NSQ < IBWIN2 ) THEN

                       C   =   cV( I-1 ) + W * (   cV( I ) -   cV( I-1 ) )
                       tau = tauV( I-1 ) + W * ( tauV( I ) - tauV( I-1 ) )

                       IF ( RunType(2:2) == 'C' ) EPS = CI * ABS( qV( 1, I ) / qV( 2, I ) )
                       contri = SQRT( C * ABS( EPS ) / Q * COS( alpha ) )* EXP( -CI * omega * ( tau + 0.5 * gamma * NSQ ) )

                       ! Get correct branch of SQRT
                       KMAH = KMAHV( I - 1 )
                       CALL BRCUT( qV( :, I - 1 ), Q, BeamType, KMAH )

                       IF ( KMAH  < 0 ) contri = -contri
                       IF ( IMAGE == 2 ) contri = -contri

                       SELECT CASE ( RunType(1:1) )
                       CASE ( 'I' )    ! Incoherent TL
                          contri =      ABS(contri)
                       CASE ( 'S' )    ! Semi-coherent TL
                          contri = DS * ABS(contri)
                       END SELECT

                       U( id, ir ) = U( id, ir ) + HERMIT( NA, RadMax, 2.0 * RadMax ) * contri
                    ENDIF
                 ENDIF
              END DO   ! next ir
           ELSE
              ! Next step may be ok
              IOK = .TRUE.
           ENDIF
           RA  = RB;   NA  = NB
           ir1 = MAX( ir2, 0 )
        END DO   ! Next step, I
     END DO   ! Next image
  END DO   ! Next receiver depth

  RETURN
END SUBROUTINE INFLUR
! **********************************************************************!
SUBROUTINE INFLUC( U, DELTAR, EPS, alpha, NIMAGE, IBWIN2, RunType, RadMax, BeamType )

  ! Computes the beam influence, i.e. 
  ! the contribution of a single beam to the complex pressure
  ! This version uses a beam representation in Cartesian coordinates

  USE bellMod
  USE SdRdRMod

  INTEGER   KMAHV( MaxN )
  REAL      x(2), Tray(2), gradc(2)
  COMPLEX   PVB( MaxN ), QVB( MaxN ), Q, EPS, contri, U( Nrd, Nr ), gammaV( MaxN ), gamma, const
  CHARACTER RunType*4, BeamType*3

  ! Note that during reflection imag(q) is constant and
  ! adjacent normals cannot bracket a segment of the TL
  ! line, so no special treatment is necessary

  DS = 2.0 * ( SIN( omega * xv(2,1) * Trayv( 2, 1 ) ) )**2   ! Lloyd mirror pattern

  ! *** Begin by forming (p, q) and KMAH index ***

  ! Note treatment of KMAH index is incorrect for 'Cerveny' style beam width BeamType

  DO I = 1, Nsteps
     IF ( BeamType(1:1) == 'C' ) EPS = CI * ABS( qV(1,I) / qV(2,I) )

     PVB( I ) = pV( 1, I ) + EPS * pV( 2, I )
     QVB( I ) = qV( 1, I ) + EPS * qV( 2, I )
     ! WRITE( *, * ) i, p1v( i ), q1v( i ), p2v( i ), q2v(i )

     !     RLTEMP = SQRT( -2.0 / ( omega * AIMAG( pV( I ) / qV( I ) ) ) )
     !     RKTEMP = -cV( I ) * REAL( pV( I ) / qV( I ) )

     !     WRITE( PRTFIL, * ) RLTEMP, RKTEMP

     !     IF ( BeamType(2:2) == 'D' ) THEN
     !  pV( I ) = REAL( pV( I ) )
     !  P = pV( I )
     !  L = 30.0 * 1500.0 / omega
     !  qV( I ) = ( CI * omega * L**2 / 2.0) * P
     !     ENDIF

     TR = Trayv( 1, I ) * cV(I)
     TZ = Trayv( 2, I ) * cV(I)

     CALL SSP( SNGL( xv( :, I ) ), C, gradc, crr, crz, czz, TopOpt, 'TAB')
     cr = gradc( 1 )
     cz = gradc( 2 )
     csq = C * C
     CS = CR * TR + CZ * TZ
     CN = CR * TZ - CZ * TR

     gammaV( I ) = 0.0
     IF ( QVB( I ) /= 0.0 ) gammaV( I ) = 0.5 * ( PVB(I) / QVB(I) * TR**2 + &
          2.0 * CN / csq * TZ * TR - CS / csq * TZ**2 )

     IF ( I == 1 ) THEN
        KMAHV( 1 ) = 1
     ELSE
        KMAHV( I ) = KMAHV( I - 1 )
        CALL BRCUT( QVB( I - 1 ), QVB( I ), BeamType, KMAHV( I ) )
     ENDIF

  END DO

  ! *** Loop over steps ***

  DO I = 3, Nsteps
     IF ( xv( 1, I ) > r( Nr ) ) RETURN
     RA = xv( 1, I-1 )
     RB = xv( 1, I )
     IF ( ABS( RB - RA ) < TINY( RB ) ) CYCLE   ! don't process duplicate points

     ! Compute upper index on rcvr line
     ir1 = MAX( MIN( INT( ( RA - r(1) ) / DELTAR ) + 1, Nr ), 1 )
     ir2 = MAX( MIN( INT( ( RB - r(1) ) / DELTAR ) + 1, Nr ), 1 )

     ! --- Following for projecting back reflected beams
     !  IF ( RV( I-2 ) == RV( I-1 ) ) ir1 = 1
     !  IF ( RV( I )   == RV( I+1 ) ) ir2 = Nr

     IF ( ir2 > ir1 ) THEN
        DO ir = ir1 + 1, ir2
           W = ( r( ir ) - RA ) / ( RB - RA )

           x     =    xv( :, I-1 ) + W * (    xv( :, I ) -      xv( :, I-1 ) )
           Tray  = TrayV( :, I-1 ) + W * ( TrayV( :, I ) -   TrayV( :, I-1 ) )
           C     =       cV( I-1 ) + W * (       cV( I ) -         cV( I-1 ) )
           Q     =     qV(1, I-1 ) + W * (    qV( 1, I ) -       qV( 1,I-1 ) )
           tau   =     tauV( I-1 ) + W * (     tauV( I ) -       tauV( I-1 ) )

           gamma = gammaV(I-1) + W * ( gammaV(I) - gammaV(I-1) )

           IF ( AIMAG( gamma ) > 0 ) THEN
              WRITE( PRTFIL, * ) 'Unbounded beam'
              WRITE( PRTFIL, * ) gammaV(I-1), gammaV(I), gamma
           ELSE

              IF ( BeamType(1:1) == 'C' ) EPS = CI * ABS( qV( 1, I ) / qV( 2, I ) )

              const = SQRT( C * ABS(   EPS   ) / Q * COS( alpha ) )

              ! Get correct branch of SQRT
              KMAH = KMAHV( I - 1 )
              CALL BRCUT( QVB( I - 1 ), Q, BeamType, KMAH )
              IF ( KMAH < 0 ) const = -const

              ! *** Loop over receivers ***

              DO id = 1, Nrd
                 ZR = RD( id )

                 ! True beam
                 deltaz = ZR - x( 2 )
                 !  IF ( omega * AIMAG( gamma )*deltaz**2 < IBWIN2 )
                 contri = HERMIT( deltaz, 0.0, RadMax ) * &
                      EXP( -CI * omega * ( tau + Tray(2) * deltaz + gamma * deltaz**2) )

                 ! Surface reflected beam
                 IF ( NIMAGE >= 2 ) THEN
                    deltaz = -ZR + 2.0 * DepthT - x( 2 )
                    IF ( omega * AIMAG( gamma )*deltaz**2 < IBWIN2 )    &
                         contri =  contri - HERMIT( deltaz, 0.0, RadMax ) *  &
                         EXP( -CI * omega * ( tau + Tray(2) * deltaz + gamma * deltaz**2) )
                 ENDIF

                 ! Bottom reflected beam
                 IF ( NIMAGE >= 3 ) THEN
                    deltaz = -ZR + 2.0 * DepthB - x( 2 )
                    IF ( omega * AIMAG( gamma )*deltaz**2 < IBWIN2 ) &
                         contri =  contri + HERMIT( deltaz, 0.0, RadMax ) * &
                         EXP( -CI * omega * ( tau + Tray(2) * deltaz + gamma * deltaz**2) )
                 ENDIF

                 ! contribution to field

                 SELECT CASE( RunType(1:1) )
                 CASE ( 'C' )   ! coherent
                    contri = const * contri
                 CASE ( 'I' )   ! incoherent
                    contri =ABS( const * contri )
                 CASE ( 'S' )   ! semi-coherent
                    contri = DS * ABS( const * contri )
                 END SELECT

                 U( id, ir ) = U( id, ir ) + contri

              END DO   ! Next receiver depth
           ENDIF
        END DO   ! Next receiver range
     ENDIF

  END DO   ! Next step along the ray

  RETURN
END SUBROUTINE INFLUC

! **********************************************************************!

SUBROUTINE INFLUG( U, zs, alpha, RunType, Dalpha )

  ! Computes the beam influence, i.e.
  ! the contribution of a single beam to the complex pressure
  ! This version uses a beam representation in Cartesian coordinates

  USE bellMod
  USE SdRdRMod
  USE ArrMod

  REAL      deltaz( Nrd ), Adeltaz( Nrd ), Amp( Nrd ), delay( Nrd ), x(2), Tray(2)
  COMPLEX   U(Nrd, * )
  CHARACTER RunType*4, RunTypeE*1

  ! some ugly code to have RunType 'a' treated like 'A'
  RunTypeE = RunType( 1 : 1 )
  IF ( RunTypeE == 'a' ) RunTypeE = 'A'

  DS       = SQRT( 2.0 ) * SIN( omega * zs * Trayv( 2, 1 ) )   ! Lloyd mirror pattern
  q0       = cV( 1 ) / Dalpha   	! Reference for J = Q0 / Q
  SrcAngle = RadDeg * alpha   	! take-off angle in degrees
  phase    = 0.0
  qOld     = 1.0
  RA       = xv( 1, 1 )
  ir       = 1
  IF ( RunType( 4:4 ) == 'R' ) THEN  ! point source
     RAT1 = SQRT( ABS( COS( alpha ) ) )
  ELSE
     RAT1 = 1
  END IF

  DO I = 2, Nsteps  ! Loop over steps
     RB = xv( 1, I )

     ! phase shifts at caustics
     q  = qV( 1, I-1 )
     IF ( q < 0.0 .AND. qOld >= 0.0 .OR. q > 0.0 .AND. qOld <= 0.0 ) phase = phase + pi / 2.  ! phase shifts at caustics
     qold = q

     DO WHILE ( ABS( RB - RA ) > TINY( RA ) .AND. RB > r( ir ) )  ! Loop over bracketted receiver ranges

        w = ( r( ir ) - RA ) / ( RB - RA )

        x     =    xv( :, I-1 ) + w * (    xv( :, I ) -      xv( :, I-1 ) )       ! ray coordinate
        Tray  = TrayV( :, I-1 ) + w * ( TrayV( :, I ) -   TrayV( :, I-1 ) )       ! ray tangent
        c     =       cV( I-1 ) + w * (       cV( I ) -         cV( I-1 ) )       ! sound speed
        q     =     qV(1, I-1 ) + w * (    qV( 1, I ) -       qV( 1,I-1 ) )       ! amplitude
        tau   =     tauV( I-1 ) + w * (     tauV( I ) -       tauV( I-1 ) )       ! delay

        tr     = Tray( 1 ) * c        	! component of ray-tangent in the radial direction

        IF ( q == 0 ) q = q0       ! avoid /0 at foci (e.g. source)
        A      = ABS( tr * q0 / q )
        RadMax = 1.0 / A          	! beam radius

        IF ( q < 0.0 .AND. qOld >= 0.0 .OR. q > 0.0 .AND. qOld <= 0.0 ) phase = phase + pi / 2.  ! phase shifts at caustics

        const = Rat1 * SQRT( C / ABS( Q ) ) * A * AmpV( I )
        IF ( RunTypeE == 'S' ) const = DS * const ! semi-coherent TL

        deltaz  = RD( 1 : Nrd ) - x( 2 )  ! ray to rcvr distance (need subscript because RD can be size Nrd+1)
        Adeltaz = ABS( deltaz )

        WHERE( Adeltaz < RadMax ) 	! amplitude, delay for the contributing beams only
           Amp    = const * ( RadMax - Adeltaz )
           delay  = tau + Tray( 2 ) * deltaz
        END WHERE

        DO id = 1, Nrd  ! Loop over receiver depths
           IF ( Adeltaz( id ) < RadMax ) THEN
              SELECT CASE( RunTypeE )
              CASE ( 'E' )      ! eigenrays
                 CALL WRTRAY( SrcAngle, xv, Trayv, Nsteps, NumTopBnc( I ), NumBotBnc( I ), DepthT, DepthB )
              CASE ( 'A' )      ! arrivals
                 RcvrAngle  = RadDeg * ATAN2( tray(2), tray(1) )
                 CALL AddArr( omega, id, ir, Amp( id ), PhaseV( I ) + phase, delay( id ), &
                      SrcAngle, RcvrAngle, NumTopBnc( I ), NumBotBnc( I ) )
              CASE ( 'C'  )     ! coherent TL
                 U( id, ir ) = U( id, ir ) + Amp( id ) * EXP( -CI * ( omega * delay( id ) - PhaseV( I ) - phase ) )
              CASE DEFAULT      ! incoherent/semi-coherent TL
                 W = ( RadMax - Adeltaz( id ) ) / RadMax   ! hat function: 1 on center, 0 on edge
                 U( id, ir ) = U( id, ir ) + ( Amp( id ) / W ) ** 2 * W
              END SELECT
           ENDIF
        END DO   ! Next receiver depth

        qOld = q
        ir   = ir + 1
        IF ( ir > Nr ) RETURN
     END DO   ! Next receiver range

     RA = RB
  END DO   ! Next step along the ray

  RETURN
END SUBROUTINE INFLUG


! **********************************************************************!

SUBROUTINE INFLUGRB( U, zs, alpha, RunType, Dalpha )

  ! Computes the beam influence, i.e.
  ! the contribution of a single beam to the complex pressure
  ! This version uses the 'GRAB' style of beam

  USE bellMod
  USE SdRdRMod
  USE ArrMod

  PARAMETER ( IBWin = 4 )   ! beam window: kills beams outside e**(-0.5 * ibwin**2 )

  REAL      lambda, deltaz( Nrd ), Adeltaz( Nrd ), Amp( Nrd ), delay( Nrd ), x( 2 ), Tray( 2 )
  COMPLEX   U(Nrd, * )
  CHARACTER RunType*4, RunTypeE*1

  ! some ugly code to have RunType 'a' treated like 'A'
  RunTypeE = RunType( 1 : 1 )
  IF ( RunTypeE == 'a' ) RunTypeE = 'A'

  DS       = SQRT( 2.0 ) * SIN( omega * zs * Trayv( 2, 1 ) )   ! Lloyd mirror pattern
  q0       = cV( 1 ) / Dalpha   ! Reference for J = Q0 / Q
  SrcAngle = RadDeg * alpha    ! take-off angle in degrees
  phase    = 0
  qOld     = 1.0
  RA       = xv( 1, 1 )
  ir       = 1

  IF ( RunType( 4:4) == 'R' ) THEN   ! point source
     RAT1 = SQRT( ABS( COS( alpha ) ) ) / 1.2535
  ELSE
     RAT1 = 1 / 1.2535 ! factor representing sum of Gaussians in free space
  END IF

  DO I = 2, Nsteps   ! *** Loop over steps ***

     RB = xv( 1, I )
     ! phase shifts at caustics
     q  = qV( 1, I-1 )
     IF ( q < 0.0 .AND. qOld >= 0.0 .OR. q > 0.0 .AND. qOld <= 0.0 ) phase = phase + pi / 2.  ! phase shifts at caustics
     qold = q

     DO WHILE ( ABS( RB - RA ) > TINY( RA ) .AND. RB > r( ir ) )   ! Loop over bracketted receiver ranges

        W = ( r( ir ) - RA ) / ( RB - RA )

        x     =    xv( :, I-1 ) + W * (    xv( :, I ) -      xv( :, I-1 ) )       ! ray coordinate
        Tray  = TrayV( :, I-1 ) + W * ( TrayV( :, I ) -   TrayV( :, I-1 ) )       ! ray tangent
        C     =       cV( I-1 ) + W * (       cV( I ) -         cV( I-1 ) )       ! sound speed
        q     =     qV(1, I-1 ) + W * (    qV( 1, I ) -       qV( 1,I-1 ) )       ! amplitude
        tau   =     tauV( I-1 ) + W * (     tauV( I ) -       tauV( I-1 ) )       ! delay

        IF ( q == 0 ) q = q0       ! avoid /0 at foci (e.g. source)
        tr     = Tray(1) * C          	! component of ray-tangent in radial direction
        A      = ABS( tr * Q0 / Q )
        RadMax = 1.0 / A          	! beam radius
        IF ( q < 0.0 .AND. qOld >= 0.0 .OR. q > 0.0 .AND. qOld <= 0.0 ) phase = phase + pi / 2. ! caustic phase shifts

        const = RAT1 * SQRT( C / ABS( Q ) ) * AmpV( I )
        IF ( RunType(1:1) == 'S' ) const = DS * const  ! semi-coherent TL

        deltaz  = RD( 1 : Nrd ) - x( 2 )   ! ray to rcvr distance (need subscript because RD can be size Nrd+1)
        Adeltaz = ABS( deltaz )

        ! calculate the beamwidth
        lambda = C / ( omega / ( 2 * pi ) )
        SigFac = 1.0
        sigma  = RadMax
        sigma  = MAX( RadMax, MIN( pi * r(ir) / 180.0 , SigFac * pi * lambda ) )
        !sigma  = MAX( RadMax, SigFac * pi * lambda )

        WHERE( Adeltaz < IBWin * sigma )  ! amplitude, delay for the contributing beams only
           Amp    = const * EXP( -0.5 * ( Adeltaz / sigma )**2 ) / ( 2. * sigma * A )
           delay  = tau + Tray( 2 ) * deltaz
        END WHERE

        DO id = 1, Nrd  ! Loop over receiver depths
           IF ( Adeltaz( id ) < IBWin * sigma ) THEN   ! Within beam window?
              SELECT CASE( RunTypeE )
              CASE ( 'E' )                ! eigenrays
                 CALL WRTRAY( SrcAngle, xv, Trayv, Nsteps, NumTopBnc( I ), NumBotBnc( I ), DepthT, DepthB )

              CASE ( 'A' )                ! arrivals
                 RcvrAngle  = RadDeg * ATAN2( tray(2), tray(1) )
                 CALL AddArr( omega, id, ir, Amp( id ), PhaseV( I ) + phase, delay( id ), &
                      SrcAngle, RcvrAngle, NumTopBnc( I ), NumBotBnc( I ) )
              CASE( 'C' )                 ! coherent TL
                 U( id, ir ) = U( id, ir ) + Amp( id ) * EXP( -CI * ( omega * delay( id ) - PhaseV( I ) - phase ) )
              CASE DEFAULT                ! incoherent/semicoherent TL
                 W =  EXP( -0.5 * ( Adeltaz( id ) / sigma )**2 ) / ( 2. * sigma * A )   ! Gaussian decay
                 U( id, ir ) = U( id, ir ) + ( Amp( id ) / W ) ** 2 * W
              END SELECT
           END IF
        END DO   ! Next receiver depth

        qOld = Q
        ir = ir + 1
        IF ( ir > Nr ) RETURN
     END DO   ! Next receiver range

     RA = RB
  END DO   ! Next step along the ray

  RETURN
END SUBROUTINE INFLUGRB


! **********************************************************************!

SUBROUTINE INFLUSGB( U, zs, alpha, RunType, Dalpha, deltas )

  ! Computes the beam influence, i.e. 
  ! the contribution of a single beam to the complex pressure
  ! This version uses a beam representation in Cartesian coordinates

  USE bellMod
  USE SdRdRMod
  USE ArrMod

  REAL      x( 2 ), Tray( 2 )
  COMPLEX   U( Nrd, Nr ), contri
  CHARACTER RunType*4

  RAT1  = SQRT(  COS( alpha ) )
  phase = 0
  qOld  = 1.0
  BETA  = 0.98  ! Beam Factor
  A     = -4.0 * LOG( BETA ) / Dalpha**2
  CN    = Dalpha * SQRT( A / pi )
  RA    = xv( 1, 1 )
  ir    = 1

  DO I = 2, Nsteps    ! *** Loop over steps ***

     RB = xv( 1, I )
     ! phase shifts at caustics
     q  = qV( 1, I-1 )
     IF ( q < 0.0 .AND. qOld >= 0.0 .OR. q > 0.0 .AND. qOld <= 0.0 ) phase = phase + pi / 2.  ! phase shifts at caustics
     qold = q

     DO WHILE ( ABS( RB - RA ) > TINY( RA ) .AND. RB > r( ir ) )   ! *** Loop over bracketted receiver ranges ***

        W = ( r( ir ) - RA ) / ( RB - RA )
        x     =    xv( :, I-1 ) + W * (    xv( :, I ) -      xv( :, I-1 ) )
        Tray  = TrayV( :, I-1 ) + W * ( TrayV( :, I ) -   TrayV( :, I-1 ) )
        q     =     qV(1, I-1 ) + W * (    qV( 1, I ) -       qV( 1,I-1 ) )
        tau   =     tauV( I-1 ) + W * (     tauV( I ) -       tauV( I-1 ) )

        SINT  =  (I-1) * deltas + W * deltas

        IF ( Q < 0.0 .AND. qOld >= 0.0 .OR. Q > 0.0 .AND. qOld <= 0.0 ) phase = phase + pi / 2. ! phase shifts at caustics

        DO id = 1, Nrd   ! *** Loop over receiver depths ***
           deltaz =  RD( id ) - x( 2 )   ! ray to rcvr distance
           !         Adeltaz    = ABS( deltaz )
           !         IF ( Adeltaz < RadMax ) THEN
           SELECT CASE( RunType(1:1) )
           CASE ( 'E' )         ! eigenrays
              SrcAngle = RadDeg * alpha   ! take-off angle in degrees
              CALL WRTRAY( SrcAngle, xv, Trayv, Nsteps, NumTopBnc( I ), NumBotBnc( I ), DepthT, DepthB )

           CASE DEFAULT         ! coherent TL
              CPA    = ABS( deltaz * ( RB - RA ) ) / SQRT( ( RB - RA )**2 + (xv(2,I) - xv(2,I-1))**2  )
              DS     = SQRT( deltaz**2 - CPA**2 )
              SX     = SINT + DS
              theta  = ATAN( CPA / SX )
              delay  = tau + Tray(2) * deltaz
              contri = RAT1 * CN * AmpV( I ) * EXP(-A * theta**2 - CI * ( omega * delay - PhaseV( I ) - phase ) ) / SQRT( SX )
              !             write( *, * ) i, id, cpa, ds, sx, theta, delay, a, cn, contri
              U( id, ir ) = U( id, ir ) + contri

           END SELECT
           !        ENDIF
        END DO   ! Next receiver depth

        qOld = Q
        ir = ir + 1
        IF ( ir > Nr ) RETURN
     END DO   ! Next receiver range

     RA = RB
  END DO   ! Next step along the ray

  RETURN
END SUBROUTINE INFLUSGB

! **********************************************************************!

SUBROUTINE BRCUT( Q1C, Q2C, BeamType, KMAH )

  ! Checks for a branch cut crossing

  COMPLEX   Q1C, Q2C
  CHARACTER BeamType*3

  SELECT CASE ( BeamType(1:1) )
  CASE ( 'W' )   ! WKBeams
     Q1 = REAL( Q1C )   ;   Q2 = REAL( Q2C )
     IF    ( ( Q1 < 0.0 .AND. Q2 >= 0.0 ) .OR. ( Q1 > 0.0 .AND. Q2 <= 0.0 ) ) KMAH = -KMAH
  CASE DEFAULT
     IF ( REAL( Q2C ) < 0.0 ) THEN
        Q1 = AIMAG( Q1C )   ;   Q2 = AIMAG( Q2C )
        IF ( ( Q1 < 0.0 .AND. Q2 >= 0.0 ) .OR. ( Q1 > 0.0 .AND. Q2 <= 0.0 ) ) KMAH = -KMAH
     ENDIF
  END SELECT

  RETURN
END SUBROUTINE BRCUT

! **********************************************************************!

FUNCTION HERMIT( X, X1, X2 )

  ! Calculates a smoothing function based on the h0 hermite cubic

  AX  = ABS( X  )
  AX1 = ABS( X1 )
  AX2 = ABS( X2 )

  IF ( AX <= AX1 ) THEN
     HERMIT = 1.0
  ELSE IF ( AX >= AX2 ) THEN
     HERMIT = 0.0
  ELSE
     U = ( AX - AX1 ) / ( AX2 - AX1 )
     HERMIT = ( 1.0 + 2.0 * U ) * ( 1.0 - U )**2
  ENDIF

  hermit = 1.0
  !  hermit = hermit / ( 0.5 * ( x1 + x2 ) )

  RETURN
END FUNCTION HERMIT

! **********************************************************************!

SUBROUTINE SCALEP( Dalpha, c, r, U, Nrd, Nr, RunType, TopOpt, freq )

  ! Scale the pressure field

  REAL, PARAMETER :: pi = 3.14159265
  REAL      r( Nr )
  COMPLEX   U( Nrd, Nr )
  CHARACTER TopOpt*4, RunType*4

  ! Compute scale factor for field

  SELECT CASE ( RunType(2:2) )
  CASE ( 'C' )
     const = -Dalpha * SQRT( freq ) / c
  CASE ( 'R' )
     const = -Dalpha * SQRT( freq ) / c
  CASE DEFAULT
     const = -1.0
  END SELECT

  ! *** Thorpe attenuation? ***

  IF ( TopOpt(4:4) == 'T' ) THEN
     f2 = ( freq / 1000.0 ) ** 2
     alpha = 40.0 * f2 / ( 4100.0 + f2 ) + 0.1 * f2 / ( 1.0 + f2 )
     alpha = alpha / 914.4     ! dB / m
     alpha = alpha / 8.6858896 ! Nepers / m
  ELSE
     alpha = 0.0
  ENDIF

  ! For incoherent RunType, convert intensity to pressure
  IF ( RunType(1:1) /= 'C' ) U = SQRT( REAL( U ) )

  ! add in attenuation

  DO ir = 1, Nr
     IF ( RunType(4:4) == 'X' ) THEN   ! line source
        factor = 4.0 * SQRT( pi )
     ELSE                             ! point source
        IF ( r ( ir ) == 0 ) THEN
           factor = 1e5       ! avoid /0 at origin
        ELSE
           factor = const * EXP( -alpha * r( ir ) ) / SQRT( r( ir ) )
        END IF
     END IF
     U( :, ir ) = factor * U( :, ir )
  END DO

  RETURN
END SUBROUTINE SCALEP

! **********************************************************************!

SUBROUTINE WRTRAY( alpha0, xv, Trayv, Nsteps, NumTopBnc, NumBotBnc, DepthT, DepthB )

  ! Compress the ray data keeping every ISKIP point, points near surface or bottom, and last point.
  ! Write to RAYFIL.

  INTEGER, PARAMETER :: RAYFIL = 21

  INTEGER ( KIND = 2 ) :: NumTopBnc, NumBotBnc
  REAL (KIND=8) :: xv( 2, * ), Trayv( 2, * )

  ! compression

  N2 = 1
  ISKIP = MAX( Nsteps / 5000, 1 )   ! max #pts written is about 5000

  DO I = 2, Nsteps
     ! following ensures that we always write ray pts. near bdry reflections
     IF ( MIN( DepthB - xv( 2, I ),  xv( 2, I ) - DepthT ) < 0.2 .OR. &
          MOD( I, ISKIP ) == 0 .OR. I == Nsteps ) THEN
        N2 = N2 + 1
        xv( :, N2 ) = xv( :, I )
     END IF
  END DO

  ! write to ray file

  WRITE( RAYFIL, * ) alpha0
  WRITE( RAYFIL, * ) N2, NumTopBnc, NumBotBnc
  DO I = 1, N2
     WRITE( RAYFIL, * ) SNGL( xv( :, I ) ) ! , SNGL( Trayv( 2, I ) )
  END DO

  RETURN
END SUBROUTINE WRTRAY

