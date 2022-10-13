PROGRAM SCOOTER

  ! Finite element fast-field program
  ! Michael B. Porter   8/85

  USE SdRdRMod
  USE scomod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  COMPLEX, ALLOCATABLE :: Green( :, :, : )     ! this storage space is used to store a matrix G( Nsd, Nrd, Nk )
  CHARACTER Title*80, PlotType*10, FileName*6
  REAL      kMin, kMax
  REAL,             ALLOCATABLE :: RK( : )
  REAL    (KIND=8), ALLOCATABLE :: rho( : )
  COMPLEX (KIND=8), ALLOCATABLE :: B1(:), B2(:), B3(:), B4(:)

  CALL CPU_TIME( Tstart )
  CALL GETPAR( Title, Freq )

  IF ( NMedia > 1 ) THEN
     IF ( ANY( Sigma( 2:NMedia ) /= 0.0 ) ) CALL ERROUT( PRTFil, 'F', 'SCOOTER', 'Rough interfaces not allowed' )
  ENDIF

  ! *** Set up vector of wavenumber samples ***

  kMin = SQRT( omega2 ) / CHigh
  kMAX = SQRT( omega2 ) / CLow

  Nk = 1000.0 * RMax * ( kMax - kMin ) / PI
  WRITE( PRTFil, * ) 'Nk = ', Nk

  ALLOCATE( RK( Nk ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFil, 'F', 'SCOOTER', 'Insufficient memory to allocate RK( Nk ) vector' )

  ! Set-up the vector of k-space points
  DeltaK = ( kMax - kMin ) / ( Nk - 1 )
  Atten  = DeltaK
  RK     = kMin + (/ ( IK, IK = 0, Nk - 1 ) /) * DeltaK

  ALLOCATE ( Green( Nsd, Nrd, Nk ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) &
       CALL ERROUT( PRTFil, 'F', 'SCOOTER', 'Insufficient memory to allocate Green''s function matrix; reduce Rmax, Nsd, or Nrd' )

  H( 1:NMedia ) = ( Depth( 2:NMedia + 1 ) - Depth( 1:NMedia ) ) / N( 1:NMedia )  ! vector of mesh widths
  NPts  = SUM( N( 1:NMedia ) ) + NMedia     ! number of solution points

  ALLOCATE ( B1( NPts ), B2( NPts ), B3( NPts ), B4( NPts ), rho( NPts ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFil, 'F', 'SCOOTER', 'Insufficient memory to allocate B1, B2, B3, B4 vectors' )

  ! Write header for Green's function file

  FileName = 'GRNFIL'
  PlotType = 'Green'
  XS = 0.0   ;   YS = 0.0   ;   Theta = 0.0
  CALL WriteHeader( FileName, Title, sd, Nsd, rd, Nrd, RK, Nk, REAL( Freq ), REAL( Atten ), PlotType, &
       REAL( XS ), REAL( YS ), REAL( Theta ) )

  CALL INIT( B1, B2, B3, B4, rho, NPts )    ! Initialize matrices

  NTot1 = SUM( N( NFirstAcoustic:NLastAcoustic ) ) + 1   	! size of matrix for acoustic part
  CALL KERNEL( B1, B2, B3, B4, rho, NPts, RK, Green, NTot1 )

  CALL CPU_TIME( Tend )
  WRITE( PRTFil, "(' CPU TIME: ', G15.5)" ) Tend - Tstart

  STOP
END PROGRAM SCOOTER
!**********************************************************************!
SUBROUTINE GetPar( Title, Freq )

  !     Read in the ENVFil data

  USE scomod
  USE SdRdRMod
  USE RefCoMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  CHARACTER        Title*80
  REAL (KIND=4) :: ZMin, ZMAX

  Title = 'SCOOTER- '
  CALL READIN( Title, Freq, MaxMedium, NMedia, &
       TopOpt, cpT, csT, rhoT, BumDen, eta, xi, N, Sigma, Depth, &
       BotOpt, cpB, csB, rhoB, ENVFil, PRTFil )

  READ(  ENVFil, *    ) CLow, CHigh                 ! Spectral limits
  WRITE( PRTFil, "( /, ' CLow = ', G12.5, '  CHigh = ', G12.5 )" ) CLow, CHigh
  IF ( CLow <= 0.0 .OR. CHigh <= 0.0 .OR. CLow >= CHigh ) &
       CALL ERROUT( PRTFil, 'F', 'GETPAR', 'Need phase speeds CLow, CHigh > 0 and CLow < CHigh'  )

  READ(  ENVFil, * ) RMax                           ! Maximum range for calculations
  WRITE( PRTFil, * ) 'RMax = ', RMax
  IF ( RMax <= 0.0 ) CALL ERROUT( PRTFil, 'F', 'GETPAR', 'RMax must be positive'  )

  ZMin = Depth( 1 )
  ZMAX = Depth( NMedia + 1 )
  CALL SDRD( ENVFil, PRTFil, ZMin, ZMAX )           ! Read source/receiver depths

  CLOSE ( ENVFil )
  omega2 = ( 2.0 * PI * Freq ) ** 2

  CALL ReadRC( BotOpt(1:1), TopOpt(2:2), PRTFil )   ! Optionally read in Bot, Top reflection coefficients

  RETURN
END SUBROUTINE GetPar
!**********************************************************************!
SUBROUTINE INIT( B1, B2, B3, B4, rho, NPts )

  ! Initializes arrays defining difference equations

  USE scomod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  REAL    (KIND=8) :: rho( NPts ), cMinV
  COMPLEX (KIND=8) :: cp( NPts ), cs( NPts ), cp2, cs2
  COMPLEX (KIND=8) :: B1( NPts ), B2( NPts ), B3( NPts ), B4( NPts )
  CHARACTER Task*8

  cMin              = 1.0E6
  NFirstAcoustic    = 0
  Loc( 1 )          = 0

  ! *** Loop over media ***

  DO Medium = 1, NMedia


     IF ( Medium /= 1 ) Loc( Medium ) = Loc( Medium - 1 ) + N( Medium - 1 ) + 1

     N1 = N(   Medium ) + 1
     I  = Loc( Medium ) + 1

     Task = 'TAB'
     CALL Profil( Depth, cp( I ), cs( I ), rho( I ), Medium, N1, Freq, TopOpt(1:1), TopOpt(3:4), Task, ENVFIL, PRTFIL )

     IF ( cs( I ) == ( 0.0, 0.0 ) ) THEN   ! *** Case of an acoustic medium ***

        Mater( Medium )  =  'ACOUSTIC'
        IF ( NFirstAcoustic == 0 ) NFirstAcoustic = Medium
        NLastAcoustic = Medium

        cMinV = MINVAL( DBLE( cp( I:I + N( Medium ) ) ) )
        cMin  = MIN( cMin, cMinV )
        B1( I:I + N( Medium ) ) = omega2 / cp( I:I + N( Medium ) ) ** 2

     ELSE                                  ! *** Case of an elastic medium ***

        Mater( Medium ) = 'ELASTIC'
        TwoH         = 2.0 * H( Medium )

        DO j = I, I + N( Medium )
           cMin = MIN( DBLE( cs( j ) ), cMin )

           cp2 = cp( j ) ** 2
           cs2 = cs( j ) ** 2

           B1( j ) = TwoH / ( rho( j ) * cs2 )
           B2( j ) = TwoH / ( rho( j ) * cp2 )
           B3( j ) = 4.0 * TwoH * rho( j ) * cs2 * ( cp2 - cs2 ) / cp2
           B4( j ) = TwoH * ( cp2 - 2.0 * cs2 ) / cp2

           rho( j ) = TwoH * omega2 * rho( j )
        END DO

     ENDIF
  END DO   ! next Medium

  RETURN
END SUBROUTINE INIT
!**********************************************************************!
SUBROUTINE BCImp( B1, B2, B3, B4, rho, NPts, X, BCType, BotTop, cpHS, csHS, rhoHS, F, G, IPow )

  !     Compute Boundary Condition IMPedance
  !     Same subroutine as in KRAKENC except
  !        PEKRT is replaced by SQRT
  !        COMC  is replaced by COMSCO
  !        cIns  is related to B1 differently

  USE scomod
  USE RefCoMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  INTEGER, INTENT( OUT ) :: IPow
  REAL    (KIND=8) :: rho( NPts ), rhoHS, C0
  COMPLEX (KIND=8) :: X, KX, KZ, Twersk, gammaS2, gammaP2, gammaS, gammaP, RMU, YV( 5 ), RCmplx
  COMPLEX (KIND=8) :: cpHS, csHS, cIns
  COMPLEX (KIND=8) :: B1( NPts ), B2( NPts ), B3( NPts ), B4( NPts )
  COMPLEX (KIND=8), INTENT( OUT ) :: F, G
  CHARACTER BCType*1, BotTop*3

  IPow = 0

  ! *** Get rho, C just INSide the boundary ***

  IF ( BotTop(1:3) == 'TOP' ) THEN
     I = 1
     rhoIns = rho( I )
     cIns   = SQRT( omega2 / B1( 1 ) )
  ELSE
     I = Loc( NLastAcoustic ) + N( NLastAcoustic ) + 1
     rhoIns = rho( I )
     cIns   = SQRT( omega2 / B1( I ) )
  ENDIF

  ! *** impedance for different bottom types ***

  IF ( BCType(1:1) == 'V' ) THEN  ! Vacuum with Kirchoff roughness
     F     = 1.0
     G     = -CI * SQRT( omega2 / cIns ** 2 - X ) * Sigma( 1 ) ** 2
     YV(1) = F
     YV(2) = G
     YV(3) = 0.0
     YV(4) = 0.0
     YV(5) = 0.0
  ENDIF

  IF ( BCType(1:1) == 'S' .OR. BCType(1:1) == 'H' .OR. &
       BCType(1:1) == 'T' .OR. BCType(1:1) == 'I' ) THEN  ! Vacuum with Twersky scatter model

     omega = SQRT( omega2 )
     KX    = SQRT( X )
     F     = 1.0
     C0    = REAL( cIns )
     G     = Twersk( BCType, omega, BumDen, xi, eta, KX, rhoIns, C0 )
     G     = G / ( CI * omega * rhoIns )
     YV(1) = F
     YV(2) = G
     YV(3) = 0.0
     YV(4) = 0.0
     YV(5) = 0.0
  ENDIF

  IF ( BCType(1:1) == 'R' ) THEN   ! Rigid
     F     = 0.0
     G     = 1.0
     YV(1) = F
     YV(2) = G
     YV(3) = 0.0
     YV(4) = 0.0
     YV(5) = 0.0
  ENDIF

  ! *** Acousto-elastic half-space ***

  IF ( BCType(1:1) == 'A' ) THEN
     IF ( REAL( csHS ) > 0.0 ) THEN
        gammaS2 = X - omega2 / csHS ** 2
        gammaP2 = X - omega2 / cpHS ** 2
        gammaS  = SQRT( gammaS2 )
        gammaP  = SQRT( gammaP2 )
        RMU   = rhoHS * csHS ** 2

        YV(1) = ( gammaS*gammaP - X ) / RMU
        YV(2) = ( ( gammaS2 + X ) ** 2 - 4.0 * gammaS * gammaP * X ) * RMU
        YV(3) = 2.0*gammaS*gammaP - gammaS2 - X
        YV(4) = gammaP * ( X - gammaS2 )
        YV(5) = gammaS * ( gammaS2 - X )

        F = omega2 * YV( 4 )
        G = YV( 2 )
     ELSE
        gammaP = SQRT( X - omega2 / cpHS**2 )
        F    = 1.0
        G    = rhoHS / gammaP
     ENDIF
  ENDIF

  ! *** Tabulated reflection coefficient ***

  IF ( BCType(1:1) == 'F' ) THEN
     ! Compute the grazing angle Theta
     KX     = SQRT( X )
     KZ     = SQRT( omega2 / cIns**2 - KX**2 )
     RadDeg = 180.0 / PI
     TheInt = RadDeg * ATAN2( REAL( KZ ), REAL( KX ) )

     ! Evaluate R( TheInt )
     IF ( BotTop(1:3) == 'TOP' ) THEN
        CALL RefCo( TheInt, RInt, PhiInt, ThetaTop, RTop, PhiTop, NTopPts, PRTFil )
     ELSE
        CALL RefCo( TheInt, RInt, PhiInt, ThetaBot, RBot, PhiBot, NBotPts, PRTFil )
     ENDIF

     ! Convert R( Theta ) to (f,g) in Robin BC
     RCmplx = RInt * EXP( CI * PhiInt )
     F      = 1.0
     G      = ( 1.0 + RCmplx ) / ( CI * KZ * ( 1.0 - RCmplx ) )
  ENDIF

  IF ( BotTop(1:3) == 'TOP' ) G = -G   ! top BC has the sign flipped relative to a bottom BC

  IF ( BCType(1:1) == 'P' ) THEN       ! Precalculated reflection coef
     CALL IRCInt( X, F, G, IPow, XTab, FTab, GTab, ITab, NkTab )
  ENDIF

  ! *** Shoot through elastic layers ***

  IF ( BotTop(1:3) == 'TOP' ) THEN
     IF ( NFirstAcoustic > 1 ) THEN
        DO Medium = 1, NFirstAcoustic - 1  	 	! Shooting down from top
           CALL ElasDn( B1, B2, B3, B4, rho, NPts, X, YV, IPow, Medium )
        END DO
        F = omega2 * YV( 4 )
        G = YV( 2 )
     ENDIF
  ELSE
     IF ( NLastAcoustic < NMedia ) THEN
        DO Medium = NMedia, NLastAcoustic + 1, -1  	! Shooting up from bottom
           CALL ElasUp( B1, B2, B3, B4, rho, NPts, X, YV, IPow, Medium )
        END DO
        F = omega2 * YV( 4 )
        G = YV( 2 )
     ENDIF
  ENDIF

  RETURN
END SUBROUTINE BCImp
!**********************************************************************!
SUBROUTINE ElasUp( B1, B2, B3, B4, rho, NPts, X, YV, IPow, Medium )

  ! Propagates through an elastic layer using compound matrix formulation

  USE scomod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL    (KIND=8) :: rho( NPts )
  COMPLEX (KIND=8) :: X, XV(5), YV(5), ZV(5), TwoX, XB3, FourHX
  COMPLEX (KIND=8) :: B1( NPts ), B2( NPts ), B3( NPts ), B4( NPts )
  PARAMETER ( Roof = 1.0E5, Floor = 1.0E-5, IPowR = 5, IPowF = -5 )

  ! Euler's method for first step

  TwoX   = 2.0 * X
  TwoH   = 2.0 * H( Medium )
  FourHX = 4.0 * H( Medium ) * X
  j      = Loc( Medium ) + N( Medium ) + 1

  XB3 = X*B3(j) - rho(j)

  ZV(1) = YV(1) - 0.5*(   B1( j ) * YV( 4 ) - B2( j ) * YV( 5 ) )
  ZV(2) = YV(2) - 0.5*( -rho( j ) * YV( 4 ) -     XB3 * YV( 5 ) )
  ZV(3) = YV(3) - 0.5*(      TwoH * YV( 4 ) + B4( j ) * YV( 5 ) )
  ZV(4) = YV(4) - 0.5*(   XB3 * YV(1) + B2(j) * YV(2) -TwoX * B4(j) * YV(3))
  ZV(5) = YV(5) - 0.5*(rho(j) * YV(1) - B1(j) * YV(2) -      FourHX * YV(3))

  ! Modified midpoint method

  DO I = N( Medium ), 1, -1
     j = j-1

     XV = YV;   YV = ZV

     XB3 = X * B3( j ) - rho( j )

     ZV(1) = XV(1) - (   B1( j ) * YV( 4 ) - B2( j) * YV( 5 ) )
     ZV(2) = XV(2) - ( -rho( j ) * YV( 4 ) -    XB3 * YV( 5 ) )
     ZV(3) = XV(3) - ( TwoH * YV( 4 ) + B4( j ) * YV( 5 ) )
     ZV(4) = XV(4) - (   XB3 * YV(1) + B2(j) * YV(2) - TwoX * B4(j) * YV(3))
     ZV(5) = XV(5) - (rho(j) * YV(1) - B1(j) * YV(2) -       FourHX * YV(3))

     ! Scale if necessary

     IF ( I /= 1 ) THEN
        IF      ( ABS( DBLE( ZV( 2 ) ) ) < Floor ) THEN
           ZV = Roof * ZV;    YV = Roof * YV
           IPow = IPow - IPowR
        ELSE IF ( ABS( DBLE( ZV( 2 ) ) ) > Roof ) THEN
           ZV = Floor * ZV;   YV = Floor * YV
           IPow = IPow - IPowF
        ENDIF

     ENDIF
  END DO

  ! Apply the standard filter at the terminal point

  YV = ( XV + 2.0 * YV + ZV ) / 4.0

  RETURN
END SUBROUTINE ElasUp
!**********************************************************************!
SUBROUTINE ElasDn( B1, B2, B3, B4, rho, NPts, X, YV, IPow, Medium )

  ! Propagates through an elastic layer using compound matrix formulation

  USE scomod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL    (KIND=8) :: rho( NPts )
  COMPLEX (KIND=8) :: X, XV(5), YV(5), ZV(5), TwoX, XB3, FourHX
  COMPLEX (KIND=8) :: B1( NPts ), B2( NPts ), B3( NPts ), B4( NPts )
  PARAMETER ( Roof = 1.0E5, Floor = 1.0E-5, IPowR = 5, IPowF = -5 )

  ! Euler's method for first step

  TwoX   = 2.0 * X
  TwoH   = 2.0 * H( Medium )
  FourHX = 4.0 * H( Medium ) * X
  j = Loc( Medium ) + 1

  XB3 = X * B3( j ) - rho( j )

  ZV(1) = YV(1) + 0.5*(   B1( j ) * YV( 4 ) - B2( j ) * YV( 5 ) )
  ZV(2) = YV(2) + 0.5*( -rho( j ) * YV( 4 ) -     XB3 * YV( 5 ) )
  ZV(3) = YV(3) + 0.5*(      TwoH * YV( 4 ) + B4( j ) * YV( 5 ) )
  ZV(4) = YV(4) + 0.5*(    XB3 * YV(1) + B2(j) * YV(2) -TwoX * B4(j) * YV(3) )
  ZV(5) = YV(5) + 0.5*( rho(j) * YV(1) - B1(j) * YV(2) -      FourHX * YV(3) )

  ! Modified midpoint method

  DO I = 1, N( Medium )
     j = j+1

     XV = YV   ;   YV = ZV

     XB3 = X * B3( j ) - rho( j )

     ZV(1) = XV(1) + (   B1( j ) * YV( 4 ) - B2( j ) * YV( 5 ) )
     ZV(2) = XV(2) + ( -rho( j ) * YV( 4 ) -     XB3 * YV( 5 ) )
     ZV(3) = XV(3) + (      TwoH * YV( 4 ) + B4( j ) * YV( 5 ) )
     ZV(4) = XV(4) + (    XB3 * YV(1) + B2(j) * YV(2) -TwoX * B4(j) * YV(3) )
     ZV(5) = XV(5) + ( rho(j) * YV(1) - B1(j) * YV(2) -      FourHX * YV(3) )

     ! Scale if necessary

     IF ( I /= N( Medium ) ) THEN

        IF     ( ABS( DBLE( ZV( 2 ) ) ) < Floor ) THEN
           ZV = Roof  * ZV   ;   YV = Roof * YV
           IPow = IPow - IPowR
        ELSE IF ( ABS( DBLE( ZV( 2 ) ) ) > Roof ) THEN
           ZV = Floor * ZV   ;   YV = Floor * YV
           IPow = IPow - IPowF
        ENDIF

     ENDIF
  END DO

  ! Apply the standard filter at the terminal point

  YV = ( XV + 2.0 * YV + ZV ) / 4.0

  RETURN
END SUBROUTINE ElasDn
!**********************************************************************!
SUBROUTINE Kernel( B1, B2, B3, B4, rho, NPts, RK, Green, NTot1 )

  ! Solve system for a sequence of k-values

  USE SdRdRMod
  USE scomod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  REAL                Z( NTot1 ), rhoElement( NPts ), RK( * )
  REAL    (KIND=8) :: rho( NPts )
  COMPLEX             Green( Nsd, Nrd, Nk )
  COMPLEX (KIND=8) :: BElement, DF( NTot1 ), EF( NTot1 )
  COMPLEX (KIND=8) :: B1( NPts ), B2( NPts ), B3( NPts ), B4( NPts ), X

  ! Tabulate z coordinates

  Z( 1 ) = Depth( NFirstAcoustic )
  j = 2

  DO Medium = NFirstAcoustic, NLastAcoustic
     Z( j : j + N(Medium) - 1 ) = Depth( Medium ) + (/ ( I * H( Medium ), I = 1, N( Medium ) ) /)
     j = j + N( Medium )
  END DO

  ! Compute weights for source/rcvr depth interpolation

  CALL WEIGHT( Z, NTot1, sd, Nsd, WS, Isd )
  CALL WEIGHT( Z, NTot1, rd, Nrd, WR, Ird )

  !     *** Assemble matrix ***

  j       = 1
  l       = Loc( NFirstAcoustic ) + 1
  DF( 1 ) = 0.0

  DO Medium = NFirstAcoustic, NLastAcoustic
     DO I = 1, N( Medium )
        rhoElement( l ) = ( rho( l ) + rho( l + 1 ) ) / 2.0
        rhoH = rhoElement( l ) * H( Medium )
        BElement = H( Medium ) * ( ( B1( l ) + B1( l + 1 ) ) / 2.0 ) / ( 12.0 * rhoElement( l ) )

        DF( j     ) = DF( j ) - 1.0 / rhoH + 5.0 * BElement
        DF( j + 1 ) =         - 1.0 / rhoH + 5.0 * BElement
        EF( j + 1 ) =           1.0 / rhoH +       BElement

        j = j + 1
        l = l + 1
     END DO
     l = l + 1
  END DO


  DO Ik = 1, Nk   ! Step through each point in k-space
     ! WRITE( 6, * ) 'IK, Nk', IK, Nk
     X = ( RK( Ik ) + CI * Atten ) ** 2
     CALL Solve( B1, B2, B3, B4, rho, NPts, NTot1, X, Green, Ik, DF, EF, rhoElement )  ! Solve for G(k)
  END DO

  ! Write Green's function to file
  DO IS = 1, Nsd
     DO IR = 1, Nrd
        WRITE( GRNFil, REC = 6 + ( IS - 1 ) * Nrd + IR ) Green( IS, IR, : )
     END DO
  END DO

  CLOSE( GRNFil )

  RETURN
END SUBROUTINE Kernel

!**********************************************************************!

SUBROUTINE Solve( B1, B2, B3, B4, rho, NPts, NTot1, X, Green, Ik, DF, EF, rhoElement )

  ! Set up the linear system and solve

  USE SdRdRMod
  USE scomod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL rhoElement( * )
  REAL (KIND=8)    :: rho( NPts )
  COMPLEX Green( Nsd, Nrd, Nk )
  COMPLEX (KIND=8) :: D( NTot1 ), E( NTot1 ), RV1( NTot1 ), RV2( NTot1 ), RV3( NTot1 ), RV4( NTot1 )
  COMPLEX (KIND=8) :: DF( * ), EF( * ), BElement, XT, X, F, G
  COMPLEX (KIND=8) :: B1( NPts ), B2( NPts ), B3( NPts ), B4( NPts )
  CHARACTER BCType*1

  ! *** Complete assembly of matrix by adding in X ***

  j = 1
  l = Loc( NFirstAcoustic ) + 1
  D( 1 ) = DF( 1 )

  DO Medium = NFirstAcoustic, NLastAcoustic
     XT = -H( Medium ) * X / 12.0

     DO I = 1, N( Medium )
        BElement = XT / rhoElement( l )
        D( j    ) = D(  j     ) + 5.0 * BElement
        D( j + 1) = DF( j + 1 ) + 5.0 * BElement
        E( j + 1) = EF( j + 1 ) +       BElement
        j = j + 1
        l = l + 1
     END DO

     l = l + 1
  END DO

  ! *** Corner elt requires top impedance ***

  BCType(1:1) = TopOpt(2:2)
  CALL BCIMP( B1, B2, B3, B4, rho, NPts, X, BCType, 'TOP', cpT, csT, rhoT, F, G, IPow )
  IF ( G == 0.0 ) THEN
     D( 1 ) = 1.0   ;   E( 2 ) = 0.0
  ELSE
     D( 1 ) = D( 1 ) + F / G
  ENDIF

  ! *** Corner elt requires bottom impedance ***

  BCType(1:1) = BotOpt(1:1)
  CALL BCIMP( B1, B2, B3, B4, rho, NPts, X, BCType, 'BOT', cpB, csB, rhoB, F, G, IPow )
  IF ( G == 0.0 ) THEN
     D( NTot1 ) = 1.0
     E( NTot1 ) = 0.0
  ELSE
     D( NTot1 ) =  D( NTot1 ) - F / G
  ENDIF

  CALL FACTOR( NTot1, D, E, RV1, RV2, RV3, RV4 )   !     * Do LU decomposition *

  DO IS = 1, Nsd   ! Loop over all source positions

     ! Set up RHS in D (previously used for diagonal)
     rhosd      = 1.0    ! assumes rho( zs ) = 1
     D          = 0.0
     I          = Isd( IS )
     D( I     ) = 2.0 * ( 1.0 - WS( IS ) ) / rhosd
     D( I + 1 ) = 2.0 *     WS( IS )       / rhosd

     CALL BackSb( NTot1, RV1, RV2, RV3, RV4, D )    ! Solve the system
     Green( IS, :, Ik ) = D( Ird ) + WR * ( D( Ird + 1 ) - D( Ird ) )   ! extract the solution at the rcvr depths
  END DO

  RETURN
END SUBROUTINE Solve
