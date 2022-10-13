SUBROUTINE READIN( Title, Freq, MaxMedia, NMedia, &
     TopOpt, CPT, CST, RhoT, BumDen, eta, xi, NG, sigma, Depth, &
     BotOpt, CPB, CSB, RhoB, ENVFIL, PRTFIL )

  !     Reads in the info in ENVFIL

  IMPLICIT NONE

  INTEGER MaxSSP
  PARAMETER ( MaxSSP = 2001 )

  INTEGER ENVFIL, PRTFIL, NMedia, NG( * ), MaxMedia, NElts, Med
  REAL (KIND=8)    :: AlphaR, BetaR, AlphaI, BetaI, RhoR, Freq, RhoT, RhoB, &
       sigma( * ), Depth( * ), BumDen, eta, xi, Rho( 1 ), C, deltaz
  COMPLEX (KIND=8) :: CPT, CST, CPB, CSB, CP( MaxSSP ), CS( MaxSSP )
  CHARACTER TopOpt*( * ), BotOpt*( * ), Title*( * ), SSPType*1, AttenUnit*2, BCType*1, Task*8

  COMMON /CPREV/ AlphaR, BetaR, RhoR, AlphaI, BetaI

  ! OPEN ( FILE = 'ENVFIL', UNIT = ENVFIL, STATUS = 'OLD', FORM = 'FORMATTED' )

  AlphaR = 1500.0
  BetaR  = 0.0
  RhoR   = 1.0
  AlphaI = 0.0
  BetaI  = 0.0
  NElts  = 0         ! this is a dummy variable, passed to profil during read of SSP

  READ(  ENVFIL, *, END = 9999 ) Title(9:80)
  WRITE( PRTFIL, * ) Title

  READ( ENVFIL, * ) Freq
  READ( ENVFIL, * ) NMedia

  WRITE( PRTFIL, "( ' Frequency = ', G11.4, ' NMedia = ', I3, // )" ) Freq, NMedia

  IF ( NMedia > MaxMedia ) THEN
     WRITE( PRTFIL, * ) 'MaxMedia = ', MaxMedia
     CALL ERROUT( PRTFIL, 'F', 'READIN', 'Too many Media' )
  ENDIF

  ! ******** TOP OPTIONS ********

  READ( ENVFIL, * ) TopOpt(1:8)

  SSPType    = TopOpt(1:1)
  BCType     = TopOpt(2:2)
  AttenUnit  = TopOpt(3:4)

  ! *** SSP approximation options ***

  SELECT CASE ( SSPType )
  CASE ( 'N' )
     WRITE( PRTFIL, * ) '    N2-LINEAR approximation to SSP'
  CASE ( 'C' )
     WRITE( PRTFIL, * ) '    C-LINEAR approximation to SSP'
  CASE ( 'S' )
     WRITE( PRTFIL, * ) '    SPLINE approximation to SSP'
  CASE ( 'A' )
     WRITE( PRTFIL, * ) '    ANALYTIC SSP option'
  CASE DEFAULT
     CALL ERROUT( PRTFIL, 'F', 'READIN', 'Unknown option for SSP approximation' )
  END SELECT

  ! *** Attenuation options ***

  SELECT CASE ( AttenUnit(1:1) )
  CASE ( 'N' )
     WRITE( PRTFIL, * ) '    Attenuation units: nepers/m'
  CASE ( 'F' )
     WRITE( PRTFIL, * ) '    Attenuation units: dB/mkHz'
  CASE ( 'M' ) 
     WRITE( PRTFIL, * ) '    Attenuation units: dB/m'
  CASE ( 'W' )
     WRITE( PRTFIL, * ) '    Attenuation units: dB/wavelength'
  CASE ( 'Q' )
     WRITE( PRTFIL, * ) '    Attenuation units: Q'
  CASE ( 'L' )
     WRITE( PRTFIL, * ) '    Attenuation units: Loss parameter'
  CASE DEFAULT
     CALL ERROUT( PRTFIL, 'F', 'READIN', 'Unknown attenuation units' )
  END SELECT

  ! *** Added volume attenuation ***

  SELECT CASE ( AttenUnit(2:2) )
  CASE ( 'T' )
     WRITE( PRTFIL, * ) '    THORP attenuation added'
  END SELECT

  ! *** CALL TOPBOT  to read top BC ***

  IF ( BCType == 'A' ) &
       WRITE( PRTFIL, "( //, '      Z          AlphaR     BetaR      Rho       AlphaI     BetaI', / )" )

  CALL TOPBOT( ENVFIL, PRTFIL, Freq, BCType, AttenUnit, CPT, CST, RhoT, BumDen, eta, xi )

  ! ****** Internal media ******

  IF ( BCType /= 'A' ) &
       WRITE( PRTFIL, "( //, '      Z          AlphaR     BetaR      Rho       AlphaI     BetaI', / )" )

  DO Med = 1, NMedia
     READ(  ENVFIL, *    ) NG( Med ), sigma( Med ), Depth( Med + 1 )
     WRITE( PRTFIL, "( /, '          ( Number of pts = ', I5, '  RMS roughness = ', G10.3, ' )')" ) &
          NG( Med ),            sigma( Med )

     ! *** Call PROFIL to read in SSP ***

     Task = 'INIT'
     CALL PROFIL( Depth, CP, CS, Rho, Med, NElts, Freq, SSPType, AttenUnit, Task, ENVFIL, PRTFIL  )

     ! *** automatic calculation of f.d. mesh (10 pts/wavelength) ***
     IF ( NG( Med ) == 0 ) THEN
        C = Alphar
        IF ( Betar > 0.0 ) C = Betar ! shear?

        deltaz = 0.1 * C / Freq     ! tenth of a wavelength default sampling
        NG( Med ) = ( Depth( Med + 1 ) - Depth( Med ) ) / deltaz
        NG( Med ) = MAX( NG( Med), 10 )     ! require a minimum of 10 points
        WRITE( PRTFIL, * ) 'Number of pts = ', NG( Med )
     ENDIF

  END DO   ! next Med

  ! ****** Bottom properties ******

  READ( ENVFIL, * ) BotOpt(1:8), sigma( NMedia + 1 )
  BCType = BotOpt(1:1)

  WRITE( PRTFIL, * )
  WRITE( PRTFIL, "( 33X, '( RMS roughness = ', G10.3, ' )' )" ) sigma( NMedia + 1 )

  ! *** CALL TOPBOT  to read bottom BC ***

  CALL TOPBOT( ENVFIL, PRTFIL, Freq, BCType, AttenUnit, CPB, CSB, RhoB, BumDen, eta, xi )

  RETURN

9999 STOP

END SUBROUTINE READIN
!**********************************************************************!
SUBROUTINE TOPBOT( ENVFIL, PRTFIL, Freq, BCType, AttenUnit, CPHS, CSHS, RhoHS, BumDen, eta, xi )

  ! Handles top and bottom boundary conditions

  ! Input:
  !     ENVFIL: Environmental file
  !     PRTFIL: Print file
  !     Freq:   Frequency
  !     BCType: Boundary condition type
  !
  ! Output:
  !    CPHS:    P-wave speed in halfspace
  !    CSHS:    S-wave speed in halfspace
  !    RhoHS:   density in halfspace

  !    BumDen:  Bump density
  !    eta:     Principal radius 1
  !    xi:      Principal radius 2

  IMPLICIT NONE

  INTEGER             ENVFIL, PRTFIL
  REAL    (KIND=8) :: AlphaR, BetaR, AlphaI, BetaI, RhoR, Freq, RhoHS, BumDen, eta, xi, ZTMP
  COMPLEX (KIND=8) :: CPHS, CSHS, CRCI
  CHARACTER           BCType*1, AttenUnit*2

  COMMON /CPREV/ AlphaR, BetaR, RhoR, AlphaI, BetaI

  ! *** Echo to PRTFIL user's choice of boundary condition ***

  SELECT CASE ( BCType )
  CASE ( 'S' )
     WRITE( PRTFIL, * ) '    Twersky SOFT BOSS scatter model'
  CASE ( 'H' )
     WRITE( PRTFIL, * ) '    Twersky HARD BOSS scatter model'
  CASE ( 'T' )
     WRITE( PRTFIL, * ) '    Twersky (amplitude only) SOFT BOSS scatter model'
  CASE ( 'I' )
     WRITE( PRTFIL, * ) '    Twersky (amplitude only) HARD BOSS scatter model'
  CASE ( 'V' )
     WRITE( PRTFIL, * ) '    VACUUM'
  CASE ( 'R' )
     WRITE( PRTFIL, * ) '    Perfectly RIGID'
  CASE ( 'A' )
     WRITE( PRTFIL, * ) '    ACOUSTO-ELASTIC half-space'
  CASE ( 'F' )
     WRITE( PRTFIL, * ) '    FILE used for reflection loss'
  CASE ( 'W' )
     WRITE( PRTFIL, * ) '    Writing an IRC file'
  CASE ( 'P' )
     WRITE( PRTFIL, * ) '    reading PRECALCULATED IRC'
  CASE DEFAULT
     CALL ERROUT( PRTFIL, 'F', 'TOPBOT', 'Unknown boundary condition type' )
  END SELECT

  ! ****** Read in BC parameters depending on particular choice ******

  CPHS  = 0.0
  CSHS  = 0.0
  RhoHS = 0.0

  ! *** Twersky ice model parameters ***

  IF ( BCType == 'S' .OR. BCType == 'H' .OR. &
       BCType == 'T' .OR. BCType == 'I' ) THEN

     READ(  ENVFIL, *    ) BumDen, eta, xi
     WRITE( PRTFIL, 1000 ) BumDen, eta, xi
1000 FORMAT( /, ' Twersky ice model parameters:', /, &
          ' Bumden = ', G15.6, '  Eta = ', G11.3, '  Xi = ', G11.3, /)
  ENDIF

  ! *** Half-space properties ***

  IF ( BCType == 'A' ) THEN

     READ(  ENVFIL, *    ) ZTMP, AlphaR, BetaR, RhoR, AlphaI, BetaI
     WRITE( PRTFIL, 2000 ) ZTMP, AlphaR, BetaR, RhoR, AlphaI, BetaI
2000 FORMAT( F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4 )

     CPHS = CRCI( AlphaR, AlphaI, Freq, AttenUnit )
     CSHS = CRCI( BetaR,  BetaI,  Freq, AttenUnit )
     RhoHS = RhoR
     IF ( CPHS == 0.0 .OR. RhoHS == 0.0 ) &
          CALL ERROUT( PRTFil, 'F', 'TOPBOT', 'Sound speed or density vanishes in halfspace' )

  ENDIF

  RETURN
END SUBROUTINE TOPBOT
!**********************************************************************!
SUBROUTINE PROFIL( Depth, CP, CS, RhoT, Med, N1, Freq, SSPType, AttenUnit, Task, ENVFIL, PRTFIL )

  !     Call the particular SSP routine specified by SSPType

  !     PROFIL is expected to perform two Tasks:
  !        Task = 'TAB'  then tabulate CP, CS, RhoT
  !        Task = 'INIT' then initialize

  !     Note that Freq is only need if Task = 'INIT'

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  INTEGER ENVFIL, PRTFIL

  REAL (KIND=8) ::    RhoT( * ), Depth( * )
  COMPLEX (KIND=8) :: CPT, CST
  COMPLEX (KIND=8) :: CP( * ), CS( * )
  CHARACTER  SSPType*1, AttenUnit*2, Task*8

  SELECT CASE ( SSPType )
  CASE ( 'A' )  ! *** Analytic profile option ***
     IF ( Task(1:4) == 'INIT' ) THEN
        N1 = 21
        CALL ANALYT( Depth, CP, CS, RhoT, Med, N1, Freq, AttenUnit, Task )
        H = ( Depth( Med+1 ) - Depth( Med ) ) / ( N1 - 1 )

        DO I = 1, N1
           Z = Depth( Med ) + ( I - 1 ) * H
           CPT =  CP( I )   ;    CST =  CS( I )
           WRITE( PRTFIL, 2000 ) Z,  REAL( CPT ),  REAL( CST ), RhoT(I), AIMAG( CPT ), AIMAG( CST )
2000       FORMAT( F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4 )
        END DO
     ELSE
        CALL ANALYT( Depth, CP, CS, RhoT, Med, N1, Freq, AttenUnit, Task )
     ENDIF

  CASE ( 'N' )  ! *** N2-linear profile option ***
     CALL N2LIN(  Depth, CP, CS, RhoT, Med, N1, Freq, AttenUnit, Task, ENVFIL, PRTFIL )

  CASE ( 'C' )  ! *** C-linear profile option ***
     CALL CLIN(   Depth, CP, CS, RhoT, Med, N1, Freq, AttenUnit, Task, ENVFIL, PRTFIL )

  CASE ( 'S' )  ! *** Cubic spline profile option ***
     CALL CCUBIC( Depth, CP, CS, RhoT, Med, N1, Freq, AttenUnit, Task, ENVFIL, PRTFIL )

  CASE DEFAULT ! *** Non-existent profile option ***
     WRITE( PRTFIL, * ) 'Profile option: ', SSPType(1:1)
     CALL ERROUT( PRTFIL, 'F', 'PROFIL', 'Unknown profile option' )

  END SELECT

  RETURN
END SUBROUTINE PROFIL
!**********************************************************************!
FUNCTION CRCI( C, Alpha, Freq, AttenUnit )

  !     Converts real wave speed and attenuation to a single
  !     complex wave speed (with positive imaginary part)

  !     6 CASES:    N for Nepers/meter
  !                 M for dB/meter      (M for Meters)
  !                 F for dB/m-kHZ      (F for Frequency dependent)
  !                 W for dB/wavelength (W for Wavelength)
  !                 Q for Q
  !                 L for Loss parameter
  !
  !     second letter adds volume attenuation according to standard laws:
  !                 T for Thorp

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  PARAMETER ( PI = 3.1415926535897932D0 )
  COMPLEX (KIND=8) :: CRCI
  CHARACTER AttenUnit*2

  Omega = 2.0 * PI * Freq

  ! *** Convert to Nepers/m ***

  AlphaT = 0.0

  SELECT CASE ( AttenUnit(1:1) )
  CASE ( 'N' )
     AlphaT = Alpha
  CASE ( 'M' )
     AlphaT = Alpha / 8.6858896D0
  CASE ( 'F' )
     AlphaT = Alpha * Freq / 8685.8896D0
  CASE ( 'W' )
     IF ( C /= 0.0 ) AlphaT = Alpha * Freq / ( 8.6858896D0 * C )

     !        The following lines give f^1.25 Frequency dependence
     !        FAC = SQRT( SQRT( Freq / 50.0 ) )
     !        IF ( C /= 0.0 ) AlphaT = FAC * Alpha * Freq / ( 8.6858896D0 * C )
  CASE ( 'Q' )
     IF( C * Alpha /= 0.0 ) AlphaT = Omega / ( 2.0 * C * Alpha )
  CASE ( 'L' )   ! loss parameter
     IF ( C /= 0.0 ) AlphaT = Alpha * Omega / C
  END SELECT

  ! added volume attenuation

  SELECT CASE ( AttenUnit(2:2) )
  CASE ( 'T' )
     F2 = ( Freq / 1000.0 ) **2
     Thorpe = 40.0 * F2 / ( 4100.0 + F2 ) + 0.1 * F2 / ( 1.0 + F2 )
     Thorpe = Thorpe / 914.4D0                 ! dB / m
     Thorpe = Thorpe / 8.6858896D0             ! Nepers / m
     AlphaT = AlphaT + Thorpe
  END SELECT

  ! Convert Nepers/m to equivalent imaginary sound speed ***

  AlphaT = AlphaT * C * C / Omega
  CRCI = CMPLX( C, AlphaT, KIND=8 )

  RETURN
END FUNCTION CRCI
!**********************************************************************!
SUBROUTINE N2LIN( Depth, CP, CS, RhoT, Med, N1, Freq, AttenUnit, Task, ENVFIL, PRTFIL )

  ! Tabulate CP, CS, Rho for specified Med
  ! Uses N2-linear segments for P and S-wave speeds
  ! Uses Rho-linear segments for density

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  INTEGER ENVFIL, PRTFIL
  PARAMETER ( MaxMedia = 501, MaxSSP = 2001, PREC = 1.0E-6 )

  INTEGER Loc( MaxMedia ), NSSPPts( MaxMedia )
  REAL (KIND=8) :: Depth( * ), RhoT( * ), Z( MaxSSP ), Rho( MaxSSP )
  COMPLEX (KIND=8) :: CP( * ), CS( * ), Alpha( MaxSSP ), Beta( MaxSSP ), N2BOT, N2TOP, CRCI
  CHARACTER AttenUnit*2, Task*8

  SAVE Z, Alpha, Beta, Rho, Loc, NSSPPts
  COMMON /CPREV/ AlphaR, BetaR, RhoR, AlphaI, BetaI

  ! If Task = 'INIT' then this is the first call and SSP is read.
  ! Any other call is a request for SSP subtabulation.

  IF ( Task(1:4) == 'INIT' ) THEN   ! Task 'INIT' for initialization

     ! The variable Loc( Med ) points to the starting point for the
     ! data in the arrays Z, Alpha, Beta and Rho

     IF ( Med == 1 ) THEN
        Loc( Med ) = 0
     ELSE
        Loc( Med ) = Loc( Med - 1 ) + NSSPPts( Med - 1 )
     ENDIF
     ILoc = Loc( Med )

     ! *** Read in data and convert attenuation to Nepers/m ***

     N1 = 1
     DO I = 1, MaxSSP
        READ(  ENVFIL, *    ) Z( ILoc + I ), AlphaR, BetaR, RhoR, AlphaI, BetaI
        WRITE( PRTFIL, 2000 ) Z( ILoc + I ), AlphaR, BetaR, RhoR, AlphaI, BetaI
2000    FORMAT( F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4 )

        Alpha(ILoc + I) = CRCI( AlphaR, AlphaI, Freq, AttenUnit )
        Beta( ILoc + I) = CRCI( BetaR,  BetaI,  Freq, AttenUnit )
        Rho(  ILoc + I) = RhoR

        ! Did we read the last point?
        IF ( ABS( Z( ILoc + I ) - Depth( Med+1 ) ) < PREC ) THEN
           NSSPPts( Med ) = N1
           IF ( Med == 1 ) Depth( 1 ) = Z( 1 )
           RETURN
        ENDIF

        N1 = N1 + 1
     END DO

     ! Fall through means too many points in the profile

     WRITE( PRTFIL, * ) 'Max. #SSP points: ', MaxSSP
     CALL ERROUT( PRTFIL, 'F', 'N2LIN', 'Number of SSP points exceeds limit' )

  ELSE   ! Task = 'TABULATE'
     ILoc = Loc( Med )
     N    = N1 - 1
     H    = ( Z( ILoc + NSSPPts( Med ) ) - Z( ILoc + 1 ) ) / N
     Lay  = 1

     DO I = 1, N1
        ZT = Z( ILoc + 1 ) + ( I - 1 ) * H
        IF ( I == N1 ) ZT = Z( ILoc + NSSPPts( Med ) )   ! Make sure no overshoot

        DO WHILE ( ZT > Z( ILoc + Lay + 1 ) )
           Lay = Lay + 1
        END DO

        R = ( ZT - Z( ILoc + Lay ) ) / ( Z( ILoc + Lay+1 ) - Z( ILoc + Lay ) )

        ! P-wave
        N2TOP   = 1.0 / Alpha( ILoc + Lay     )**2
        N2BOT   = 1.0 / Alpha( ILoc + Lay + 1 )**2
        CP( I ) = 1.0 / SQRT( ( 1.0 - R ) * N2TOP + R * N2BOT )

        ! S-wave
        IF ( Beta(ILoc + Lay) /= 0.0 ) THEN
           N2TOP   = 1.0 / Beta( ILoc + Lay     )**2
           N2BOT   = 1.0 / Beta( ILoc + Lay + 1 )**2
           CS( I ) = 1.0 / SQRT( ( 1.0 - R ) * N2TOP + R * N2BOT )
        ELSE
           CS( I ) = 0.0
        ENDIF

        RhoT( I ) = ( 1.0 - R ) * Rho( ILoc + Lay ) + R * Rho( ILoc + Lay + 1 )
     END DO

  ENDIF

  RETURN
END SUBROUTINE N2LIN
!**********************************************************************!
SUBROUTINE CLIN( Depth, CP, CS, RhoT, Med, N1, Freq, AttenUnit, Task, ENVFIL, PRTFIL  )

  ! Tabulate CP, CS, Rho for specified Med

  ! Uses c-linear segments for P and S-wave speeds
  ! Uses Rho-linear segments for density

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  INTEGER ENVFIL, PRTFIL
  PARAMETER ( MaxMedia = 501, MaxSSP = 2001, PREC = 1.0E-6 )

  INTEGER Loc( MaxMedia ), NSSPPts( MaxMedia )
  REAL (KIND=8) :: Depth( * ), RhoT( * ), Z( MaxSSP ), Rho( MaxSSP )
  COMPLEX (KIND=8) :: CP( * ), CS( * ), Alpha( MaxSSP ), Beta( MaxSSP ), CRCI
  CHARACTER AttenUnit*2, Task*8

  SAVE Z, Alpha, Beta, Rho, Loc, NSSPPts
  COMMON /CPREV/ AlphaR, BetaR, RhoR, AlphaI, BetaI

  ! If Task = 'INIT' then this is the first call and SSP is read.
  ! Any other call is a request for SSP subtabulation.

  IF ( Task(1:4) == 'INIT' ) THEN   ! Task 'INIT' FOR INITIALIZATION
     NSSPPts( Med ) = N1

     ! The variable Loc(Med) points to the starting point for the
     ! data in the arrays Z, Alpha, Beta and Rho

     IF ( Med == 1 ) THEN
        Loc( Med ) = 0
     ELSE
        Loc( Med ) = Loc( Med - 1 ) + NSSPPts( Med - 1 )
     ENDIF
     ILoc = Loc( Med )

     ! *** Read in data and convert attenuation to Nepers/m ***

     N1 = 1
     DO I = 1, MaxSSP
        READ(  ENVFIL, *    ) Z( ILoc + I ), AlphaR, BetaR, RhoR, AlphaI, BetaI
        WRITE( PRTFIL, 2000 ) Z( ILoc + I ), AlphaR, BetaR, RhoR, AlphaI, BetaI
2000    FORMAT( F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4 )

        Alpha( ILoc + I ) = CRCI( AlphaR, AlphaI, Freq, AttenUnit )
        Beta(  ILoc + I ) = CRCI( BetaR,  BetaI,  Freq, AttenUnit )
        Rho(   ILoc + I ) = RhoR

        ! Did we read the last point?
        IF ( ABS( Z( ILoc + I ) - Depth( Med+1 ) ) < PREC ) THEN
           NSSPPts( Med ) = N1
           IF ( Med == 1 ) Depth( 1 ) = Z( 1 )
           RETURN
        ENDIF

        N1 = N1 + 1
     END DO

     ! Fall through means too many points in the profile

     WRITE( PRTFIL, * ) 'Max. #SSP points: ', MaxSSP
     CALL ERROUT( PRTFIL, 'F', 'CLIN', 'Number of SSP points exceeds limit' )

  ELSE   ! Task = 'TABULATE'
     ILoc = Loc( Med )
     N    = N1 - 1
     H    = ( Z( ILoc + NSSPPts( Med ) ) - Z( ILoc + 1 ) ) / N
     Lay  = 1

     DO I = 1, N1
        ZT = Z( ILoc + 1 ) + ( I - 1 ) * H
        IF ( I == N1 ) ZT = Z( ILoc + NSSPPts( Med ) )   ! Make sure no overshoot

        DO WHILE ( ZT > Z( ILoc + Lay + 1 ) )
           Lay = Lay + 1
        END DO

        R = ( ZT - Z( ILoc + Lay ) ) / ( Z( ILoc + Lay + 1 ) - Z( ILoc + Lay ) )
        CP(   I ) = ( 1.0 - R ) * Alpha( ILoc + Lay ) + R * Alpha( ILoc + Lay+1 )
        CS(   I ) = ( 1.0 - R ) *  Beta( ILoc + Lay ) + R *  Beta( ILoc + Lay+1 )
        RhoT( I ) = ( 1.0 - R ) *   Rho( ILoc + Lay ) + R *   Rho( ILoc + Lay+1 )
     END DO
  ENDIF

  RETURN
END SUBROUTINE CLIN
!**********************************************************************!
SUBROUTINE CCUBIC( Depth, CP, CS, RhoT, Med, N1, Freq, AttenUnit, Task, ENVFIL, PRTFIL  )

  ! Tabulate CP, CS, Rho for specified Med
  ! using cubic spline interpolation

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  INTEGER ENVFIL, PRTFIL
  PARAMETER ( MaxMedia = 501, MaxSSP = 2001, PREC = 1.0E-6 )

  INTEGER Loc( MaxMedia ), NSSPPts( MaxMedia )
  REAL (KIND=8) :: Depth( * ), Z( MaxSSP ), RhoT( * )
  COMPLEX (KIND=8) :: CP( * ), CS( * ), ESPLINE, CRCI, Alpha( 4, MaxSSP ), Beta( 4, MaxSSP ), Rho( 4, MaxSSP )
  CHARACTER AttenUnit*2, Task*8

  SAVE Z, Alpha, Beta, Rho, Loc, NSSPPts
  COMMON /CPREV/ AlphaR, BetaR, RhoR, AlphaI, BetaI

  ! If Task = 'INIT' then this is the first call and SSP is read.
  ! Any other call is a request for SSP subtabulation.

  IF ( Task(1:4) == 'INIT' ) THEN   ! --- Task 'INIT' for initialization
     NSSPPts( Med ) = N1

     ! The variable Loc(Med) points to the starting point for the
     ! data in the arrays Z, Alpha, Beta and Rho

     IF ( Med == 1 ) THEN
        Loc( Med ) = 0
     ELSE
        Loc( Med ) = Loc( Med - 1 ) + NSSPPts( Med - 1 )
     ENDIF
     ILoc = Loc( Med )

     ! *** Read in data and convert attenuation to Nepers/m ***

     N1 = 1

     DO I = 1, MaxSSP
        READ(  ENVFIL, *    ) Z( ILoc + I ), AlphaR, BetaR, RhoR, AlphaI, BetaI
        WRITE( PRTFIL, 2000 ) Z( ILoc + I ), AlphaR, BetaR, RhoR, AlphaI, BetaI
2000    FORMAT(F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4)

        Alpha(1, ILoc + I) = CRCI( AlphaR, AlphaI, Freq, AttenUnit )
        Beta( 1, ILoc + I) = CRCI( BetaR,  BetaI,  Freq, AttenUnit )
        Rho(  1, ILoc + I) = RhoR

        ! Did we read the last point?
        IF ( ABS( Z( ILoc + I ) - Depth(Med+1) ) < PREC ) THEN
           NSSPPts( Med ) = N1
           IF ( Med == 1 ) Depth( 1 ) = Z( 1 )

           !  *** Compute spline coefs ***

           IBCBEG = 0
           IBCEND = 0
           CALL CSPLINE( Z( ILoc + 1 ), Alpha( 1, ILoc + 1 ), NSSPPts( Med ), IBCBEG, IBCEND, NSSPPts( Med ) )
           CALL CSPLINE( Z( ILoc + 1 ),  Beta( 1, ILoc + 1 ), NSSPPts( Med ), IBCBEG, IBCEND, NSSPPts( Med ) )
           CALL CSPLINE( Z( ILoc + 1 ),   Rho( 1, ILoc + 1 ), NSSPPts( Med ), IBCBEG, IBCEND, NSSPPts( Med ) )

           RETURN
        ENDIF

        N1 = N1 + 1
     END DO

     ! Fall through means too many points in the profile

     WRITE( PRTFIL, * ) 'Max. #SSP points: ', MaxSSP
     CALL ERROUT( PRTFIL, 'F', 'CCUBIC', 'Number of SSP points exceeds limit' )

  ELSE   ! Task = 'TABULATE'
     ILoc = Loc( Med )
     N    = N1 - 1
     H    = ( Z( ILoc + NSSPPts( Med ) ) - Z( ILoc + 1 ) ) / N
     Lay  = 1
     DO I = 1, N1
        ZT = Z( ILoc + 1 ) + ( I - 1 ) * H
        IF ( I == N1 ) ZT = Z( ILoc + NSSPPts( Med ) )   ! Make sure no overshoot
        DO WHILE ( ZT > Z( ILoc + Lay + 1 ) )
           Lay = Lay + 1
        END DO

        HSPLNE = ZT - Z( ILoc + Lay )

        CP(   I ) =       ESPLINE( Alpha( 1, ILoc + Lay ), HSPLNE )
        CS(   I ) =       ESPLINE(  Beta( 1, ILoc + Lay ), HSPLNE )
        RhoT( I ) = DBLE( ESPLINE(   Rho( 1, ILoc + Lay ), HSPLNE ) )

     END DO
  ENDIF

  RETURN
END SUBROUTINE CCUBIC
