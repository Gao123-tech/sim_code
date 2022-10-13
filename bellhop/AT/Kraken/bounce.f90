PROGRAM BOUNCE

  ! Program for computing the reflection coefficient, R(theta)
  ! Michael B. Porter

  USE krakcmod
  USE RefCoMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  ! *** Read in environmental info ***

  TITLE = 'BOUNCE-'

  CALL READIN( TITLE, Freq, MAXMed, NMedia,                         &
       TopOpt, CPT, CST, RhoT, BUMDEN, ETA, XI, NG, Sigma, Depth,     &
       BotOpt, CPB, CSB, RhoB, ENVFIL, PRTFIL )

  READ(  ENVFIL, *    ) Clow, Chigh        ! Spectral limits
  WRITE( PRTFIL, FMT = "( /, ' Clow = ', G12.5, '  Chigh = ', G12.5 )"  ) Clow, Chigh
  IF ( Clow == 0.0 ) CALL ERROUT( PRTFil, 'F', 'BOUNCE', 'Clow must be greater than zero' )

  READ(  ENVFIL, * ) RMax                  ! Maximum range for calculations
  WRITE( PRTFIL, * ) 'RMax = ', RMax
  CLOSE( ENVFIL )

  Omega2 = ( 2.0 * PI * Freq ) ** 2

  CALL READRC( BotOpt(1:1), TopOpt(1:1), PRTFil ) ! Optionally read in bottom reflection coefficient data

  ! *** Write internal reflection coefficient data ***

  WRITE( PRTFIL, * ) 'Writing internal refl. coeff. table'
  ! Compute NkTab

  RKmin = SQRT( Omega2 ) / Chigh
  RKmax = SQRT( Omega2 ) / Clow
  IF ( Chigh > 1.0E6 ) RKmin = 0.0

  NkTab = 1000.0 * RMax * ( RKmax - RKmin ) / ( 2.0 * PI )
  WRITE( PRTFIL, * ) 'NkTab = ', NkTab

  ALLOCATE( XTab( NkTab ), FTab( NkTab ), GTab( NkTab), ITab( NkTab ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFIL, 'F', 'BOUNCE', 'Too many points in reflection coefficient' )

  CALL COMPR( RKmin, RKmax )

  STOP
END PROGRAM BOUNCE

!**********************************************************************C

SUBROUTINE INIT

  ! Initializes arrays defining difference equations

  USE krakcmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  LOGICAL :: ELFLAG = .FALSE.
  COMPLEX (KIND=8) :: CP2, CS2
  COMPLEX (KIND=8), ALLOCATABLE :: CP( : ), CS( : )
  CHARACTER TASK*8

  Cmin     = 1.0E6
  NFACT    = 0
  LOC( 1 ) = 0
  NPTS = SUM( N( 1:NMedia ) ) + NMedia

  ALLOCATE ( B1( NPTS ), B2( NPTS ), B3( NPTS ), B4( NPTS ), Rho( NPTS ), CP( NPTS ), CS( NPTS ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) &
       CALL ERROUT( PRTFIL, 'F', 'BOUNCE - INIT', 'Insufficient memory: Reduce mesh.' )

  ! *** Loop over media ***

  DO Med = 1, NMedia
     IF ( Med /= 1 ) LOC( Med ) = LOC( Med-1 ) + N( Med-1 ) + 1
     N1 = N(   Med ) + 1
     I  = LOC( Med ) + 1

     ! *** PROFIL reads in the data for a medium ***

     TASK = 'TAB'
     CALL PROFIL( Depth, CP( I ), CS( I ), Rho( I ), Med, N1, Freq, TopOpt(1:1), TopOpt(3:4), TASK, ENVFIL, PRTFIL )

     ! *** Load diagonals of the finite-difference equations ***

     IF ( CS( I ) == ( 0.0, 0.0 ) ) THEN ! Case of an acoustic medium ---

        MATER( Med ) = 'ACOUSTIC'
        IF ( NFACT == 0 ) NFACT = Med
        NLACT = Med

        Cmin = MIN( MINVAL( DBLE( CP( I : I + N( Med ) ) ) ), Cmin )
        B1( I : I + N( Med ) ) = -2.0 +  H( Med ) ** 2 * Omega2 / CP( I : I + N( Med ) ) ** 2

     ELSE                                ! Case of an elastic medium ---

        IF ( Sigma( Med ) /= 0.0 ) THEN
           WRITE( PRTFIL, * ) 'Rough elastic interface not allowed'
           WRITE( PRTFIL, * ) 'PROGRAM ABORTING'
           STOP 'ERROR IN BOUNCE: Rough elastic interface not allowed'
        ENDIF

        MATER( Med ) = 'ELASTIC'
        ELFLAG = .TRUE.
        TWOH   = 2.0 * H( Med )

        DO J = I, I + N( Med )
           Cmin = MIN( DBLE( CS( J ) ), Cmin )

           CP2 = CP( J ) ** 2
           CS2 = CS( J ) ** 2

           B1( J ) = TWOH / ( Rho( J ) * CS2 )
           B2( J ) = TWOH / ( Rho( J ) * CP2 )
           B3( J ) = 4.0 * TWOH * Rho( J ) * CS2 * ( CP2 - CS2 ) / CP2
           B4( J ) = TWOH * ( CP2 - 2.0 * CS2 ) / CP2

           Rho( J ) = TWOH * Omega2 * Rho( J )
        END DO
     ENDIF
  END DO   ! Next Med

  ! *** Bottom properties ***

  IF ( BotOpt(1:1) == 'A' ) THEN
     IF ( CSB /= ( 0.0, 0.0 ) ) THEN ! Elastic bottom:
        ELFLAG = .TRUE.
        Cmin = MIN( Cmin, DBLE( CSB ) )
     ELSE                            ! Acoustic bottom:
        Cmin = MIN( Cmin, DBLE( CPB ) )
     ENDIF
  ENDIF

  ! *** Top properties ***

  IF ( TopOpt(2:2) == 'A' ) THEN
     IF ( CST /= ( 0.0, 0.0 ) ) THEN   ! Elastic top:
        ELFLAG = .TRUE.
        Cmin = MIN( Cmin, DBLE( CST ) )
     ELSE                              ! Acoustic top:
        Cmin = MIN( Cmin, DBLE( CPT ) )
     ENDIF
  ENDIF

  IF ( ELFLAG ) Cmin = 0.9 * Cmin
  Clow = MAX( Clow, 0.99 * Cmin )

  RETURN
END SUBROUTINE INIT
!**********************************************************************C
SUBROUTINE COMPR( RKmin, RKmax )

  ! Computes the reflection coefficient for k in [RKmin, RKmax]

  USE krakcmod
  USE RefCoMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  PARAMETER( RadDeg = 180.0 / PI )

  COMPLEX (KIND=8) :: X, F, G
  CHARACTER BCType*1

  REAL (KIND=8) :: K0, C0
  REAL    (KIND=8), ALLOCATABLE :: Kx(:), Kz( : ), Theta( : ), R(:), phase(:)
  COMPLEX (KIND=8), ALLOCATABLE :: RCmplx( : )

  ALLOCATE( Kx( NkTab ), Kz( NkTab ), RCmplx( NkTab) , R( NkTab ), Theta( NkTab ), phase( NkTab ) )

  N( 1:NMedia ) = NG( 1:NMedia )
  H( 1:NMedia ) = ( Depth( 2:NMedia + 1 ) - Depth( 1:NMedia ) ) / N( 1:NMedia )

  HV( 1 ) = H( 1 )
  CALL INIT

  DeltaK = ( RKmax - RKmin ) / ( NkTab - 1 )

  DO ik = 1, NkTab
     RK = RKmin + ( ik - 1 ) * DeltaK
     X  = RK ** 2

     BCType(1:1) = BotOpt(1:1)
     CALL BCIMP( X, BCType, 'BOT', CPB, CSB, RhoB, F, G, IPow )  ! Bottom impedance
     CALL ACOUST( X, F, G, IPow  )  ! Shoot through acoustic layers
     XTab( ik ) = X
     FTab( ik ) = F
     GTab( ik ) = G
     ITab( ik ) = IPow
  END DO

  IF ( TopOpt(2:2) == 'A' ) THEN
     C0 = CPT                     ! use upper halfspace speed for reference if a halfspace was specified
  ELSE
     C0 = 1500
  END IF

  WRITE( PRTFIL, * )
  WRITE( PRTFIL, * ) 'Reference sound speed = ', C0

  K0     = SQRT( Omega2 ) / C0 ! free-space wavenumber
  Kx     = SQRT( XTab )        ! horizontal wavenumber
  R     = 0.0
  phase = 0.0
  Kz = 0

  WHERE( K0 > Kx )
     Kz = SQRT( K0 ** 2 - Kx ** 2 ) ! vertical   wavenumber
  END WHERE
  Theta     = RadDeg * ATAN2( Kz, Kx )    ! angle of incidence
  RCmplx =  - ( FTab - CI * Kz * GTab ) / ( FTab + CI * Kz * GTab )   ! complex reflection coef.
  R      = ABS( RCmplx )
  phase  = RadDeg * ATAN2( AIMAG( RCmplx ), REAL( RCmplx ) )

  ! unwrap the phase by counting loops in the complex plane
  Loops = 0
  DO itheta = NkTab - 1, 1, -1
     IF ( AIMAG( RCmplx( itheta ) ) > 0 .AND. AIMAG( RCmplx( itheta + 1 ) ) < 0 .AND. &
                                               REAL( RCmplx( itheta + 1 ) ) < 0 ) Loops = Loops + 1
     IF ( AIMAG( RCmplx( itheta ) ) < 0 .AND. AIMAG( RCmplx( itheta + 1 ) ) > 0 .AND. &
                                               REAL( RCmplx( itheta + 1 ) ) < 0 ) Loops = Loops - 1
     phase( itheta ) = phase( itheta ) - Loops * 360
  END DO

  OPEN( FILE = 'IRCFIL', UNIT = IRCFIL, STATUS = 'UNKNOWN' )  ! Internal Reflection Coef. format
  WRITE( IRCFIL, * ) '''', TITLE, '''', Freq
  WRITE( IRCFIL, * ) NkTab
  WRITE( IRCFIL, FMT = "( 5G15.7, I5 )" ) ( XTab( ik ), FTab( ik ), GTab( ik ), ITab( ik ), ik = 1, NkTab )

  OPEN( FILE = 'BRCFIL', UNIT = BRCFIL, STATUS = 'UNKNOWN' )  ! Bottom Reflection Coef. format
  WRITE( BRCFIL, * ) NkTab
  DO ik = NkTab, 1, -1
     WRITE( BRCFIL, * ) Theta( ik ), R( ik ), phase( ik )
  END DO

  RETURN
END SUBROUTINE COMPR
!**********************************************************************C
SUBROUTINE ACOUST( X, F, G, IPow )

  ! Shoot through acoustic layers

  USE krakcmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  COMPLEX (KIND=8) :: X, F, G, P0, P1, P2, H2K2
  PARAMETER ( ROOF = 1.0E5, FLOOR = 1.0E-5, IPowF = -5 )

  IF ( NFACT == 0 ) RETURN

  ! *** Loop over successive acoustic media ***

  DO Med = NLACT, NFACT, -1
     H2K2 = H( Med ) ** 2 * X
     I = LOC( Med ) + N( Med ) + 1
     RhoM = Rho( I )
     P1 = -2.0 * G
     P2 = ( B1( I ) - H2K2 ) * G - 2.0 * H( Med ) * F * RhoM

     ! *** Shoot through a single medium ***

     DO I = LOC( Med ) + N( Med ), LOC( Med ) + 1, -1

        P0 = P1
        P1 = P2
        P2 = ( H2K2 - B1( I ) ) * P1 - P0

        DO WHILE ( ABS( DBLE( P2 ) ) > ROOF )
           P0 = FLOOR * P0
           P1 = FLOOR * P1
           P2 = FLOOR * P2
           IPow = IPow - IPowF
        END DO
     END DO

     ! F = P'/Rho and G = -P since FP+GP'/Rho = 0
     F = -( P2 - P0 ) / ( 2.0 * H( Med ) ) / RhoM
     G = -P1
  END DO

  RETURN
END SUBROUTINE ACOUST
