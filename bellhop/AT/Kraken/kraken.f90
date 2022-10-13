PROGRAM KRAKEN

  ! Program for solving for ocean acoustic normal modes
  ! Michael B. Porter

  USE krakmod
  USE SdRdRMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  INTEGER Min_LOC( 1 )
  REAL (KIND=4)  ZMin, ZMax

  ! *** Loop over a sequence of profiles ***

  DO IPROF = 1, 9999

     NV( 1:5 ) = (/ 1, 2, 4, 8, 16 /)

     ! *** Read in environmental info ***

     TITLE = 'KRAKEN- '

     CALL READIN( TITLE, FREQ, MaxMED, NMEDIA, &
          TOPOPT, CPT, CST, rhoT, BUMDEN, eta, xi, NG, SIGMA, DEPTH, & 
          BOTOPT, CPB, CSB, rhoB, ENVFil, PRTFil )

     READ(  ENVFil, *    ) CLow, CHigh   ! spectral limits
     WRITE( PRTFil, '( /, '' CLow = '', G12.5, ''  CHigh = '', G12.5 )'  ) CLow, CHigh

     READ(  ENVFil, * ) RMax   ! Maximum range for calculations
     WRITE( PRTFil, * ) 'RMax = ', RMax

     ! *** Read source/receiver depths ***

     ZMin = DEPTH( 1 )
     ZMax = DEPTH( NMEDIA + 1 )

     CALL SDRD( ENVFil, PRTFil, ZMin, ZMax )

     omega2 = ( 2.0 * PI * FREQ ) ** 2

     ! *** Main loop: solve the problem for a sequence of meshes ***

     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'Mesh multiplier   CPU seconds'

     DO ISET = 1, NSETS
        N( 1:NMEDIA ) = NG( 1:NMEDIA ) * NV( ISET )
        H( 1:NMEDIA ) = ( DEPTH( 2:NMEDIA + 1 ) - DEPTH( 1:NMEDIA ) ) / N( 1:NMEDIA )
        HV( ISET ) = H( 1 )

        CALL SOLVE( ERROR )

        IF ( ERROR * 1000.0 * RMax < 1.0 ) GOTO 3000
     END DO

     ! Fall through indicates failure to converge
     CALL ERROUT( PRTFil, 'W', 'KRAKEN', 'Too many meshes needed: check convergence' )

     ! Solution complete: discard modes with phase velocity above CHigh

3000 OMEGA = SQRT( omega2 )

     Min_LOC = MINLOC( EXTRAP( 1, 1:M ), EXTRAP( 1, 1:M ) > omega2 / CHigh ** 2 )
     M = Min_LOC( 1 )

     ! *** Write eigenvalues to PRTFil and MODFil ***

     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) '   I          K             ALPHA          PHASE SPEED       GROUP SPEED'

     CK( 1:M ) = SQRT( EXTRAP( 1, 1:M ) + CK( 1:M ) )

     DO MODE = 1, M
        WRITE( PRTFil, "( I5, 4G18.10 )" ) MODE, CK( MODE ), OMEGA / DBLE( CK( MODE ) ), VG( MODE )
     END DO

     WRITE( MODFil, REC = 5 ) M, LRECL

     IFIRST = 1
     DO IREC = 1, 1 + ( 2 * M - 1 ) / LRECL

        ILAST  = MIN( M, IFIRST + LRECL / 2 - 1 )
        WRITE( MODFil, REC = 6 + M + IREC ) ( CMPLX( CK( MODE ) ), MODE = IFIRST, ILAST )

        IFIRST = ILAST + 1
     END DO

     CLOSE( MODFil )

  END DO   ! next profile
  CLOSE( ENVFil )

  STOP
END PROGRAM KRAKEN
! **********************************************************************!
SUBROUTINE INIT

  ! Initializes arrays defining difference equations

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  LOGICAL :: ELFLAG = .FALSE.
  REAL    (KIND=8) :: CP2, CS2
  COMPLEX (KIND=8), ALLOCATABLE :: CP( : ), CS( : )
  CHARACTER TASK*8

  CMin     = 1.0E6
  NFACT    = 0
  LOC( 1 ) = 0

  ! Allocate storage for finite-difference coefficients

  NPTS = SUM( N( 1:NMEDIA ) ) + NMEDIA

  IF ( ALLOCATED( B1 ) ) DEALLOCATE( B1, B1C, B2, B3, B4, rho )
  ALLOCATE ( B1( NPTS ), B1C( NPTS ), B2( NPTS ), B3( NPTS ), B4( NPTS ), rho( NPTS ), &
       CP( NPTS ), CS( NPTS ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) &
       CALL ERROUT( PRTFil, 'F', 'KRAKEN - INIT', 'Insufficient memory: Reduce mesh.' )

  DO MED = 1, NMEDIA   ! *** Loop over media ***
     IF ( MED /= 1 ) LOC( MED ) = LOC( MED-1 ) + N( MED-1 ) + 1
     N1 = N(   MED ) + 1
     I  = LOC( MED ) + 1

     ! *** PROFIL reads in the data for a medium ***

     TASK = 'TAB'
     CALL PROFIL( DEPTH, CP( I ), CS( I ), rho( I ), MED, N1, FREQ, TOPOPT(1:1), TOPOPT(3:4), TASK, ENVFil, PRTFil )

     ! *** Load diagonals of the finite-difference equations ***

     IF ( REAL( CS( I ) ) == 0.0 ) THEN ! --- Case of an acoustic medium ---

        MATER( MED ) = 'ACOUSTIC'
        IF ( NFACT == 0 ) NFACT = MED
        NLACT = MED

        CMin = MIN( CMin, MINVAL( DBLE( CP( I:I + N( MED ) ) ) ) )

        B1(  I:I+N(MED) ) = -2.0 + H( MED ) ** 2 * DBLE( omega2 / CP( I:I+N(MED) ) ** 2 )
        B1C( I:I+N(MED) ) = AIMAG( omega2 / CP( I:I+N(MED) ) ** 2 )
     ELSE                               ! --- Case of an elastic medium ---

        IF ( SIGMA( MED ) /= 0.0 )&
             CALL ERROUT( PRTFil, 'F', 'KRAKEN', 'Rough elastic interfaces are not allowed' )

        MATER( MED ) = 'ELASTIC'
        ELFLAG = .TRUE.
        TWOH   = 2.0 * H( MED )

        DO J = I, I + N( MED )
           CMin = MIN( DBLE( CS( J ) ), CMin )

           CP2 = CP( J ) ** 2
           CS2 = CS( J ) ** 2

           B1( J ) = TWOH / ( rho( J ) * CS2 )
           B2( J ) = TWOH / ( rho( J ) * CP2 )
           B3( J ) = 4.0 * TWOH * rho( J ) * CS2 * ( CP2 - CS2 ) / CP2
           B4( J ) = TWOH * ( CP2 - 2.0 * CS2 ) / CP2

           rho( J ) = TWOH * omega2 * rho( J )
        END DO

     ENDIF
  END DO   ! next MED

  ! (CLow, CHigh) = phase speed interval for the mode search
  ! user specified interval is reduced if it exceeds domain
  ! of possible mode phase speeds

  ! *** Bottom properties ***

  IF ( BOTOPT(1:1) == 'A' ) THEN
     IF ( REAL( CSB ) > 0.0 ) THEN     ! Elastic bottom
        ELFLAG = .TRUE.
        CMin  = MIN( CMin,  DBLE( CSB ) )
        CHigh = MIN( CHigh, DBLE( CSB ) )
     ELSE                                 ! Acoustic bottom
        CMin  = MIN( CMin,  DBLE( CPB ) )
        CHigh = MIN( CHigh, DBLE( CPB ) )
     ENDIF
  ENDIF

  ! *** Top properties ***

  IF ( TOPOPT(2:2) == 'A' ) THEN
     IF ( REAL( CST ) > 0.0 ) THEN     ! Elastic  top half-space
        ELFLAG = .TRUE.
        CMin =  MIN( CMin,  DBLE( CST ) )
        CHigh = MIN( CHigh, DBLE( CST ) )
     ELSE                                 ! Acoustic top half-space
        CMin  = MIN( CMin,  DBLE( CPT ) )
        CHigh = MIN( CHigh, DBLE( CPT ) )
     ENDIF
  ENDIF

  ! --- If elastic medium then reduce CMin for Scholte wave
  IF ( ELFLAG ) CMin = 0.85 * CMin
  CLow = MAX( CLow, CMin )

  RETURN
END SUBROUTINE INIT
! **********************************************************************!
SUBROUTINE SOLVE( ERROR )

  ! Solves the eigenvalue problem at the current mesh
  ! and produces a new extrapolation

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  CALL CPU_TIME( Tstart )
  CALL INIT

  ! Choose a solver ...

  IF ( IPROF > 1 .AND. ISET <= 2 .AND. TOPOPT(4:4) == 'C' ) THEN
     ! use continuation from last profile if option selected and we're doing the first or second mesh
     CALL SOLVE3
  ELSE IF ( ( ISET <= 2 ) .AND. ( NMEDIA <= NLACT-NFACT+1 ) ) THEN
     ! use bisection for first two sets if possible (applicable if elasticity is limited to halfspaces)
     CALL SOLVE1
  ELSE
     ! use extrapolation from first two meshes
     CALL SOLVE2
  ENDIF

  EXTRAP( ISET, 1:M ) = EVMAT( ISET, 1:M )

  IF ( ISET == 1 ) CALL VECTOR   ! *** If this is the first mesh, compute the eigenvectors ***

  ! *** Now do the Richardson extrapolation ***

  ERROR = 10            ! initialize error to a large number
  KEY = 2 * M / 3 + 1   ! KEY value used to check convergence

  IF ( ISET > 1 ) THEN
     T1 = EXTRAP( 1, KEY )

     DO J = ISET - 1, 1, -1
        DO MODE = 1, M
           X1 = NV( J    ) ** 2
           X2 = NV( ISET ) ** 2
           F1 = EXTRAP( J,     MODE )
           F2 = EXTRAP( J + 1, MODE )
           EXTRAP( J, MODE ) = F2 - ( F1 - F2 ) / ( X2 / X1 - 1.0 )
        END DO
     END DO

     T2 = EXTRAP( 1, KEY )
     ERROR = ABS( T2 - T1 )
  ENDIF

  CALL CPU_TIME( Tend )
  ET( ISET ) = Tend - Tstart
  WRITE( PRTFil, '( 1X, I8, 6X, G15.3 )' ) NV( ISET ), ET( ISET )

  RETURN
END SUBROUTINE SOLVE
! **********************************************************************!
SUBROUTINE SOLVE2

  ! Provides initial guess to root finder for each EVMAT(I)

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL (KIND=8) P( 10 )
  CHARACTER ERRMSG*80

  X = omega2 / CLow ** 2
  MaxIT = 500

  ! solve1 has already allocated space for the following unless the problem has shear
  IF ( .NOT. ALLOCATED( CK ) ) THEN
     M = 3000   ! this sets the upper limit on how many modes can be calculated
     ALLOCATE( EVMAT( NSETS, M ), EXTRAP( NSETS, M ), CK( M ), VG( M ), Stat = IAllocStat )
     IF ( IAllocStat /= 0 ) &
          CALL ERROUT( PRTFil, 'F', 'KRAKEN - SOLVE2', 'Insufficient memory (too many modes).' )
  END IF

  DO MODE = 1, M

     ! For first or second meshes, use a high guess
     ! Otherwise use extrapolation to produce an initial guess

     X = 1.00001 * X

     IF ( ISET >= 2 ) THEN

        P( 1:ISET - 1 ) = EVMAT( 1:ISET - 1, MODE )

        IF ( ISET >= 3 ) THEN
           DO I = 1, ISET - 2
              DO J = 1, ISET - I - 1
                 X1 = HV( J     ) ** 2
                 X2 = HV( J + I ) ** 2

                 P( J ) = ( ( HV( ISET ) ** 2 - X2 ) * P( J     ) - &
                      ( HV( ISET ) ** 2 - X1 ) * P( J + 1 ) ) &
                      / ( X1 - X2 )
              END DO
           END DO
           X = P( 1 )
        ENDIF

     ENDIF

     !    *** Use the secant method to refine the eigenvalue ***

     TOL = ABS( X ) * 10.0 ** ( 4.0 - PRECISION( X ) )
     CALL ZSECX( X, TOL, IT, MaxIT, ERRMSG )

     IF ( ERRMSG /= ' ' ) THEN
        WRITE( PRTFil, * ) 'ISET, MODE = ', ISET, MODE
        CALL ERROUT( PRTFil, 'W', 'KRAKEN-ZSECX', ERRMSG )
        X = TINY( X )   ! make sure value is discarded
     ENDIF

     EVMAT( ISET, MODE ) = X

     ! Toss out modes outside user specified spectrum
     IF ( omega2 / X > CHigh ** 2 ) THEN
        M = MODE - 1
        RETURN
     ENDIF

  END DO

  RETURN
END SUBROUTINE SOLVE2
! **********************************************************************!
SUBROUTINE SOLVE3

  ! Provides initial guess to root finder for each EVMAT(I)
  ! This solver tries to use eigenvalues from a previous profile
  ! as initial guesses

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  CHARACTER ERRMSG*80

  MaxIT = 500

  ! *** Determine number of modes ***

  XMin = 1.00001D0 * omega2 / CHigh ** 2

  CALL FUNCT( XMin, DELTA, IPOW )
  M = MODECT

  DO MODE = 1, M

     X = EVMAT( ISET, MODE )
     TOL = ABS( X ) * 10.0 ** ( 2.0 - PRECISION( X ) )
     CALL ZSECX( X, TOL, IT, MaxIT, ERRMSG )  ! Use the secant method to refine the eigenvalue ***

     IF ( ERRMSG /= ' ' ) THEN
        WRITE( PRTFil, * ) 'ISET, MODE = ', ISET, MODE
        CALL ERROUT( PRTFil, 'W', 'KRAKEN-ZSECX', ERRMSG )
        X = TINY( X )   ! make sure value is discarded
     ENDIF

     EVMAT( ISET, MODE ) = X

     IF ( omega2 / X > CHigh ** 2 ) THEN  ! Toss out modes outside user specified spectrum
        M = MODE - 1
        RETURN
     ENDIF

  END DO

  RETURN
END SUBROUTINE SOLVE3
! **********************************************************************!
SUBROUTINE FUNCT( X, DELTA, IPOW )

  ! FUNCT( X ) = 0 is the dispersion relation

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  PARAMETER ( ROOF = 1.0E5, FLOOR = 1.0E-5, IPOWR = 5, IPOWF = -5 )
  CHARACTER BCTYPE*1

  IF ( X <= omega2 / CHigh ** 2 ) THEN    ! For a k below the cts spectrum limit, force a zero
     DELTA = 0.0
     IPOW  = 0
     RETURN
  ENDIF

  MODECT = 0

  BCTYPE(1:1) = BOTOPT(1:1)
  CALL BCIMP( X, BCTYPE, 'BOT', CPB, CSB, rhoB, F, G, IPOW )      ! Bottom impedance

  CALL ACOUST( X, F, G, IPOW  )                                   ! Shoot through acoustic layers

  BCTYPE(1:1) = TOPOPT(2:2)
  CALL BCIMP( X, BCTYPE, 'TOP', CPT, CST, rhoT, F1, G1, IPOW1 )   ! Top impedance

  DELTA = F * G1 - G * F1
  IF ( G * DELTA > 0.0 ) MODECT = MODECT + 1
  IPOW = IPOW + IPOW1

  ! *** Deflate previous roots ***

  IF ( ( MODE > 1 ) .AND. ( NMEDIA > NLACT - NFACT+1 ) ) THEN
     DELTA = DELTA / ( X - EVMAT( ISET, MODE-1 ) )

     IF ( MODE > 2 ) THEN
        DO J = 1, MODE - 2
           DELTA = DELTA / ( X - EVMAT(ISET, J) )

           ! Scale if necessary
           DO WHILE ( ABS( DELTA ) < FLOOR .AND. ABS( DELTA ) > 0.0 )
              DELTA = ROOF * DELTA
              IPOW  = IPOW - IPOWR
           END DO

           DO WHILE ( ABS( DELTA ) > ROOF )
              DELTA = FLOOR * DELTA
              IPOW  = IPOW - IPOWF
           END DO

        END DO
     ENDIF
  ENDIF

  RETURN
END SUBROUTINE FUNCT
! **********************************************************************!
SUBROUTINE ACOUST( X, F, G, IPOW )

  ! Shoot through acoustic layers

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  PARAMETER ( ROOF = 1.0E20, FLOOR = 1.0E-20, IPOWF = -20 )

  IF ( NFACT == 0 ) RETURN

  DO MED = NLACT, NFACT, -1    ! *** Loop over successive acoustic media ***
     H2K2 = H( MED ) ** 2 * X
     I    = LOC( MED ) + N( MED ) + 1
     rhoM = rho( I )
     P1   = -2.0 * G
     P2   = ( B1( I ) - H2K2 ) * G - 2.0 * H( MED ) * F * rhoM

     ! *** Shoot through a single medium ***

     DO I = LOC( MED ) + N( MED ), LOC( MED ) + 1, -1

        P0 = P1
        P1 = P2
        P2 = ( H2K2 - B1( I ) ) * P1 - P0

        IF ( P0 * P1 <= 0.0 ) MODECT = MODECT + 1

        DO WHILE ( ABS( P2 ) > ROOF )   ! Scale if necessary
           P0 = FLOOR * P0
           P1 = FLOOR * P1
           P2 = FLOOR * P2
           IPOW = IPOW - IPOWF
        END DO

     END DO

     ! F = P' / rho and G = -P since F P + G P' / rho = 0
     rhoM = rho( LOC( MED ) + 1 )
     F = -( P2 - P0 ) / ( 2.0 * H( MED ) ) / rhoM
     G = -P1
  END DO

  RETURN
END SUBROUTINE ACOUST

! **********************************************************************!

SUBROUTINE VECTOR

  ! Do inverse iteration to compute each of the eigenvectors
  ! and write these to the disk file

  USE krakmod
  USE SdRdRMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  INTEGER, ALLOCATABLE :: IZTAB( : )
  REAL ZTAB( NSD + NRD )
  REAL, ALLOCATABLE :: Z( : ), WTS( : )
  REAL (KIND=8), ALLOCATABLE :: PHI( : ), D( : ), E( : ), RV1( : ), RV2( : ), RV3( : ), RV4( : )
  COMPLEX, ALLOCATABLE :: PHITAB( : )  ! this could be made real to save space
  CHARACTER BCTOP*1, BCBOT*1, FilNAMT*20

  BCTOP(1:1) = TOPOPT(2:2)
  BCBOT(1:1) = BOTOPT(1:1)

  ! *** Tabulate z-coordinates and off-diagonals of matrix ***

  NTOT = SUM( N( NFACT:NLACT ) )
  NTOT1 = NTOT + 1

  ALLOCATE( Z( NTOT1 ), E( NTOT1 + 1 ), D( NTOT1 ), PHI( NTOT1 ), &
       RV1( NTOT1 ), RV2( NTOT1 ), RV3( NTOT1 ), RV4( NTOT1 ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFil, 'F', 'KRAKEN - VECTOR', 'Insufficient memory: Reduce mesh.' )

  J      = 1
  Z( 1 ) = DEPTH( NFACT )

  DO MED = NFACT, NLACT

     Hrho = H( MED ) * rho( LOC( MED ) + 1 )

     E( J+1 : J + N( MED ) ) = 1.0 / Hrho
     Z( J+1 : J + N( MED ) ) = Z( J ) + H( MED ) * (/ (JJ, JJ = 1, N(MED) ) /)

     J = J + N( MED )
  END DO
  E( NTOT1 + 1 ) = 1.0 / Hrho       ! dummy value; never used

  ! Calculate the indices, weights, ... for mode interpolation

  CALL MERGEV( SD, NSD, RD, NRD, ZTAB, NZTAB )

  ALLOCATE( WTS( NZTAB ), IZTAB( NZTAB ), PHITAB( NZTAB ) )
  CALL WEIGHT( Z, NTOT1, ZTAB, NZTAB, WTS, IZTAB )

  ! Open MODFil and write header

  LRECL = MAX( 2 * NZTAB, 32, 3 * ( NLACT - NFACT + 1 ) )   ! Logical record length in `longwords' (4 bytes)

  WRITE( FilNAMT, FMT="( 'MODFil', I4.4 )" ) IPROF
  OPEN ( FILE = FilNAMT, UNIT = MODFil, ACCESS = 'DIRECT', RECL = 4 * LRECL, FORM = 'UNFORMATTED' )

  WRITE( MODFil, REC = 1 ) LRECL, TITLE, REAL( FREQ ), NLACT - NFACT + 1, NZTAB, NZTAB
  WRITE( MODFil, REC = 2 ) ( N( MED ), MATER( MED ), MED = NFACT, NLACT )
  WRITE( MODFil, REC = 3 ) BCTOP(1:1), CMPLX( CPT ), CMPLX( CST ), REAL( rhoT ), REAL( DEPTH( 1          ) ), &
       BCBOT(1:1), CMPLX( CPB ), CMPLX( CSB ), REAL( rhoB ), REAL( DEPTH( NMEDIA + 1 ) )
  WRITE( MODFil, REC = 4 ) ( REAL( DEPTH( MED ) ), REAL( rho( LOC( MED ) + 1 ) ), MED = NFACT, NLACT )
  WRITE( MODFil, REC = 6 ) ZTAB( 1:NZTAB)

  ! *** Main loop: for each eigenvalue call SINVIT to get eigenvector

  DO MODE = 1, M
     X = EVMAT( 1, MODE )

     ! *** Corner elt requires top impedance ***

     CALL BCIMP( X, BCTOP, 'TOP', CPT, CST, rhoT, F, G, IPOW )

     IF ( G == 0.0 ) THEN
        D( 1 ) = 1.0   ;   E( 2 ) = EPSILON( D( 1 ) )
     ELSE
        L = LOC( NFACT ) + 1
        XH2    = X * H( NFACT ) * H( NFACT )
        Hrho   = H( NFACT ) * rho( L )
        D( 1 ) = ( B1( L ) - XH2 ) / Hrho / 2.0 + F / G
     ENDIF

     ! *** Set up the diagonal ***

     ITP = NTOT
     J   = 1
     L   = LOC( NFACT ) + 1

     DO MED = NFACT, NLACT
        XH2  = X * H( MED ) ** 2
        Hrho = H( MED ) * rho( LOC( MED ) + 1 )

        IF ( MED >= NFACT + 1 ) THEN
           L = L + 1
           D( J ) = ( D( J ) + ( B1( L ) - XH2 ) / Hrho ) / 2.0
        ENDIF

        DO I = 1, N( MED )
           J = J + 1   ;   L = L + 1
           D( J ) = ( B1( L ) - XH2 ) / Hrho

           IF ( B1( L ) - XH2 + 2.0 > 0.0 ) THEN    ! Find index of turning point nearest top
              ITP = MIN( J, ITP )
           ENDIF
        END DO

     END DO

     ! *** Corner elt requires bottom impedance ***

     CALL BCIMP( X, BCBOT, 'BOT', CPB, CSB, rhoB, F, G, IPOW )

     IF ( G == 0.0 ) THEN
        D( NTOT1 ) = 1.0   ;   E( NTOT1 ) = EPSILON( D( NTOT1 ) )
     ELSE
        D( NTOT1 ) =  D( NTOT1 ) / 2.0 - F / G
     ENDIF

     CALL SINVIT( NTOT1, D, E, IERR, RV1, RV2, RV3, RV4, PHI )   ! Inverse iteration to compute eigenvector

     IF ( IERR /= 0 ) THEN
        WRITE( PRTFil, * ) 'MODE = ', MODE
        CALL ERROUT( PRTFil, 'W', 'KRAKEN-SINVIT', 'Inverse iteration failed to converge' )
     ENDIF

     CALL NORMIZ( PHI, ITP, NTOT1, X )  ! *** Normalize the eigenvector ***

     ! Tabulate the modes at the source/rcvr depths and write to disK

     PHITAB = PHI( IZTAB ) + WTS * ( PHI( IZTAB + 1 ) - PHI( IZTAB ) )
     WRITE( MODFil, REC = 6 + MODE ) PHITAB
  END DO

  DEALLOCATE( Z, E, D, PHI, RV1, RV2, RV3, RV4 )

  RETURN
END SUBROUTINE VECTOR
! **********************************************************************!
SUBROUTINE NORMIZ( PHI, ITP, NTOT1, X )

  ! Normalize the eigenvector:
  !    SQNRM = Integral(PHI ** 2) by the trapezoidal rule:
  !    Integral(F) = H*( F(1)+...+F(N-1) + 0.5*(F(0)+F(N)) )

  ! Compute perturbation due to material absorption
  ! Compute the group velocity
  ! Call SCAT to figure interfacial scatter loss

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL    (KIND=8) PHI( NTOT1 ), SLOW
  COMPLEX (KIND=8) :: PERK, DEL
  CHARACTER BCTYPE*1

  SQNRM = 0.0
  PERK  = 0.0
  SLOW = 0.0

  ! *** Compute perturbation due to loss in top half-space ***

  IF ( TOPOPT(2:2) == 'A' ) THEN
     DEL = -0.5*( omega2 / CPT ** 2 - DBLE( omega2 / CPT ** 2 ) ) / &
          SQRT( X                 - DBLE( omega2 / CPT ** 2 ) )
     PERK = PERK - DEL * PHI( 1 ) ** 2 / rhoT
     SLOW  = SLOW + PHI( 1 ) ** 2 / ( 2 * SQRT( X - DBLE( omega2 / CPT ** 2 ) ) ) / ( rhoT * DBLE( CPT ) ** 2 )

  ENDIF

  ! *** Compute norm and pertubation due to material absorption ***

  L = LOC( NFACT )
  J = 1

  DO MED = NFACT, NLACT
     L = L + 1
     rhoM = rho( L )
     rhoOMH2 = rhoM * omega2 * H( MED ) ** 2

     ! top interface
     SQNRM = SQNRM + 0.5 * H( MED ) *                  PHI( J ) ** 2 / rhoM
     PERK  = PERK  + 0.5 * H( MED ) * CI * B1C(L)    * PHI( J ) ** 2 / rhoM
     SLOW  = SLOW  + 0.5 * H( MED ) * ( B1(L) + 2. ) * PHI( J ) ** 2 / rhoOMH2

     ! medium
     L1 = L + 1   ;   L = L + N( MED ) - 1
     J1 = J + 1   ;   J = J + N( MED ) - 1
     SQNRM = SQNRM +  H( MED ) *      SUM(                     PHI( J1:J ) ** 2 ) / rhoM
     PERK  = PERK  +  H( MED ) * CI * SUM(  B1C(L1:L) *        PHI( J1:J ) ** 2 ) / rhoM
     SLOW  = SLOW  +  H( MED ) *      SUM( ( B1(L1:L) + 2. ) * PHI( J1:J ) ** 2 ) / rhoOMH2

     ! bottom interface
     L = L + 1   ;   J = J + 1
     SQNRM = SQNRM + 0.5 * H( MED ) *                  PHI( J ) ** 2 / rhoM
     PERK  = PERK  + 0.5 * H( MED ) * CI * B1C(L)    * PHI( J ) ** 2 / rhoM
     SLOW  = SLOW  + 0.5 * H( MED ) * ( B1(L) + 2. ) * PHI( J ) ** 2 / rhoOMH2

  END DO

  ! *** Compute perturbation due to loss in bottom half-space ***

  IF ( BOTOPT(1:1) == 'A' ) THEN
     DEL = -0.5*( omega2 / CPB ** 2 - DBLE( omega2 / CPB ** 2 ) ) / &
          SQRT( X                 - DBLE( omega2 / CPB ** 2 ) )
     PERK = PERK - DEL * PHI( J ) ** 2 / rhoB
     SLOW  = SLOW + PHI( J ) ** 2 / ( 2 * SQRT( X - DBLE( omega2 / CPB ** 2 ) ) ) / ( rhoB * DBLE( CPB ) ** 2 )

  ENDIF

  ! *** Compute derivative of top admitance ***

  X1 = 0.9999999D0 * X
  X2 = 1.0000001D0 * X

  BCTYPE(1:1) = TOPOPT(2:2)

  CALL BCIMP( X1, BCTYPE, 'TOP', CPT, CST, rhoT, F1, G1, IPOW )
  CALL BCIMP( X2, BCTYPE, 'TOP', CPT, CST, rhoT, F2, G2, IPOW )

  DrhoDX = 0.0
  IF ( G1 /= 0.0 ) DrhoDX = -( F2 / G2 - F1 / G1 ) / ( X2 - X1 )

  ! *** Compute derivative of bottom admitance ***

  BCTYPE(1:1) = BOTOPT(1:1)

  CALL BCIMP( X1, BCTYPE, 'BOT', CPB, CSB, rhoB, F1, G1, IPOW )
  CALL BCIMP( X2, BCTYPE, 'BOT', CPB, CSB, rhoB, F2, G2, IPOW )

  DetaDX = 0.0
  IF ( G1 /= 0.0 ) DetaDX = -( F2 / G2 - F1 / G1 ) / ( X2 - X1 )

  ! *** Scale the mode ***

  RN = SQNRM + DrhoDX * PHI( 1 ) ** 2 - DetaDX * PHI( NTOT1 ) ** 2

  IF ( RN <= 0.0 ) THEN
     RN = -RN
     WRITE( PRTFil, * ) 'MODE = ', MODE
     CALL ERROUT( PRTFil, 'W', 'KRAKEN', 'Normalization constant non-positive' )
     WRITE( *, * ) PHI
  ENDIF

  SCALEF = 1.0 / SQRT( RN )
  IF ( PHI( ITP ) < 0.0 ) SCALEF = -SCALEF

  PHI( 1:NTOT1 ) = SCALEF * PHI( 1:NTOT1 )

  PERK = SCALEF ** 2 * PERK
  SLOW = SCALEF ** 2 * SLOW * SQRT( omega2 / X )
  VG( MODE )   = 1 / SLOW

  ! *** Compute interfacial scatter loss ***

  CALL SCAT( PERK, PHI, X )

  RETURN
END SUBROUTINE NORMIZ
! **********************************************************************!
SUBROUTINE SCAT( PERK, PHI, X )

  ! Figure scatter loss

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL    (KIND=8) PHI( * )
  COMPLEX (KIND=8) CIMPED, KX, TWERSK, PERK, eta1SQ, eta2SQ, KUPING, U, PHIC
  CHARACTER BCTYPE*1

  OMEGA = SQRT( omega2 )
  KX    = SQRT( X )

  ! *** Top Twersky roughness ***

  BCTYPE(1:1) = TOPOPT(2:2)
  IF ( BCTYPE(1:1) == 'S' .OR. BCTYPE(1:1) == 'H' .OR. &
       BCTYPE(1:1) == 'T' .OR. BCTYPE(1:1) == 'I' ) THEN

     I = LOC( NFACT ) + N( NFACT ) + 1
     rhoINS = rho( I )
     CINS = SQRT( omega2 * H( NFACT ) ** 2 / ( 2.0 + B1( NFACT ) ) )

     CIMPED = TWERSK( BCTYPE(1:1), OMEGA, BUMDEN, xi, eta, KX, rhoINS, CINS )

     CIMPED = CIMPED / ( -CI * OMEGA * rhoINS )
     DPHIDZ = PHI( 2 ) / H( NFACT )
     PERK   = PERK - CIMPED * DPHIDZ ** 2
  ENDIF

  ! *** Bottom Twersky roughness ***

  BCTYPE(1:1) = BOTOPT(1:1)
  IF ( BCTYPE(1:1) == 'S' .OR. BCTYPE(1:1) == 'H' .OR. &
       BCTYPE(1:1) == 'T' .OR. BCTYPE(1:1) == 'I' ) THEN

     I = LOC( NLACT ) + N( NLACT ) + 1
     rhoINS = rho( I )
     CINS   = SQRT( omega2 * H(NLACT) ** 2 / ( 2.0 + B1(NLACT) ) )

     CIMPED = TWERSK( BCTYPE(1:1), OMEGA, BUMDEN, xi, eta, KX, rhoINS, CINS )

     CIMPED = CIMPED / ( -CI * OMEGA * rhoINS )
     DPHIDZ = PHI( 2 ) / H( NFACT )
     PERK   = PERK - CIMPED * DPHIDZ ** 2
  ENDIF

  J = 1
  L = LOC( NFACT )

  DO MED = NFACT - 1, NLACT  ! *** Loop over media ***

     ! *** Calculate rho1, eta1SQ, PHI, U ***

     IF ( MED == NFACT - 1 ) THEN
        ! *** Top properties ***

        BCTYPE(1:1) = TOPOPT(2:2)

        SELECT CASE ( BCTYPE(1:1) )
        CASE ( 'A' )          ! Acousto-elastic
           rho1   = rhoT
           eta1SQ = X - omega2 / CPT ** 2
           U = SQRT( eta1SQ ) * PHI( 1 ) / rhoT
        CASE ( 'V' )          ! Vacuum
           rho1   = 1.0E-9
           eta1SQ = 1.0
           rhoINS = rho( LOC( NFACT ) + 1 )
           U      = PHI( 2 ) / H( NFACT ) / rhoINS
        CASE ( 'R' )          ! Rigid
           rho1   = 1.0E+9
           eta1SQ = 1.0
           U      = 0.0
        END SELECT
     ELSE
        H2 = H( MED ) ** 2
        J  = J + N( MED )
        L  = LOC( MED ) + N( MED ) + 1

        rho1   = rho( L )
        eta1SQ = ( 2.0 + B1( L ) ) / H2 - X
        U = ( -PHI( J-1 ) - 0.5 * ( B1(L) - H2*X ) * PHI( J ) ) / ( H( MED ) * rho1 )
     ENDIF

     ! *** Calculate rho2, eta2 ***

     IF ( MED == NLACT ) THEN
        ! *** Bottom properties ***

        BCTYPE(1:1) = BOTOPT(1:1)
        SELECT CASE ( BCTYPE(1:1) )
        CASE ( 'A' )          ! Acousto-elastic
           rho2   = rhoB
           eta2SQ = omega2 / CPB ** 2 - X
        CASE ( 'V' )          ! Vacuum
           rho2   = 1.0E-9
           eta2SQ = 1.0
        CASE ( 'R' )          ! Rigid
           rho2   = 1.0E+9
           eta2SQ = 1.0
        END SELECT
     ELSE
        rho2   = rho( L + 1 )
        eta2SQ = ( 2.0 + B1( L + 1 ) ) / H( MED + 1 ) ** 2 - X
     ENDIF

     PHIC = PHI( J )   ! convert to complex*16
     PERK = PERK + KUPING( SIGMA( MED + 1 ), eta1SQ, rho1, eta2SQ, rho2, PHIC, U )

  END DO

  CK( MODE ) = PERK

  RETURN
END SUBROUTINE SCAT
! **********************************************************************!
SUBROUTINE SOLVE1

  ! Uses Sturm sequences to isolate the eigenvalues
  ! and Brent's method to refine them

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL (KIND=8), ALLOCATABLE :: XL( : ), XR( : )
  CHARACTER ERRMSG*80, FilNAMT*20

  ! *** Determine number of modes ***

  XMin = 1.00001D0 * omega2 / CHigh ** 2

  CALL FUNCT( XMin, DELTA, IPOW )
  M = MODECT
  WRITE( PRTFil, * ) ' --- Number of modes = ', M

  IF ( ALLOCATED( XL ) ) DEALLOCATE( XL, XR )
  ALLOCATE( XL( M + 1 ), XR( M + 1 ) )

  IF ( ISET == 1 ) THEN
     IF ( ALLOCATED ( EVMAT ) ) DEALLOCATE( EVMAT, EXTRAP, CK, VG )
     ALLOCATE( EVMAT( NSETS, M ), EXTRAP( NSETS, M ), CK( M ), VG( M ), Stat = IAllocStat )
     IF ( IAllocStat /= 0 ) &
          CALL ERROUT( PRTFil, 'F', 'KRAKEN - SOLVE1', 'Insufficient memory (too many modes).' )
  END IF

  XMax = omega2 / CLow ** 2
  CALL FUNCT( XMax, DELTA, IPOW )
  M = M - MODECT

  IF ( M == 0 ) THEN   ! Open a dummy MODFil for possible use by FIELD3D

     LRECL = 32   ;   NZTAB = 0

     ! Open MODFil and write header

     WRITE( FilNAMT, FMT="( 'MODFil', I4.4 )" ) IPROF
     OPEN ( FILE = FilNAMT, UNIT = MODFil, ACCESS = 'DIRECT', RECL = 4 * LRECL, FORM = 'UNFORMATTED' )

     WRITE( MODFil, REC = 1 ) LRECL, TITLE, REAL( FREQ ), 1, NZTAB, NZTAB
     WRITE( MODFil, REC = 5 ) M, LRECL

     CALL ERROUT( PRTFil, 'F', 'KRAKEN', 'No modes for given phase speed interval' )

  ENDIF

  NTOT = SUM( N( NFACT:NLACT ) )

  IF ( M > NTOT / 5 ) THEN
     WRITE( PRTFil, * ) 'Approximate number of modes =', M
     CALL ERROUT( PRTFil, 'W', 'KRAKEN', 'Mesh too coarse to sample the modes adequately' )
  ENDIF

  CALL BISECT( XMin, XMax, XL, XR )   ! *** Initialize upper and lower bounds ***

  ! *** Call ZBRENT to refine each eigenvalue in turn ***

  DO MODE = 1, M
     X1  = XL( MODE )   ;   X2  = XR( MODE )
     EPS = ABS( X2 ) * 10.0 ** ( 2.0 - PRECISION( X2 ) )
     CALL ZBRENTX( X, X1, X2, EPS, ERRMSG )

     IF ( ERRMSG /= ' ' ) THEN
        WRITE( PRTFil, * ) 'ISET, MODE = ', ISET, MODE
        CALL ERROUT( PRTFil, 'W', 'KRAKEN-ZBRENTX', ERRMSG )
     ENDIF

     EVMAT( ISET, MODE ) = X
  END DO

  DEALLOCATE( XL, XR )

  RETURN
END SUBROUTINE SOLVE1
! **********************************************************************!
SUBROUTINE BISECT( XMin, XMax, XL, XR )

  ! Returns an isolating interval (XL, XR) for each eigenvalue

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  PARAMETER( MXBIS = 50 )

  REAL (KIND=8) XL( M + 1 ), XR( M + 1 )

  ! initialize left and right bounds

  XL = XMin
  XR = XMax

  CALL FUNCT( XMax, DELTA, IPOW )
  NZER1 = MODECT

  IF ( M == 1 ) RETURN   ! quick exit if only one mode is sought

  ! *** For each eigenvalue, do: ***

  MODELOOP: DO MODE = 1, M - 1

     ! Obtain initial guesses for X1 and X2

     IF ( XL( MODE ) == XMin ) THEN
        X2 = XR( MODE )
        X1 = MAX( MAXVAL( XL( MODE + 1 : M ) ), XMin )

        ! Begin bisection (allowing no more than MXBIS bisections per mode)

        DO J = 1, MXBIS
           X = X1 + ( X2 - X1 ) / 2
           CALL FUNCT( X, DELTA, IPOW )
           NZEROS = MODECT - NZER1

           IF ( NZEROS < MODE ) THEN   ! not too many zeros, this is a new right bdry
              X2 = X
              XR( MODE ) = X
           ELSE                        ! this is a new left bdry
              X1 = X
              IF ( XR( NZEROS + 1 ) >= X ) XR( NZEROS + 1 ) = X
              IF ( XL( NZEROS     ) <= X ) XL( NZEROS     ) = X
           ENDIF

           ! when we have replaced the default, initial values, we are done
           IF ( XL( MODE ) /= XMin ) CYCLE MODELOOP
        END DO
     ENDIF
  END DO MODELOOP

  RETURN
END SUBROUTINE BISECT
