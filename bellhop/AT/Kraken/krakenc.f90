PROGRAM KRAKENC

  ! Program for solving for ocean acoustic normal modes
  ! Michael B. Porter                                                 

  USE krakcmod
  USE SdRdRMod
  USE RefCoMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  INTEGER Min_LOC( 1 )
  REAL (KIND=4) :: ZMin, ZMax

  ! *** Loop over a sequence of profiles ***                          

  DO IPROF = 1, 9999

     NV( 1:5 ) = (/ 1, 2, 4, 8, 16 /) 

     ! *** Read in environmental info ***                                

     TITLE = 'KRAKENC-' 

     CALL READIN( TITLE, FREQ, MaxMed, NMedia,                         &
          TopOpt, CPT, CST, RhoT, BUMDEN, Eta, XI, NG, Sigma, Depth,     &
          BotOpt, CPB, CSB, RhoB, ENVFIL, PRTFIL )

     READ(  ENVFIL, *    ) CLOW, CHIGH   ! Spectral limits
     WRITE( PRTFIL, "( /, ' CLOW = ', G12.5, '  CHIGH = ', G12.5 )" ) CLOW, CHIGH

     READ(  ENVFIL, * ) RMax   ! Maximum range for calculations
     WRITE( PRTFIL, * ) 'RMax = ', RMax

     ZMin = Depth( 1 ) 
     ZMax = Depth( NMedia + 1 )
     CALL SDRD( ENVFIL, PRTFIL, ZMin, ZMax )   ! Read source/receiver depths

     omega2 = ( 2.0 * PI * FREQ ) ** 2 

     CALL READRC( BotOpt(1:1), TopOpt(2:2), PRTFIL )   ! Optionally read in Bot, Top reflection coefficients

     M = MaxM 

     ! *** Main loop: solve the problem for a sequence of meshes ***     

     WRITE( PRTFIL, * ) 
     WRITE( PRTFIL, * ) 'Mesh multiplier   CPU seconds' 

     DO ISET = 1, NSETS 

        N( 1:NMedia ) = NG( 1:NMedia ) * NV( ISET )
        H( 1:NMedia ) = ( Depth( 2:NMedia + 1 ) - Depth( 1:NMedia ) ) / N( 1:NMedia )
        HV( ISET ) = H( 1 )

        CALL SOLVE( ERROR ) 

        IF ( ERROR * 1000.0 * RMax < 1.0 ) GOTO 3000 
     END DO

     ! Fall through indicates failure to converge                    
     CALL ERROUT( PRTFIL, 'W', 'KRAKENC', 'Too many meshes needed: check convergence' )        

     ! Solution complete: discard modes with phase velocity above CHIGH  

3000 omega = SQRT( omega2 ) 
     Min_LOC = MinLOC( DBLE( EXTRAP( 1, 1:M ) ), DBLE( EXTRAP( 1, 1:M ) ) > omega2 / CHIGH ** 2 )
     M = Min_LOC( 1 )

     ! *** Write eigenvalues to PRTFIL and MODFIL ***                    

     WRITE( PRTFIL, * )
     WRITE( PRTFIL, * ) '   I          K             ALPHA          PHASE SPEED       GROUP SPEED'

     CK( 1:M ) = SQRT( EXTRAP( 1, 1:M ) + CK( 1:M ) )
     DO mode = 1, M 

        WRITE( PRTFIL, '( I5, 4G18.10 )' ) mode, CK( mode ), omega / DBLE( CK( mode ) ), VG( mode )

        ! Zero out positive imaginary part which would cause growth in range. Should be small.
        IF ( AIMAG( CK( mode ) ) > 0.0 ) CK( mode ) = REAL( CK( mode ) )
     END DO

     WRITE( MODFIL, REC = 5 ) M, LRECL 

     IFIRST = 1 
     DO IREC = 1, 1 + ( 2 * M - 1 ) / LRECL 

        ILAST  = MIN( M, IFIRST + LRECL / 2 - 1 ) 
        WRITE( MODFIL, REC = 6 + M + IREC ) ( CMPLX( CK( mode ) ), mode = IFIRST, ILAST )                            
        IFIRST = ILAST + 1 
     END DO

     CLOSE( MODFIL )

  END DO   ! next profile
  CLOSE( ENVFIL )

  STOP 
END PROGRAM KRAKENC
!**********************************************************************C
SUBROUTINE INIT 

  ! Initializes arrays defining difference equations

  USE krakcmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  LOGICAL :: ELFLAG = .FALSE.
  COMPLEX (KIND=8) :: CP2, CS2
  COMPLEX (KIND=8), ALLOCATABLE :: CP( : ), CS( : )
  CHARACTER TASK*8

  CMin     = 1.0E6 
  NFACT    = 0
  LOC( 1 ) = 0 

  ! Allocate storage for finite-difference coefficients

  NPTS = SUM( N( 1:NMedia ) ) + NMedia

  IF ( ALLOCATED( B1 ) ) DEALLOCATE( B1, B2, B3, B4, Rho )
  ALLOCATE ( B1( NPTS ), B2( NPTS ), B3( NPTS ), B4( NPTS ), Rho( NPTS ), &
       CP( NPTS ), CS( NPTS ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) &
       CALL ERROUT( PRTFIL, 'F', 'KRAKENC - INIT', 'Insufficient memory to allocate B1, B2, B3, B4 vectors. Reduce mesh.' )

  DO Med = 1, NMedia   ! *** Loop over media ***

     IF ( Med /= 1 ) LOC( Med ) = LOC( Med-1 ) + N( Med-1 ) + 1 
     N1 = N(   Med ) + 1
     I  = LOC( Med ) + 1

     ! *** PROFIL reads in the data for a medium ***                  

     TASK = 'TAB'
     CALL PROFIL( Depth, CP( I ), CS( I ), Rho( I ), Med, N1, FREQ, TopOpt(1:1), TopOpt(3:4), TASK, ENVFIL, PRTFIL )

     ! *** Load diagonals of the finite-difference equations ***      

     IF ( CS( I ) == ( 0.0, 0.0 ) ) THEN 

        !   Case of an acoustic medium                          

        Mater( Med ) = 'ACOUSTIC' 
        IF ( NFACT == 0 ) NFACT = Med
        NLACT = Med

        CMin = MIN( MINVAL( DBLE( CP( I: I + N( Med ) ) ) ), CMin )

        B1( I:I + N( Med ) ) = -2.0 +  H( Med ) ** 2 * omega2 / CP( I:I + N( Med ) ) ** 2
     ELSE

        !   Case of an elastic medium                           

        IF ( Sigma( Med ) /= 0.0 )                                &
             CALL ERROUT( PRTFIL, 'F', 'KRAKENC', 'Rough elastic interfaces are not allowed' )

        Mater( Med ) = 'ELASTIC' 
        ELFLAG = .TRUE.
        TwoH   = 2.0 * H( Med ) 

        DO J = I, I + N( Med ) 
           CMin = MIN( DBLE( CS( J ) ), CMin )

           CP2 = CP( J ) ** 2 
           CS2 = CS( J ) ** 2 

           B1( J ) = TwoH / ( Rho( J ) * CS2 ) 
           B2( J ) = TwoH / ( Rho( J ) * CP2 )
           B3( J ) = 4.0 * TwoH * Rho(J) * CS2 * ( CP2 - CS2 ) / CP2
           B4( J ) = TwoH * ( CP2 - 2.0 * CS2 ) / CP2

           Rho( J ) = TwoH * omega2 * Rho( J ) 
        END DO

     ENDIF

  END DO   ! next Med

  ! *** Bottom properties ***                                         

  IF ( BotOpt(1:1) == 'A' ) THEN                      
     IF ( CSB /= ( 0.0, 0.0 ) ) THEN      ! Elastic  bottom
        ELFLAG = .TRUE. 
        CMin = MIN( CMin, DBLE( CSB ) )       
     ELSE                                 ! Acoustic bottom
        CMin = MIN( CMin, DBLE( CPB ) ) 
     ENDIF
  ENDIF

  ! *** Top properties ***                                            

  IF ( TopOpt(2:2) == 'A' ) THEN                          
     IF ( CST /= ( 0.0, 0.0 ) ) THEN    ! Elastic  top
        ELFLAG = .TRUE. 
        CMin = MIN( CMin, DBLE( CST ) )
     ELSE                               ! Acoustic top
        CMin = MIN( CMin, DBLE( CPT ) ) 
     ENDIF
  ENDIF

  IF ( ELFLAG ) CMin = 0.85 * CMin 
  CLOW = MAX( CLOW, 0.99 * CMin )

  RETURN 
END SUBROUTINE INIT
!**********************************************************************C
SUBROUTINE SOLVE( ERROR ) 

  ! Solves the eigenvalue problem at the current mesh                 
  ! and produces a new extrapolation

  USE krakcmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  COMPLEX (KIND=8) :: T1, T2, F1, F2

  CALL CPU_TIME( Tstart )
  CALL INIT     ! set up the finite difference mesh                      
  CALL SOLVE2   ! solve for the eigenvalues

  EXTRAP( ISET, 1:M ) = EVMat( ISET, 1:M ) 

  IF ( ISET == 1 ) CALL VECTOR   ! If this is the first mesh, compute the eigenvectors

  ! *** Richardson extrapolation to improve the accuracy ***          

  ERROR = 10
  KEY = 2 * M / 3 + 1     ! index of element used to check convergence

  IF ( ISET > 1 ) THEN 
     T1 = EXTRAP( 1, KEY ) 

     DO J = ISET - 1, 1, -1 
        DO mode = 1, M 
           X1 = NV( J    ) ** 2 
           X2 = NV( ISET ) ** 2 
           F1 = EXTRAP( J,     mode ) 
           F2 = EXTRAP( J + 1, mode ) 
           EXTRAP( J, mode ) = F2 - ( F1 - F2 ) / ( X2 / X1 - 1.0 ) 
        END DO
     END DO

     T2 = EXTRAP( 1, KEY ) 
     ERROR = ABS( T2 - T1 ) 
  ENDIF

  CALL CPU_TIME( Tend)   ! check elapsed time
  ET( ISET ) = Tend - Tstart
  WRITE( PRTFIL, '( 1X, I8, 6X, G15.3 )' ) NV( ISET ), ET( ISET )

  RETURN 
END SUBROUTINE SOLVE
!**********************************************************************C
SUBROUTINE SOLVE2 

  ! Provides initial guess to root finder for each EVMat(I)           

  ! A sequence of restarts is done to compute a complete set of modes 
  ! for the first two sets (ISET=1,2).                                
  ! 'Complete' in this case means that all the modes in the           
  ! user-specified interval are found. However, if, for ISET=2,       
  ! the same number of modes is found as for ISET=1 then it is assumed
  ! that the set is complete and no restarts are performed

  USE krakcmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL (KIND=4) :: RVAR1, RVAR2
  COMPLEX    CKIG( MaxM ) 
  COMPLEX (KIND=8) :: X, P( 10 ), XTEMP( MaxM ), CTRY
  CHARACTER ERRMSG*80, FILNAMT*20

  X = ( 1.0, 0.0 ) * omega2 / CLOW ** 2 

  MaxIT = 5000   ! maximum # of iterations in root-finder 

  IF ( TopOpt(5:5) == '.' .AND. ISET <= 2 ) THEN
     MaxTries = MaxM / 5   ! MaxTries = # of restarts for the root finder
  ELSE
     MaxTries = 1 
  ENDIF
  WRITE( PrtFil, * ) 'Max. number of restarts in root finder, MaxTries = ', MaxTries

  ITRY = 1   ! counter that keeps track of # of tries

  ! optionally read an existing mode file to get initial guesses  

  IF ( ISET == 1 .AND. TopOpt(4:4) == 'I' ) THEN 
     OPEN ( FILE = 'MODFIL', UNIT = MODFIL, STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED' )

     READ( MODFIL, REC = 5 ) M, LRECL 

     IFIRST = 1 
     DO IREC = 1, 1 + ( 2 * M - 1 ) / LRECL 

        ILAST  = MIN( M, IFIRST + LRECL / 2 - 1 ) 
        READ( MODFIL, REC = 6 + M + IREC ) ( CKIG( mode ), mode = IFIRST, ILAST )
        IFIRST = ILAST + 1 
     END DO
  END IF

  ! *** start looking for each root in succession ***                 

  DO mode = 1, M 

     ! For first or second meshes, use a high guess                   
     ! Otherwise use extrapolation to produce an initial guess        

5500 CONTINUE 
     X = 1.00001 * X 

     IF ( ISET == 1 .AND. TopOpt(4:4) == 'I' ) THEN 
        X = CKIG( mode ) ** 2   ! initial guess from MODFIL
     ELSE IF ( ISET >= 2 ) THEN
        P( 1:ISET - 1 ) = EVMat( 1:ISET - 1, mode )

        IF ( ISET >= 3 ) THEN 
           DO I = 1, ISET - 2 
              DO J = 1, ISET - I - 1 
                 X1 = HV( J     ) ** 2 
                 X2 = HV( J + I ) ** 2 
                 P( J ) = ( ( HV( ISET ) ** 2 - X2 ) * P( J     ) - &
                      ( HV( ISET ) ** 2 - X1 ) * P( J + 1 ) ) / (  X1 - X2)                   
              END DO
           END DO
           X = P( 1 ) 
        ENDIF
     ENDIF

     TOL = ABS( X ) * 10.0 ** ( 5.0 - PRECISION( X ) )
     CALL ZSECCX( X, TOL, IT, MaxIT, ERRMSG )   ! Use the secant method to refine the eigenvalue ***

     IF ( ERRMSG /= ' ' ) THEN   ! any problems in root finder?
        WRITE( PRTFIL, * ) 'ISET = ', ISET, '   mode = ', mode 
        CALL ERROUT( PRTFIL, 'W', 'KRAKENC-ZSECCX', ERRMSG )
        X = TINY( REAL( X ) )   ! make sure value discarded later 
     ENDIF

     ! *** Is the mode inside the user specified spectral limits? ***

     IF ( SQRT( omega2 ) / REAL( SQRT( X ) ) > CHIGH ) THEN 
        ! Mode outside, restart at a random point
        CALL RANDOM_NUMBER( RVAR1 )
        CALL RANDOM_NUMBER( RVAR2 )

        CTRY = CLOW + RVAR1 * ( CHIGH - CLOW ) + ( 0.0, 0.01 ) * RVAR2 * CLOW                                      
        X = omega2 / CTRY ** 2 
        MaxIT = 30 ! but don't let root-finder search long

        IF ( ITRY < MaxTries ) THEN 
           MaxTries = MAX( MaxTries, 5 * mode ) 
           ITRY = ITRY + 1 
           WRITE( *, * ) 'Restart at phase speed = ', CTRY 
           GOTO 5500 
        ELSE 
           M = mode - 1
           EXIT   ! done searching for modes 
        ENDIF
     ENDIF

     EVMat( ISET, mode ) = X 
     XTEMP( mode )       = X

  END DO   ! next mode 

  ! If no modes, open a dummy MODFIL for use by FIELD3D

  IF ( M == 0 ) THEN 
     LRECL = 32 
     NZTab = 0 

     ! Open MODFIL and write header

     WRITE( FILNAMT, FMT="( 'MODFIL', I4.4 )" ) IPROF
     OPEN ( FILE = FILNAMT, UNIT = MODFIL, ACCESS = 'DIRECT', RECL = 4 * LRECL, FORM = 'UNFORMATTED' )
     WRITE( MODFIL, REC = 1 ) LRECL, TITLE, REAL( FREQ ), 1, NZTab, NZTab         
     WRITE( MODFIL, REC = 5 ) M, LRECL

     CALL ERROUT( PRTFIL, 'F', 'KRAKENC', 'No modes for given phase speed interval' )       

  ENDIF

  IF ( M == MaxM ) THEN   ! Have we hit the ceiling on max # modes
     WRITE( PRTFIL, * ) 'Number of modes = ', M
     WRITE( PRTFIL, * ) 'Number of modes allowable = ', MaxM 
     CALL ERROUT( PRTFIL, 'W', 'KRAKENC', 'Too many modes: Program will compute as many as it can' )
  ENDIF

  CALL ORDER( XTEMP, M )    ! order eigenvalues by real part
  EVMat( ISET, 1:M ) = XTEMP( 1:M )

  RETURN 
END SUBROUTINE SOLVE2
!**********************************************************************C
SUBROUTINE FUNCT( X, Delta, IPow ) 

  ! FUNCT( X ) = 0 is the dispersion relation

  USE krakcmod 

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  PARAMETER ( ROOF = 1.0E5, FLOOR = 1.0E-5, IPowR = 5, IPowF = -5 ) 
  COMPLEX (KIND=8) :: X, Delta, F, G, F1, G1
  CHARACTER BCType*1

  BCType(1:1) = BotOpt(1:1)
  CALL BCIMP( X, BCType, 'BOT', CPB, CSB, RhoB, F, G, IPow )  ! Bottom impedance

  CALL ACOUST( X, F,  G,  IPow  )  ! Shoot through acoustic layers

  BCType(1:1) = TopOpt(2:2) 
  CALL BCIMP( X, BCType, 'TOP', CPT, CST, RhoT, F1, G1, IPow1 )  ! Top impedance

  Delta = F * G1 - G * F1 
  IPow = IPow + IPow1

  ! *** Deflate previous roots ***                                    

  IF ( mode > 1 ) THEN 
     Delta = Delta / ( X - EVMat( ISET, mode-1 ) ) 

     IF ( mode > 2 ) THEN 
        DO J = 1, mode - 2 
           Delta = Delta / ( X - EVMat(ISET, J) ) 

           ! Scale if necessary
           DO WHILE ( ABS( DBLE( Delta ) ) < FLOOR .AND. ABS( Delta ) > 0.0 )               
              Delta = ROOF * Delta
              IPow  = IPow - IPowR
           END DO

           DO WHILE ( ABS( DBLE( Delta ) ) > ROOF )
              Delta = FLOOR * Delta
              IPow  = IPow - IPowF
           END DO

        END DO
     ENDIF
  ENDIF

  RETURN 
END SUBROUTINE FUNCT
!**********************************************************************C
SUBROUTINE ACOUST( X, F, G, IPow ) 

  ! Shoot through acoustic layers                                     

  USE krakcmod 

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  COMPLEX (KIND=8) :: X, F, G, P0, P1, P2, H2K2
  ! for 2-layer code at end: COMPLEX (KIND=8) :: gamma1, gamma2
  PARAMETER ( ROOF = 1.0E20, FLOOR = 1.0E-20, IPowF = -20 )

  IF ( NFACT == 0 ) RETURN 

  DO Med = NLACT, NFACT, -1  ! *** Loop over successive acoustic media ***

     H2K2 = H( Med ) ** 2 * X 
     I = LOC( Med ) + N( Med ) + 1
     RhoM = Rho( I )	! density at the bottom of the layer
     P1 = -2.0 * G
     P2 = ( B1( I ) - H2K2 ) * G - 2.0 * H( Med ) * F  * RhoM

     ! *** Shoot (towards surface) through a single medium ***

     DO I = LOC( Med ) + N( Med ), LOC( Med ) + 1, -1 

        P0 = P1 
        P1 = P2 
        P2 = ( H2K2 - B1( I ) ) * P1 - P0

        DO WHILE ( ABS(  DBLE( P2 ) ) > ROOF )  ! Scale if necessary
           P0 = FLOOR * P0
           P1 = FLOOR * P1
           P2 = FLOOR * P2
           IPow = IPow - IPowF
        END DO
     END DO

     ! F = P'/Rho and G = -P since FP+GP'/Rho = 0                 
     RhoM = Rho( LOC( Med ) + 1 )	! density at top of layer
     F = -( P2 - P0 ) / ( 2.0 * H( Med ) ) / RhoM
     G = -P1
  END DO

  ! here's a little two-layer analytic formula as a test
  !d1 = 5000
  !d2 = 10000
  !gamma1 = SQRT( ( 2 + b1(    1 ) ) / H( 1 ) ** 2 - x )
  !gamma2 = SQRT( ( 2 + b1( 6000 ) ) / H( 2 ) ** 2 - x )
  !rho2 = 1.8
  !f = 0
  !g = SIN( gamma1 * d1 ) * COS( gamma2 * (d1-d2) ) / gamma1 - COS( gamma1 * d1 ) * SIN( gamma2 * ( d1-d2 ) ) / gamma2
  !IPow = 0

  RETURN 
END SUBROUTINE ACOUST
!**********************************************************************C
SUBROUTINE VECTOR 

  ! Do inverse iteration to compute each of the eigenvectors          
  ! and write these to the disk file                                  

  USE krakcmod
  USE SdRdRMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  INTEGER, ALLOCATABLE :: IZTab( : )
  REAL ZTab( NSD + NRD )
  COMPLEX, ALLOCATABLE :: PhiTab( : )
  COMPLEX (KIND=8) :: XH2, X, F, G
  REAL, ALLOCATABLE :: Z( : ), WTS( : )
  COMPLEX (KIND=8), ALLOCATABLE :: RV1( : ), RV2( : ), RV3( : ), RV4( : ), Phi( : ), D( : ), E( : )
  CHARACTER BCTOP*1, BCBot*1, FILNAMT*20

  BCTop(1:1) = TopOpt(2:2) 
  BCBot(1:1) = BotOpt(1:1)

  ! *** Tabulate z-coordinates and off-diagonals of matrix ***        

  NTot = SUM( N( NFACT:NLACT ) )
  NTot1 = NTot + 1
  ALLOCATE( Z( NTot1 ), E( NTot1 + 1 ), D( NTot1 ), Phi( NTot1 ), &
       RV1( NTot1 ), RV2( NTot1 ), RV3( NTot1 ), RV4( NTot1 ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) &
       CALL ERROUT( PRTFIL, 'F', 'KRAKENC - VECTOR', 'Insufficient memory: Reduce mesh.' )

  J = 1 
  Z( 1 ) = Depth( NFACT ) 

  DO Med = NFACT, NLACT 
     HRho = H( Med ) * Rho( LOC( Med ) + 1 )        ! density at the top of each layer

     E( J+1 : J + N( Med ) ) = 1.0 / HRho
     Z( J+1 : J + N( Med ) ) = Z( J ) + H( Med ) * (/ (JJ, JJ = 1, N(Med) ) /)

     J = J + N( Med )

  END DO

  E( NTot1 + 1 ) = 1.0 / HRho       ! Dummy value; not used

  ! Calculate the indices, weights, ... for mode interpolation        

  CALL MERGEV( SD, NSD, RD, NRD, ZTab, NZTab )
  ALLOCATE( WTS( NZTab ), IZTab( NZTab ), PhiTab( NZTab ) )
  CALL WEIGHT( Z, NTot1, ZTab, NZTab, WTS, IZTab )

  ! Open MODFIL and write header

  LRECL = MAX( 2 * NZTab, 32, 3 * ( NLACT - NFACT + 1 ) )   ! Logical record length in `longwords' (4 bytes)
  WRITE( FILNAMT, FMT="( 'MODFIL', I4.4 )" ) IPROF
  OPEN ( FILE = FILNAMT, UNIT = MODFIL, ACCESS = 'DIRECT', RECL = 4 * LRECL, FORM = 'UNFORMATTED' )

  WRITE( MODFIL, REC = 1 ) LRECL, TITLE, REAL( FREQ ), NLACT - NFACT + 1, NZTab, NZTab                                
  WRITE( MODFIL, REC = 2 ) ( N( Med ), Mater( Med ), Med = NFACT, NLACT )
  WRITE( MODFIL, REC = 3 ) BCTop(1:1), CMPLX( CPT ), CMPLX( CST ), REAL( RhoT ), REAL( Depth( 1          ) ), &
       BCBot(1:1), CMPLX( CPB ), CMPLX( CSB ), REAL( RhoB ), REAL( Depth( NMedia + 1 ) )
  WRITE( MODFIL, REC = 4 ) ( REAL( Depth( Med ) ), REAL( Rho( LOC( Med ) + 1 ) ), Med = NFACT, NLACT )
  WRITE( MODFIL, REC = 6 ) ZTab( 1:NZTab )

  ! *** Main loop: for each eigenvalue call SINVIT to get eigenvector 

  DO mode = 1, M 
     X = EVMat( 1, mode ) 

     ! *** Corner elt requires top impedance ***                      

     CALL BCIMP( X, BCTop, 'TOP', CPT, CST, RhoT, F, G, IPow ) 

     IF ( G == 0.0 ) THEN 
        D( 1 ) = 1.0   ;   E( 2 ) = EPSILON( REAL( D( 1 ) ) )
     ELSE 
        L = LOC( NFACT ) + 1 
        XH2    = X * H( NFACT ) * H( NFACT ) 
        HRho   = H( NFACT ) * Rho( L ) 
        D( 1 ) = ( B1( L ) - XH2 ) / HRho / 2.0 + F / G
     ENDIF

     ! *** Set up the diagonal ***                                    

     ITP = NTot 
     J = 1
     L = LOC( NFACT ) + 1 

     DO Med = NFACT, NLACT 
        XH2  = X * H( Med ) ** 2
        HRho = H( Med ) * Rho( LOC( Med ) + 1 )

        IF ( Med >= NFACT + 1 ) THEN 
           L = L + 1
           D( J ) = ( D( J ) + ( B1( L ) - XH2 ) / HRho ) / 2.0 
        ENDIF

        DO I = 1, N( Med ) 
           J = J + 1
           L = L + 1 
           D( J ) = ( B1( L ) - XH2 ) / HRho 

           IF ( REAL( B1( L ) - XH2 ) + 2.0 > 0.0 ) THEN   ! Locate index of turning point nearest top
              ITP = MIN( J, ITP ) 
           ENDIF
        END DO
     END DO

     ! *** Corner elt requires bottom impedance ***                   

     CALL BCIMP( X, BCBot, 'BOT', CPB, CSB, RhoB, F, G, IPow ) 

     IF ( G == 0.0 ) THEN 
        D( NTot1 ) = 1.0   ;   E( NTot1 ) = EPSILON( REAL( D( NTot1 ) ) )
     ELSE
        D( NTot1 ) =  D( NTot1 ) / 2.0 - F / G 
     ENDIF

     CALL SINVIT( NTot1, D, E, IERR, RV1, RV2, RV3, RV4, Phi ) ! inverse iteration to compute eigenvector ***

     IF ( IERR /= 0 ) THEN 
        WRITE( PRTFIL, * ) 'mode = ', mode 
        CALL ERROUT( PRTFIL, 'W', 'KRAKENC-SINVIT', 'Inverse iteration failed to converge' )       
     ENDIF

     CALL NORMIZ( Phi, ITP, NTot1, X )   ! Normalize the eigenvector

     ! Tabulate the modes at the source depths and write to disk

     PhiTab = Phi( IZTab ) + WTS * ( Phi( IZTab + 1 ) - Phi( IZTab ) )
     WRITE( MODFIL, REC = 6 + mode ) PhiTab

  END DO

  DEALLOCATE( Z, E, D, Phi, RV1, RV2, RV3, RV4 )
  DEALLOCATE( WTS, IZTab, PhiTab )      

  RETURN 
END SUBROUTINE VECTOR
!**********************************************************************C
SUBROUTINE NORMIZ( Phi, ITP, NTot1, X ) 

  ! Normalize the eigenvector:                                        
  ! SqNrm = Integral(Phi ** 2) by the trapezoidal rule:            
  ! Integral(F) = H*( F(1)+...+F(N-1) + 0.5*(F(0)+F(N)) )

  ! Compute perturbation due to material absorption                   
  ! Call SCAT to figure interfacial scatter loss                      

  USE krakcmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  COMPLEX (KIND=8) :: Phi( NTot1 ), X, X1, F1, G1, X2, F2, G2, DRhoDX, DEtaDX, SqNrm, RN, ScaleF, Slow
  CHARACTER BCType*1

  SqNrm = 0.0
  Slow  = 0.0

  ! *** Compute contribution to slowness from top half-space ***

  IF ( TopOpt(2:2) == 'A' ) THEN
     Slow  = Slow + Phi( 1 ) ** 2 / ( 2 * SQRT( X - omega2 / CPT ** 2 ) ) / ( RhoT * CPT ** 2 )
  ENDIF

  ! *** Loop to compute norm of eigenvector ***

  L = LOC( NFACT )
  J = 1 

  DO Med = NFACT, NLACT 
     L = L + 1
     RhoM = Rho( L )
     RhoOMH2 = RhoM * omega2 * H( Med ) ** 2

     ! top interface
     SqNrm = SqNrm + 0.5 * H( Med ) * Phi( J ) ** 2 / RhoM
     Slow  = Slow  + 0.5 * H( Med ) * ( B1(L) + 2. ) * Phi( J ) ** 2 / RhoOMH2

     ! medium
     L1 = L + 1   ;   L = L + N( Med ) - 1
     J1 = J + 1   ;   J = J + N( Med ) - 1
     SqNrm = SqNrm + H( Med ) * SUM(                     Phi( J1:J ) ** 2 ) / RhoM
     Slow  = Slow  + H( Med ) * SUM( ( B1(L1:L) + 2. ) * Phi( J1:J ) ** 2 ) / RhoOMH2

     ! bottom interface
     L = L + 1   ;   J = J + 1
     SqNrm = SqNrm + 0.5 * H( Med ) * Phi( J ) ** 2 / RhoM
     Slow  = Slow  + 0.5 * H( Med ) * ( B1(L) + 2. ) * Phi( J ) ** 2 / RhoOMH2

  END DO

  ! *** Compute contribution to slowness from bottom half-space ***

  IF ( BotOpt(1:1) == 'A' ) THEN
     Slow  = Slow + Phi( J ) ** 2 / ( 2 * SQRT( X - omega2 / CPB ** 2 ) ) / ( RhoB * CPB ** 2 )
  ENDIF

  ! *** Compute derivative of top admitance ***                       

  X1 = 0.9999999 * X 
  X2 = 1.0000001 * X 

  BCType(1:1) = TopOpt(2:2) 

  CALL BCIMP( X1, BCType, 'TOP', CPT, CST, RhoT, F1, G1, IPow ) 
  CALL BCIMP( X2, BCType, 'TOP', CPT, CST, RhoT, F2, G2, IPow )

  DRhoDX = 0.0 
  IF ( G1 /= 0.0 ) DRhoDX = -( F2 / G2 - F1 / G1 ) / ( X2 - X1 )

  ! *** Compute derivative of bottom admitance ***                    

  BCType(1:1) = BotOpt(1:1) 

  CALL BCIMP( X1, BCType, 'BOT', CPB, CSB, RhoB, F1, G1, IPow ) 
  CALL BCIMP( X2, BCType, 'BOT', CPB, CSB, RhoB, F2, G2, IPow )

  DEtaDX = 0.0 
  IF ( G1 /= 0.0 ) DEtaDX = -( F2 / G2 - F1 / G1 ) / ( X2 - X1 )

  ! *** Scale the mode ***                                            

  RN = SqNrm + DRhoDX * Phi( 1 ) ** 2 - DEtaDX * Phi( NTot1 ) ** 2

  ScaleF = 1.0 / SQRT( RN )
  IF ( REAL( Phi( ITP ) ) < 0.0 ) ScaleF = -ScaleF  ! make sign consistent at mode turning point

  Phi = ScaleF * Phi

  Slow  = ScaleF ** 2 * Slow * SQRT( omega2 / X )
  VG( mode )   = 1 / Slow

  ! *** Compute interfacial scatter loss ***                          

  CALL SCAT( Phi, X ) 

  RETURN 
END SUBROUTINE NORMIZ
!**********************************************************************C
SUBROUTINE SCAT( Phi, X ) 

  ! Figure scatter loss                                               

  USE krakcmod 

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  COMPLEX (KIND=8) :: Phi( * ), X, PERK, Eta1SQ, Eta2SQ, KUPING, U, PEKRT
  CHARACTER BCType*1

  PERK = 0.0 
  J = 1 
  L = LOC( NFACT ) 

  DO Med = NFACT - 1, NLACT   ! *** Loop over media ***

     ! *** Calculate Rho1, Eta1SQ, Phi, U ***                         

     IF ( Med == NFACT - 1 ) THEN 
        ! *** Top properties ***

        BCType(1:1) = TopOpt(2:2) 

        SELECT CASE ( BCType(1:1) ) 

        CASE ( 'A' )       ! Acousto-elastic
           Rho1   = RhoT 
           Eta1SQ = X - omega2 / CPT ** 2
           U      = PEKRT( Eta1SQ ) * Phi( 1 ) / RhoT
        CASE ( 'V' )       ! Vacuum
           Rho1   = 1.0E-9 
           Eta1SQ = 1.0
           RhoINS = Rho( LOC( NFACT ) + 1 )
           U      = Phi( 2 ) / H( NFACT ) / RhoINS
        CASE ( 'R' )       ! Rigid
           Rho1   = 1.0E+9 
           Eta1SQ = 1.0
           U      = 0.0
        CASE DEFAULT       ! Tabulated
           Rho1   = 0.0 
           Eta1SQ = 0.0
           U      = 0.0
        END SELECT
     ELSE 
        H2 = H( Med ) ** 2 
        J  = J + N( Med )
        L  = LOC( Med ) + N( Med ) + 1

        Rho1   = Rho( L ) 
        Eta1SQ = ( 2.0 + B1( L ) ) / H2 - X
        U      = ( -Phi( J-1 ) - 0.5 * ( B1(L)-H2*X ) * Phi( J ) ) / ( H( Med ) * Rho1 )
     ENDIF

     ! *** Calculate Rho2, Eta2 ***                                   

     IF ( Med == NLACT ) THEN 
        ! *** Bottom properties ***

        BCType(1:1) = BotOpt(1:1) 

        SELECT CASE ( BCType(1:1) ) 

        CASE ( 'A' )        ! Acousto-elastic 
           Rho2   = RhoB 
           Eta2SQ = omega2 / CPB ** 2 - X
        CASE ( 'V' )        ! Vacuum
           Rho2   = 1.0E-9 
           Eta2SQ = 1.0
        CASE ( 'R' )        ! Rigid
           Rho2   = 1.0E+9 
           Eta2SQ = 1.0
        CASE DEFAULT        ! Tabulated
           Rho2   = 0.0 
           Eta2SQ = 0.0
        END SELECT
     ELSE 
        Rho2   = Rho( L + 1 ) 
        Eta2SQ = ( 2.0 + B1( L + 1 ) ) / H( Med + 1 ) ** 2 - X
     ENDIF

     PERK = PERK + KUPING( Sigma( Med + 1 ), Eta1SQ, Rho1, Eta2SQ, Rho2, Phi( J ), U )              

  END DO

  CK( mode ) = PERK 

  RETURN 
END SUBROUTINE SCAT
!**********************************************************************C
SUBROUTINE ORDER( X, N ) 

  ! Does an insertion sort of the complex vector X in order of decreasing real part                                           

  ! At the Ith step, the first I-1 positions contain a sorted vector.
  ! We shall insert the Ith value into its place in that
  ! vector, shifting up to produce a new vector of length I.           

  COMPLEX (KIND=8) X( * ), T

  IF ( N == 1 ) RETURN 

  DO I = 2, N 

     T = X( I ) 

     IF ( REAL( T ) > REAL( X( 1 ) ) ) THEN
        X( 2:I ) = X( 1:I-1 )
        X( 1 )   = T  ! goes in the first position
     ELSE IF ( REAL( T ) > REAL( X( I - 1 ) ) ) THEN

        ! *** Binary search for its place ***                         

        IRIGHT = I - 1 
        ILEFT  = 1 

        DO WHILE ( IRIGHT > ILEFT + 1 )
           IMID = ( ILEFT + IRIGHT ) / 2 

           IF ( REAL( T ) > REAL( X( IMID ) ) ) THEN 
              IRIGHT = IMID 
           ELSE 
              ILEFT  = IMID 
           ENDIF
        END DO

        X( IRight + 1: I ) = X( IRight : I - 1 )
        X( IRight ) = T

     ENDIF

  END DO

  RETURN 
END SUBROUTINE ORDER
