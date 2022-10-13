!     Last change:  MBP   3 Mar 2001    7:42 pm
COMPLEX FUNCTION PADE( X0, X, F, N, ErrorMessage )

  !     Computes the [MU, NU] Pade approximant at X0, where
  !        MU = [M/2], NU = M - MU and M = N - 1
  !
  !     FORMULAS TAKEN FROM BURLISCH AND STOER
  !        NUMERISCHE MATHEMATIK 8:1-13(1966)
  !
  !     N IS THE NUMBER OF POINTS USED.
  !     X CONTAINS THE INDEPENDENT VARIABLE
  !     F CONTAINS THE FUNCTIONAL VALUES AT EACH X
  !     X0 IS THE POINT AT WHICH THE APPROXIMATION IS TO BE CALCULATED
  !
  !     Note much wasted space in this implementation ...
  !
  !     Michael Porter, 11 September 1985

  INTEGER II( 1 )
  COMPLEX X0, X( N ), F( N ), RDT, WDen, CNum, CDen, D( N, N ), C( N, N ), H( N )
  CHARACTER*80 ErrorMessage

  ErrorMessage = '      '

  ! Initialize arrays

  Nearest = 1
  H       = X - X0

  ! Quick return if x lies "on" a data point

  II = MINLOC( ABS( H ) )
  Nearest = II( 1 )
  IF ( ABS( H( Nearest ) ) < 100 * SPACING( REAL( X0 ) ) ) THEN
     PADE = F( Nearest )
     RETURN
  ENDIF

  ! Recursion for solution

  DO M = 1, N
     D( 1, M ) = F( M ) + EPSILON( REAL( X0 ) )  ! perturbation to avoid 0/0 condition
     C( 1, M ) = F( M )

     IF ( M >= 2 ) THEN

        DO K = 1, M - 1
           RDT  = H( M - K ) / H( M ) * D( K, M - K )
           CNum = C( K, M - K + 1 ) - D( K, M - K )
           CDen = RDT - C( K, M - K + 1 )

           IF ( ABS( CNum ) > 1.0E10 * ABS( CDen ) ) THEN
              ErrorMessage = 'ERROR: Nearly-singular matrix in PADE'
              PADE = F( Nearest )
              RETURN
           ENDIF

           WDen = CNum / CDen
           D( K + 1, M - K ) = C( K, M - K + 1 ) * WDen
           C( K + 1, M - K ) = RDT * WDen
        END DO
     ENDIF
  END DO

  ! Sum diagonal elements to get interpolate

  PADE = 0.0

  DO K = 1, N
     PADE = PADE + D( K, N - K + 1 )
  END DO

  ! WRITE( *, * ) REAL( X0 ), REAL( X ), REAL( F ), pade

  RETURN
END FUNCTION PADE
