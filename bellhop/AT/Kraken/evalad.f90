SUBROUTINE EVALAD( rProf, NProf, phiS, phiR, rd, Nrd, R, Nr, M, Opt, P )

  ! Computes pressure field using adiabatic mode theory
  ! Normalized to pressure at 1 meter

  ! Opt:
  !   X     Cartesian   (x, z) coordinates
  !   T     Translationally invariant ocean
  !   R     Cylindrical (r, z) coordinates
  !   S     Scaled cylindrical coordinates ( 1/r fall-off removed )

  COMPLEX CI
  PARAMETER ( MaxM = 10000, CI = ( 0.0, 1.0 ), pi = 3.1415926 )
  REAL rd( * ), rProf( * ), r( * )
  COMPLEX phiS( * ), phiL( MaxM, Nrd ), phiR( MaxM, * ), const( MaxM ), Hank( MaxM ), &
       SUM, ckInt( MaxM ), phiINT( MaxM ), P( Nrd, * ), ckL( MaxM ), ckR( MaxM ), ckMid( MaxM )
  COMPLEX (KIND=8) :: sumk( MaxM ), sumkinv( MaxM )
  CHARACTER Opt*( * ), Title*80

  ! *** Initialization ***

  rProf( NProf + 1 ) = 1.0E9
  iProf = 1

  ! Receiver depths at left  of segment
  CALL GETMOD( iProf  , ' ', MaxM, rd, Nrd, 'N', ckL, phiL, M1, freq, Title )
  M = MIN( M, M1 )

  ! Receiver depths at right of segment
  CALL GETMOD( iProf+1, ' ', MaxM, rd, Nrd, 'N', ckR, phiR, M1, freq, Title )
  M = MIN( M, M1 )

  const(   1:M ) = CI * SQRT( 2.0 * pi ) * EXP( CI * pi / 4.0 ) * phiS( 1:M )
  sumk(    1:M ) = 0.0
  sumkinv( 1:M ) = 0.0
  IF ( Opt(1:1) == 'T' ) const( 1:M ) = const( 1:M ) / SQRT( ckL( 1:M ) )

  ! *** March forward in range ***

  DO ir = 1, Nr

     ! Crossing into new range segment?

     DO WHILE ( r( ir ) > 1000.0 * rProf( iProf + 1 ) )
        CALL NEWSEG( R, ir, rProf, iProf, NProf, ckL, ckR, phiL, phiR, M, MaxM, rd, Nrd, sumk, sumkinv )
     END DO

     ! Compute proportional distance, W, and interpolate

     IF ( ir > 1 ) THEN
        rLeft = MAX( r( ir - 1 ), 1000.0 * rProf( iProf ) )
     ELSE
        rLeft = 1000.0 * rProf( iProf )
     ENDIF

     RMid = 0.5 * ( r( ir ) + rLeft )
     W    = ( r( ir ) / 1000.0 - rProf( iProf ) ) / ( rProf( iProf + 1 ) - rProf( iProf ) )
     wMid = ( RMid    / 1000.0 - rProf( iProf ) ) / ( rProf( iProf + 1 ) - rProf( iProf ) )

     ckInt(   1:M ) = ckL(     1:M ) + W    * ( ckR( 1:M ) - ckL( 1:M ) )
     ckMid(   1:M ) = ckL(     1:M ) + wMid * ( ckR( 1:M ) - ckL( 1:M ) )
     sumk(    1:M ) = sumk(    1:M ) + ckMid(1:M) * ( r( ir ) - rLeft )
     sumkinv( 1:M ) = sumkinv( 1:M ) + ( r( ir ) - rLeft ) / ckMid( 1:M )

     IF ( Opt(4:4) /= 'I' ) THEN   ! coherent   case
        Hank( 1:M ) = const( 1:M ) *       EXP( -CI * sumk( 1:M ) )
     ELSE                          ! incoherent case
        Hank( 1:M ) = const( 1:M ) * EXP( REAL( -CI * sumk( 1:M ) ) )
     ENDIF

     SELECT CASE( Opt(1:1) )
     CASE ( 'R' )                             ! Cylindrical coords.
        IF ( r( ir ) == 0.0 ) THEN
           Hank( 1:M ) = 0.0
        ELSE
           Hank( 1:M ) = Hank( 1:M ) / SQRT( ckInt(1:M) * r( ir ) )
        ENDIF
     CASE ( 'X' )                             ! Cartesian coords.
        Hank( 1:M ) = Hank( 1:M ) / ckInt(1:M)
     CASE ( 'T' )                             ! Translationally invariant
        Hank( 1:M ) = Hank( 1:M ) / SQRT( ckInt(1:M) * sumkinv( 1:M ) )
     CASE DEFAULT                             ! Scaled cylindrical coords.
        Hank( 1:M ) = Hank( 1:M ) / SQRT( ckInt(1:M) )
     END SELECT

     ! *** For each rcvr, add up modal contributions ***

     DO ird = 1, Nrd
        phiINT( 1:M ) = phiL( 1:M, ird ) + W * ( phiR( 1:M, ird ) - phiL( 1:M, ird ) )
        IF ( Opt(4:4) /= 'I' )  THEN   ! coherent   case
           P( ird, ir ) =       SUM(       phiINT( 1:M ) * Hank( 1:M ) )
        ELSE                           ! incoherent case
           P( ird, ir ) = SQRT( SUM( ABS(  phiINT( 1:M ) * Hank( 1:M ) ) ** 2 ) )
        ENDIF

     END DO

  END DO   ! Next range step

  RETURN
END SUBROUTINE EVALAD

!**********************************************************************!

SUBROUTINE NEWSEG( r, ir, rProf, iProf, NProf, ckL, ckR, phiL, phiR, M, MaxM, rd, Nrd, sumk, sumkinv )

  ! Treats the crossing into a new range segment

  REAL rd( * ), rProf( * ), r( * )
  COMPLEX phiL( MaxM, * ), phiR( MaxM, * ), ckL( * ), ckR( * ), ckMid( M )
  COMPLEX (KIND=8) :: sumk( * ), sumkinv( * )
  CHARACTER Title*80

  ! *** Do phase integral up to the new range ***

  IF ( ir > 1 ) THEN
     rLeft = MAX( r( ir - 1 ), 1000.0 * rProf( iProf ) )
  ELSE
     rLeft = 1000.0 * rProf( iProf )
  ENDIF

  RMid = 0.5 * ( 1000.0 * rProf( iProf + 1 ) + rLeft )
  wMid = ( RMid / 1000.0 - rProf( iProf ) ) / ( rProf( iProf + 1 ) - rProf( iProf ) )

  ckMid(   1:M ) = ckL(     1:M ) + wMid  * ( ckR( 1:M ) - ckL( 1:M ) )
  sumk(    1:M ) = sumk(    1:M ) + ckMid( 1:M ) * ( 1000.0 * rProf( iProf + 1 ) - rLeft )
  sumkinv( 1:M ) = sumkinv( 1:M ) + ( 1000.0 * rProf( iProf + 1 ) - rLeft ) / ckMid( 1:M )

  ! ***  Copy right modes to left ***

  ckL(  1:M )        = ckR(  1:M )
  phiL( 1:M, 1:Nrd ) = phiR( 1:M, 1:Nrd )

  ! *** Read in the new right mode set ***

  iProf = iProf + 1

  IF ( iProf + 1  <= NProf ) THEN
     CALL GETMOD( iProf+1, ' ', MaxM, rd, Nrd, 'N', ckR, phiR, M1, freq, Title )
     M = MIN( M, M1 )
     WRITE( *, * ) 'New profile read', r(ir), iProf+1, rProf(iProf+1), ' #modes=', M
  ENDIF

  RETURN
END SUBROUTINE NEWSEG
