SUBROUTINE EVAL( C, phi, Nz, R, Nr, rr, ck, M, Option, P )

  ! Given modes and wavenumbers, compute pressure field
  ! Normalized to pressure of point source at 1 meter
  ! Option = X     Cartesian   (x, z) coordinates
  ! Option = R     Cylindrical (r, z) coordinates

  COMPLEX   CI
  PARAMETER ( MaxM = 10000, CI = (0.0, 1.0), pi = 3.1415926, MinExp = -100 )
  REAL      rr( * ), r( * )
  COMPLEX   C( * ), phi( MaxM, * ), P( Nz, Nr ), T, Hank( M ), CIck( M ), const( M ), Cmat( M, Nz ), ck( * ), factor
  CHARACTER Option*(*)

  ! *** If no modes, return vanishing pressure ***

  IF ( M <= 0 ) THEN
     P = 0.0
     RETURN
  END IF

  ! Initialization

  factor = CI * SQRT( 2.0 * pi ) * EXP( CI * pi / 4.0 )

  IF ( Option(1:1) == 'X' ) THEN
     const( 1:M ) = factor * C( 1:M ) / ck( 1:M )
  ELSE
     const( 1:M ) = factor * C( 1:M ) / SQRT( ck( 1:M ) )
  ENDIF

  CIck( 1:M ) = -CI * ck( 1:M )   ! use e{i(wt-kr)} form

  IF ( Option(4:4) == 'I' ) CIck = REAL( CIck )   ! Incoherent case

  DO iz = 1, Nz
     Cmat( :, iz ) = const( : ) * phi( 1:M, iz ) * EXP( CIck( : ) * rr( iz ) )
  END DO

  !  *** Loop over range ***

  DO ir = 1, Nr
     ! eliminate underflows (can raise CPU time)
     !WHERE (  REAL( CIck * r( ir ) ) > MinExp )
     Hank = EXP( CIck * r( ir ) )
     !ELSEWHERE
     !   Hank = 0.0
     !END WHERE

     ! *** Loop over depth ***

     DO iz = 1, Nz
        IF ( Option(4:4) /= 'I' )  THEN         ! coherent case
           T =       SUM(    Cmat( :, iz ) * Hank( : ) )
        ELSE                                    ! incoherent case
           T = SQRT( SUM(  ( Cmat( :, iz ) * Hank( : ) ) ** 2 ) )
        ENDIF

        ! Cylindrical or cartesian coordinates?
        IF ( Option(1:1) == 'R' .AND. ABS( r( ir ) + rr( iz ) ) > TINY( R( 1 ) ) ) T = T / SQRT( r( ir ) + rr( iz ) )
        P( iz, ir ) = T
     END DO

  END DO   ! next range step

  RETURN
END SUBROUTINE EVAL
