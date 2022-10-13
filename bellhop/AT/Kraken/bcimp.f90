SUBROUTINE BCIMP( X, BCTYPE, BOTTOP, CPHS, CSHS, RHOHS, F, G, IPOW )

  ! Compute Boundary Condition IMPedance

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  INTEGER, INTENT( OUT ) :: IPOW
  REAL (KIND=8) YV( 5 )
  REAL (KIND=8), INTENT( OUT ) :: F, G
  COMPLEX (KIND=8) CPHS, CSHS
  CHARACTER BCTYPE*1, BOTTOP*3

  IPOW = 0

  ! *** Vacuum or Twersky ***

  IF ( BCTYPE(1:1) == 'V' .OR. &
       BCTYPE(1:1) == 'S' .OR. BCTYPE(1:1) == 'H' .OR. &
       BCTYPE(1:1) == 'T' .OR. BCTYPE(1:1) == 'I' ) THEN
     F = 1.0   ;   G = 0.0
     YV( 1 ) = F ; YV( 2 ) = G; YV( 3 ) = 0.0; YV( 4 ) = 0.0; YV( 5 ) = 0.0
  ENDIF

  ! *** Rigid ***

  IF ( BCTYPE(1:1) == 'R' ) THEN
     F = 0.0   ;   G = 1.0
     YV( 1 ) = F ; YV( 2 ) = G; YV( 3 ) = 0.0; YV( 4 ) = 0.0; YV( 5 ) = 0.0
  ENDIF

  ! *** Acousto-elastic half-space ***

  IF ( BCTYPE(1:1) == 'A' ) THEN
     IF ( REAL( CSHS ) > 0.0 ) THEN
        GAMS2 = X - Omega2 / DBLE( CSHS ) ** 2
        GAMP2 = X - Omega2 / DBLE( CPHS ) ** 2

        GAMS = SQRT( GAMS2 )   ;   GAMP = SQRT( GAMP2 )
        RMU = RHOHS * DBLE( CSHS ) ** 2

        YV( 1 ) = ( GAMS*GAMP - X ) / RMU
        YV( 2 ) = ( ( GAMS2 + X ) ** 2 - 4.0*GAMS * GAMP * X ) * RMU
        YV( 3 ) = 2.0 * GAMS * GAMP - GAMS2 - X
        YV( 4 ) = GAMP * ( X - GAMS2 )
        YV( 5 ) = GAMS * ( GAMS2 - X )

        F = Omega2 * YV( 4 )
        G = YV( 2 )
        IF ( G > 0.0 ) MODECT = MODECT + 1
     ELSE
        GAMP = SQRT( X - DBLE( Omega2 / CPHS ** 2 ) )
        F = 1.0   ;   G = RHOHS / GAMP
     ENDIF
  ENDIF

  IF ( BOTTOP(1:3) == 'TOP' ) G = -G

  ! *** Shoot through elastic layers ***

  IF ( BOTTOP(1:3) == 'TOP' ) THEN

     ! Shoot down from top
     IF ( NFACT > 1 ) THEN

        DO MED = 1, NFACT - 1
           CALL ELASDN( X, YV, IPOW, MED )
        END DO

        F = Omega2 * YV( 4 )   ;   G = YV( 2 )
     ENDIF
  ELSE

     ! Shoot up from bottom
     IF ( NLACT < NMEDIA ) THEN

        DO MED = NMEDIA, NLACT + 1, -1
           CALL ELASUP( X, YV, IPOW, MED )
        END DO

        F = Omega2 * YV( 4 )   ;   G = YV( 2 )
     ENDIF
  ENDIF

  RETURN
END SUBROUTINE BCIMP
!**********************************************************************!
SUBROUTINE ELASUP( X, YV, IPOW, MED )

  ! Propagates through an elastic layer using compound matrix formulation

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL (KIND=8) :: XV( 5 ), YV( 5 ), ZV( 5 )
  PARAMETER ( ROOF = 1.0E5, FLOOR = 1.0E-5, IPOWR = 5, IPOWF = -5 )

  ! Euler's method for first step

  TWOX   = 2.0 * X
  TWOH   = 2.0 * H( MED )
  FOURHX = 4.0 * H( MED ) * X
  J = LOC( MED ) + N( MED ) + 1
  XB3 = X * B3( J ) - RHO( J )

  ZV(1) = YV(1) - 0.5*(   B1( J ) * YV( 4 ) - B2( J ) * YV( 5 ) )
  ZV(2) = YV(2) - 0.5*( -RHO( J ) * YV( 4 ) -     XB3 * YV( 5 ) )
  ZV(3) = YV(3) - 0.5*(      TWOH * YV( 4 ) + B4( J ) * YV( 5 ) )
  ZV(4) = YV(4) - 0.5*(   XB3*YV(1) + B2(J)*YV(2) -TWOX*B4(J)*YV(3))
  ZV(5) = YV(5) - 0.5*(RHO(J)*YV(1) - B1(J)*YV(2) -    FOURHX*YV(3))

  ! Modified midpoint method

  DO I = N( MED ), 1, -1
     J = J - 1

     XV = YV
     YV = ZV

     XB3 = X * B3( J ) - RHO( J )

     ZV(1) = XV(1) - (   B1( J ) * YV( 4 ) - B2( J ) * YV( 5 ) )
     ZV(2) = XV(2) - ( -RHO( J ) * YV( 4 ) -     XB3 * YV( 5 ) )
     ZV(3) = XV(3) - (      TWOH * YV( 4 ) + B4( J ) * YV( 5 ) )
     ZV(4) = XV(4) - (   XB3*YV(1) + B2(J)*YV(2) - TWOX*B4(J)*YV(3))
     ZV(5) = XV(5) - (RHO(J)*YV(1) - B1(J)*YV(2) -     FOURHX*YV(3))

     !         P1 = YV(2) * ( YV(4) - YV(5) ) 
     !         P0 = XV(2) * ( XV(4) - XV(5) ) 
     !         IF ( ( P0 > 0.0 ) .AND. (P1 < 0.0) ) MODECT = MODECT+1
     !         IF ( ( P0 < 0.0 ) .AND. (P1 > 0.0) ) MODECT = MODECT+1

     ! *** Scale if necessary ***

     IF ( I /= 1 ) THEN
        IF ( ABS( ZV( 2 ) ) < FLOOR ) THEN
           ZV = ROOF * ZV
           YV = ROOF * YV
           IPOW = IPOW - IPOWR
        ENDIF

        IF ( ABS( ZV( 2 ) ) > ROOF ) THEN
           ZV = FLOOR * ZV
           YV = FLOOR * YV
           IPOW = IPOW - IPOWF
        ENDIF
     ENDIF
  END DO

  YV = ( XV + 2.0 * YV + ZV ) / 4.0 ! Apply the standard filter at the terminal point

  RETURN
END SUBROUTINE ELASUP
!**********************************************************************!
SUBROUTINE ELASDN( X, YV, IPOW, MED )

  ! Propagates through an elastic layer using compound matrix formulation

  USE krakmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  REAL (KIND=8) :: XV( 5 ), YV( 5 ), ZV( 5 )
  PARAMETER ( ROOF = 1.0E5, FLOOR = 1.0E-5, IPOWR = 5, IPOWF = -5 )

  ! Euler's method for first step

  TWOX   = 2.0 * X
  TWOH   = 2.0 * H( MED )
  FOURHX = 4.0 * H( MED ) * X
  J = LOC( MED ) + 1
  XB3 = X * B3( J ) - RHO( J )

  ZV(1) = YV(1) + 0.5*(   B1( J ) * YV( 4 ) - B2( J ) * YV( 5 ) )
  ZV(2) = YV(2) + 0.5*( -RHO( J ) * YV( 4 ) -     XB3 * YV( 5 ) )
  ZV(3) = YV(3) + 0.5*(      TWOH * YV( 4 ) + B4( J ) * YV( 5 ) )
  ZV(4) = YV(4) + 0.5*(   XB3*YV(1) + B2(J)*YV(2) -TWOX*B4(J)*YV(3))
  ZV(5) = YV(5) + 0.5*(RHO(J)*YV(1) - B1(J)*YV(2) -    FOURHX*YV(3))

  ! Modified midpoint method

  DO I = 1, N( MED )
     J = J + 1

     XV = YV
     YV = ZV

     XB3 = X * B3( J ) - RHO( J )

     ZV(1) = XV(1) + (   B1( J ) * YV( 4 ) - B2( J ) * YV( 5 ) )
     ZV(2) = XV(2) + ( -RHO( J ) * YV( 4 ) -     XB3 * YV( 5 ) )
     ZV(3) = XV(3) + (      TWOH * YV( 4 ) + B4( J ) * YV( 5 ) )
     ZV(4) = XV(4) + (   XB3*YV(1) + B2(J)*YV(2) - TWOX*B4(J)*YV(3))
     ZV(5) = XV(5) + (RHO(J)*YV(1) - B1(J)*YV(2) -     FOURHX*YV(3))

     ! *** Scale if necessary ***

     IF ( I /= N( MED ) ) THEN
        IF ( ABS( ZV( 2 ) ) < FLOOR ) THEN
           ZV = ROOF * ZV
           YV = ROOF * YV
           IPOW = IPOW - IPOWR
        ENDIF

        IF ( ABS( ZV( 2 ) ) > ROOF ) THEN
           ZV = FLOOR * ZV
           YV = FLOOR * YV
           IPOW = IPOW - IPOWF
        ENDIF
     ENDIF
  END DO

  YV = ( XV + 2.0 * YV + ZV ) / 4.0 ! Apply the standard filter at the terminal point

  RETURN
END SUBROUTINE ELASDN
