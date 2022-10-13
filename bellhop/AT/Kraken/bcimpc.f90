SUBROUTINE BCIMP( X, BCType, BotTop, CPHS, CSHS, RhoHS, F, G, IPow )                                                   

  ! Compute Boundary Condition IMPedance

  USE krakcmod
  USE RefCoMod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  INTEGER, INTENT( OUT ) :: IPow
  COMPLEX (KIND=8) :: X, KX, KZ, TWERSK, GamS2, GamP2, GamS, GamP, RMU, &
       YV( 5 ), PEKRT, RCmplx, CPHS, CSHS, CInside
  COMPLEX (KIND=8), INTENT( OUT ) :: F, G
  CHARACTER BCType*1, BotTop*3

  IPow = 0 

  ! *** Get Rho, C just INSide the boundary ***                       
  ! (There is at least one acoustic layer in the problem, except
  !  in the case where BOUNCE is used to get the refl. coef. for       
  !  a purely elastic stack of layers.                                 

  IF ( BotTop(1:3) == 'TOP' ) THEN 
     IF ( NFAct > 0 ) THEN
        I = Loc( NFAct ) + N( NFAct ) + 1
        RhoINS = Rho( I )
        CInside = PEKRT( Omega2 * H( NFAct ) **2 / ( 2.0 + B1( NFAct ) ) )
     ENDIF
  ELSE 
     IF ( NLACT > 0 ) THEN 
        I = Loc( NLACT ) + N( NLACT ) + 1 
        RhoINS = Rho( I )
        CInside = PEKRT( Omega2 * H( NLACT ) **2 / ( 2.0 + B1( I ) ) )
     ENDIF
  ENDIF

  ! *** Vacuum ***                                                    

  IF ( BCType(1:1) == 'V' ) THEN 
     F = 1.0 
     !G = -CI * PEKRT( Omega2 / CInside ** 2 - X ) * SIGMA( 1 ) ** 2
     G = 0.0
     YV( 1 ) = F
     YV( 2 ) = G
     YV( 3 ) = 0.0
     YV( 4 ) = 0.0
     YV( 5 ) = 0.0
  ENDIF

  ! *** Vacuum with Twersky scatter model ***                         

  IF ( BCType(1:1) == 'S' .OR. BCType(1:1) == 'H' .OR.          &
       BCType(1:1) == 'T' .OR. BCType(1:1) == 'I' ) THEN        

     Omega = SQRT( Omega2 ) 
     KX = SQRT( X )
     F = 1.0
     C0 = REAL( CInside )
     G = TWERSK( BCType, Omega, BUMDEN, XI, ETA, KX, RhoINS, C0 )
     G = G / ( CI * Omega * RhoINS )
     YV( 1 ) = F
     YV( 2 ) = G
     YV( 3 ) = 0.0
     YV( 4 ) = 0.0
     YV( 5 ) = 0.0
  ENDIF

  ! *** Rigid ***                                                     
  IF ( BCType(1:1) == 'R' ) THEN 
     F = 0.0
     G = 1.0
     YV( 1 ) = F
     YV( 2 ) = G
     YV( 3 ) = 0.0
     YV( 4 ) = 0.0
     YV( 5 ) = 0.0
  ENDIF

  ! *** Acousto-elastic half-space ***                                

  IF ( BCType(1:1) == 'A' ) THEN 
     IF ( REAL( CSHS ) > 0.0 ) THEN 
        GamS2 = X - Omega2 / CSHS ** 2 
        GamP2 = X - Omega2 / CPHS ** 2

        GamS = PEKRT( GamS2 )
        GamP = PEKRT( GamP2 )

        RMU = RhoHS * CSHS ** 2

        YV( 1 ) = ( GamS*GamP - X ) / RMU 
        YV( 2 ) = ( ( GamS2 + X ) ** 2 - 4.0*GamS * GamP * X ) * RMU
        YV( 3 ) = 2.0 * GamS * GamP - GamS2 - X
        YV( 4 ) = GamP * ( X - GamS2 )
        YV( 5 ) = GamS * ( GamS2 - X )

        F = Omega2 * YV( 4 )
        G = YV( 2 )
     ELSE 
        GamP = PEKRT( X - Omega2 / CPHS ** 2 ) 
        F = 1.0
        G = RhoHS / GamP 
     ENDIF
  ENDIF

  ! *** Tabulated reflection coefficient ***                          

  IF ( BCType(1:1) == 'F' ) THEN 
     ! Compute the grazing angle THETA
     KX = SQRT( X ) 
     KZ = SQRT( Omega2 / CInside ** 2 - X ) 
     RadDeg = 360.0 / ( 2.0 * PI )
     ThetaInt = RadDeg * DATAN2( DBLE( KZ ), DBLE( KX ) )

     ! Evaluate R( ThetaInt )
     IF ( BotTop(1:3) == 'TOP' ) THEN
        CALL REFCO( ThetaInt, RInt, PhiInt, ThetaTop, RTop, PhiTop, NTopPts, PRTFil )
     ELSE
        CALL REFCO( ThetaInt, RInt, PhiInt, ThetaBot, RBot, PhiBot, NBotPts, PRTFil )
     ENDIF

     ! Convert R(THETA) to (f,g) in Robin BC
     RCmplx = RInt * EXP( CI * PhiInt )
     F = 1.0
     G = ( 1.0 + RCmplx ) / ( CI * KZ * ( 1.0 - RCmplx ) )
  ENDIF

  IF ( BotTop(1:3) == 'TOP' ) G = -G    ! A top BC has the sign flipped relative to a bottom BC

  ! *** Precalculated reflection coef ***                             

  IF ( BCType(1:1) == 'P' ) THEN 
     CALL IRCINT( X, F, G, IPow, XTab, FTab, GTab, ITab, NkTab ) 
  ENDIF

  ! *** Shoot through elastic layers ***                              

  IF ( BotTop(1:3) == 'TOP' ) THEN 

     IF ( NFAct > 1 ) THEN 

        DO Med = 1, NFAct - 1  		! Shoot down from top
           CALL ELASDN( X, YV, IPow, Med )
        END DO

        F = Omega2 * YV( 4 )
        G = YV( 2 )
     ENDIF
  ELSE 

     IF ( NLACT < NMedia ) THEN
        DO Med = NMedia, NLACT + 1, -1     	! Shoot up from bottom
           CALL ELASUP( X, YV, IPow, Med )
        END DO

        F = Omega2 * YV( 4 )
        G = YV( 2 )
     ENDIF
  ENDIF

  RETURN 
END SUBROUTINE BCIMP
!**********************************************************************C
SUBROUTINE ELASUP( X, YV, IPow, Med ) 

  ! Propagates through an elastic layer using compound matrix formulation

  USE krakcmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  PARAMETER ( ROOF = 1.0E5, FLOOR = 1.0E-5, IPowR = 5, IPowF = -5 )
  COMPLEX (KIND=8) :: X, XV( 5 ), YV( 5 ), ZV( 5 ), TWOX, XB3, FOURHX

  ! Euler's method for first step                                     

  TWOX   = 2.0 * X 
  TWOH   = 2.0 * H( Med ) 
  FOURHX = 4.0 * H( Med ) * X
  J = Loc( Med ) + N( Med ) + 1
  XB3 = X * B3( J ) - Rho( J )

  ZV(1) = YV(1) - 0.5*(   B1( J ) * YV( 4 ) - B2( J ) * YV( 5 ) ) 
  ZV(2) = YV(2) - 0.5*( -Rho( J ) * YV( 4 ) -     XB3 * YV( 5 ) ) 
  ZV(3) = YV(3) - 0.5*(      TWOH * YV( 4 ) + B4( J ) * YV( 5 ) )
  ZV(4) = YV(4) - 0.5*(   XB3*YV(1) + B2(J)*YV(2) -TWOX*B4(J)*YV(3)) 
  ZV(5) = YV(5) - 0.5*(Rho(J)*YV(1) - B1(J)*YV(2) -    FOURHX*YV(3)) 

  ! Modified midpoint method                                          

  DO I = N( Med ), 1, -1 
     J = J - 1
     XV = YV 
     YV = ZV 

     XB3 = X * B3( J ) - Rho( J ) 

     ZV(1) = XV(1) - (   B1( J ) * YV( 4 ) - B2( J ) * YV( 5 ) ) 
     ZV(2) = XV(2) - ( -Rho( J ) * YV( 4 ) -     XB3 * YV( 5 ) ) 
     ZV(3) = XV(3) - (      TWOH * YV( 4 ) + B4( J ) * YV( 5 ) )
     ZV(4) = XV(4) - (   XB3*YV(1) + B2(J)*YV(2) - TWOX*B4(J)*YV(3)) 
     ZV(5) = XV(5) - (Rho(J)*YV(1) - B1(J)*YV(2) -     FOURHX*YV(3)) 

     ! *** Scale if necessary ***                                     

     IF ( I /= 1 ) THEN 
        IF ( ABS( DBLE( ZV( 2 ) ) ) < FLOOR ) THEN 
           ZV = ROOF * ZV   ;   YV = ROOF * YV 
           IPow = IPow - IPowR 
        ENDIF

        IF ( ABS( DBLE( ZV( 2 ) ) ) > ROOF  ) THEN 
           ZV = FLOOR * ZV   ;   YV = FLOOR * YV 
           IPow = IPow - IPowF 
        ENDIF
     ENDIF
  END DO

  YV = ( XV + 2.0 * YV + ZV ) / 4.0   ! Apply the standard filter at the terminal point

  RETURN 
END SUBROUTINE ELASUP
!**********************************************************************C
SUBROUTINE ELASDN( X, YV, IPow, Med ) 

  ! Propagates through an elastic layer using compound matrix formulation

  USE krakcmod

  IMPLICIT REAL (KIND=8) (A-H, O-Z)

  COMPLEX (KIND=8) :: X, XV( 5 ), YV( 5 ), ZV( 5 ), TWOX, XB3, FOURHX 
  PARAMETER ( ROOF = 1.0E5, FLOOR = 1.0E-5, IPowR = 5, IPowF = -5 )

  ! Euler's method for first step                                     

  TWOX   = 2.0 * X 
  TWOH   = 2.0 * H( Med ) 
  FOURHX = 4.0 * H( Med ) * X
  J = Loc( Med ) + 1
  XB3 = X * B3( J ) - Rho( J )

  ZV(1) = YV(1) + 0.5*(   B1( J ) * YV( 4 ) - B2( J ) * YV( 5 ) ) 
  ZV(2) = YV(2) + 0.5*( -Rho( J ) * YV( 4 ) -     XB3 * YV( 5 ) ) 
  ZV(3) = YV(3) + 0.5*(      TWOH * YV( 4 ) + B4( J ) * YV( 5 ) )
  ZV(4) = YV(4) + 0.5*(   XB3*YV(1) + B2(J)*YV(2) -TWOX*B4(J)*YV(3)) 
  ZV(5) = YV(5) + 0.5*(Rho(J)*YV(1) - B1(J)*YV(2) -    FOURHX*YV(3)) 

  ! Modified midpoint method                                          

  DO I = 1, N( Med ) 
     J = J + 1

     XV = YV 
     YV = ZV 

     XB3 = X * B3( J ) - Rho( J ) 

     ZV(1) = XV(1) + (   B1( J ) * YV( 4 ) - B2( J ) * YV( 5 ) ) 
     ZV(2) = XV(2) + ( -Rho( J ) * YV( 4 ) -     XB3 * YV( 5 ) ) 
     ZV(3) = XV(3) + (      TWOH * YV( 4 ) + B4( J ) * YV( 5 ) )
     ZV(4) = XV(4) + (   XB3*YV(1) + B2(J)*YV(2) - TWOX*B4(J)*YV(3)) 
     ZV(5) = XV(5) + (Rho(J)*YV(1) - B1(J)*YV(2) -     FOURHX*YV(3)) 

     ! *** Scale if necessary ***                                     

     IF ( I /= N( Med ) ) THEN 
        IF ( ABS( DBLE( ZV( 2 ) ) ) < FLOOR ) THEN
           ZV = ROOF * ZV   ;   YV = ROOF * YV 
           IPow = IPow - IPowR 
        ENDIF

        IF ( ABS( DBLE( ZV( 2 ) ) ) > ROOF  ) THEN 
           ZV = FLOOR * ZV   ;   YV = FLOOR * YV 
           IPow = IPow - IPowF 
        ENDIF
     ENDIF
  END DO

  YV = ( XV + 2.0 * YV + ZV ) / 4.0   ! Apply the standard filter at the terminal point

  RETURN 
END SUBROUTINE ELASDN
