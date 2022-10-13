SUBROUTINE EVAL3D( CK, PHIR, PHIS, M, IELTSC, ADJELT, ISET,       &
     X, Y, NNODES, NODE, NELTS, XS, YS, THETA, NTHETA, RMINM, RMAXM, NR, MACT, OPT, P )                               

  ! Computes 3-D pressure field using adiabatic mode theory           
  ! Normalized to pressure of point source at 1 meter                 
  ! Note RMINM must be zero                                           

  INTEGER ZBARFIL 
  COMPLEX CI 
  PARAMETER ( KBARFIL = 55, ZBARFIL = 56, PI = 3.141592, DEGRAD = PI/180.0, CI = ( 0.0, 1.0 ), MAXM = 200 )
  INTEGER   M( * ), NODE( 3, * ), ADJELT( 3, * ), ISET( * ), OUTSIDE 
  REAL      X( * ), Y( * ), THETA( * ) 
  COMPLEX   PHIR( MAXM, * ), PHIIN( MAXM ), PHIOUT( MAXM ), PHIS( MAXM, * ), CONST( MAXM ), &
       CK(   MAXM, * ),  CKIN( MAXM ),  CKOUT( MAXM ), P( NTHETA, * ), T, CKINT, PHIINT
  COMPLEX (KIND=8), ALLOCATABLE :: SUMK( : ) 
  CHARACTER OPT*( * ) 

  ! Open file for eigenfunctions                                  
  OPEN ( FILE = 'ZBARFIL', UNIT = ZBARFIL, STATUS = 'UNKNOWN' ) 

  !  *** Loop over angle ***                                           

  DELTAR = ( RMAXM - RMINM ) / ( NR - 1 ) 

  DO ITHETA = 1, NTHETA 
     TSX = COS( DEGRAD * THETA( ITHETA ) )   ;   TSY = SIN( DEGRAD * THETA( ITHETA ) ) 

     ! Get modal values                                           
     IELT = IELTSC 
     WRITE( *, * ) 
     WRITE( *, * ) 'ITHETA, TSX, TSY', ITHETA, TSX, TSY 

     CALL FIRST( IELT, OUTSIDE, RIN, ROUT, XS, YS, TSX, TSY,        &
          X, Y, MPROP, M, MAXM, CK, PHIR, PHIS, CONST, NODE,          &
          ISET, CKIN, PHIIN, CKOUT, PHIOUT )                          

     MPROP = MIN( MACT, MPROP ) 

     ! write modes at first range                                 
     IF ( ITHETA == 1 ) THEN 
        WRITE( ZBARFIL, * ) MPROP 
        WRITE( ZBARFIL, * ) ( CONST( L ), L = 1, MPROP ) 
     END IF

     CONST( 1:MPROP ) = CI * SQRT( 2.0 * PI ) * EXP( CI * PI / 4.0 ) * CONST( 1:MPROP )

     IF ( ITHETA == 1 ) ALLOCATE( SUMK( MPROP ) )
     SUMK = 0.0

     ! *** March forward in range ***                                 

     DO IR = 1, NR 
        RM = RMINM + ( IR - 1 ) * DELTAR 
        IF ( RM == 0.0 ) RM = MIN( 1.0, DELTAR ) 

        ! Crossing into new element?                                  

        DO WHILE ( RM > ROUT ) 

           ! Copy outside info to inside                          
           NEWELT = ADJELT( OUTSIDE, IELT ) 
           RIN = ROUT 

           CKIN(  1:MPROP ) =  CKOUT( 1:MPROP ) 
           PHIIN( 1:MPROP ) = PHIOUT( 1:MPROP ) 

           ! Get new outside info                                 
           CALL OUT( IELT, NEWELT, OUTSIDE, ROUT, ADJELT, ISET,     &
                XS, YS, TSX, TSY, X, Y, NODE, MPROP, M, MAXM, CK, PHIR, CKOUT, PHIOUT )             
           IELT = NEWELT 

        END DO

        ! *** Compute modal contribution at this range ***            

        T = 0.0 

        IF ( RIN /= ROUT ) THEN 
           ALPHA = ( RM - RIN ) / ( ROUT - RIN ) 
           ALPHA = MIN( MAX( ALPHA,  0.0 ), 1.0 ) 
        ELSE 
           ALPHA = 0.0 
        ENDIF

        IF ( IR == NR ) WRITE( ZBARFIL, * ) MPROP 
        DO L = 1, MPROP 
           CKINT     =  CKIN( L ) + ALPHA * (  CKOUT(L) -  CKIN(L) ) 
           PHIINT    = PHIIN( L ) + ALPHA * ( PHIOUT(L) - PHIIN(L) ) 
           SUMK( L ) =  SUMK( L ) + DELTAR * CKINT 
           T         = T + PHIINT * CONST( L ) * EXP( -CI*SUMK( L ) ) / SQRT( CKINT )       
           IF ( IR == NR ) WRITE( ZBARFIL, * ) PHIINT  ! write mode at last range     
        END DO

        P( ITHETA, IR ) = T / SQRT( RM ) 

     END DO   ! Next range

     ! Write average wavenumber to file                           
     OPEN ( FILE = 'KBARFIL', UNIT = KBARFIL, STATUS = 'UNKNOWN' ) 
     WRITE( KBARFIL, * ) MPROP, RM 
     WRITE( KBARFIL, * ) ( SUMK( L ) / RM , L = 1, MPROP )

  END DO ! Next bearing

  RETURN 
END SUBROUTINE EVAL3D
!**********************************************************************C
SUBROUTINE FIRST( IELT, OUTSIDE, RIN, ROUT, XS, YS, TSX, TSY,     &
     X, Y, MPROP, M, MAXM, CK, PHIR, PHIS, CONST, NODE, ISET, CKIN, PHIIN, CKOUT, PHIOUT )                             

  ! Given an element number for the source                            
  ! Computes                                                          
  !    OUTSIDE    the side through which path exits                   
  !    ROUT       the range at which path exits                       
  !    Mode values at entrance and exit                               
  !    Interpolated mode excitation coeffs                            

  INTEGER NODE( 3, * ), OUTSIDE, M( * ), ICOR( 3, 2 ), ISET( * ) 
  REAL    X( * ), Y( * ), RV( 3 ), SV( 3 ), RVC( 3 ) 
  COMPLEX PHIS( MAXM, * ), PHIIN( * ), PHIOUT( * ), PHIR( MAXM, * ), CONST( * ), CKIN( * ), CKOUT( * ), CK( MAXM, * )          
  DATA ( (ICOR( IS, J), IS = 1, 3), J = 1, 2) /1, 2, 3, 2, 3, 1/ 

  ! ICOR maps a side (1, 2 OR 3) and a local node (1 OR 2) to a       
  !      corner (1, 2, or 3) of the triangle                        

  WRITE( *, * ) 'IELT = ', IELT 
  MPROP = 9999 

  ! Coordinates of the centroid of the source element                 

  XCEN = SUM( X( NODE( 1:3, IELT ) ) ) / 3.0   ;   YCEN = SUM( Y( NODE( 1:3, IELT ) ) ) / 3.0 

  DO ISIDE = 1, 3 

     NODE1 = NODE( ICOR( ISIDE, 1 ), IELT )   ;   NODE2 = NODE( ICOR( ISIDE, 2 ), IELT )
     ISET1 = ISET( NODE1 )                    ;   ISET2 = ISET( NODE2 ) 
     MPROP = MIN( MPROP, M( ISET1 ), M( ISET2 ) ) 

     X1S = X( NODE1 ) - XS         ;         Y1S = Y( NODE1 ) - YS
     TXC = X( NODE1 ) - XCEN       ;         TYC = Y( NODE1 ) - YCEN                                 
     TX  = X( NODE2 ) - X( NODE1 ) ;         TY  = Y( NODE2 ) - Y( NODE1 ) 

     DELTA = TSX * TY - TSY * TX 

     ! *** Radial parallel to side? ***                               

     IF ( DELTA == 0.0 ) THEN 
        SV(  ISIDE )    = 999.0 
     ELSE 
        RVC( ISIDE ) = ( TXC * TY  - TYC * TX  ) / DELTA 
        RV(  ISIDE ) = ( X1S * TY  - Y1S * TX  ) / DELTA 
        SV(  ISIDE ) = ( X1S * TSY - Y1S * TSX ) / DELTA 
     ENDIF
  END DO

  ! Identify two good sides and one bad side based on the intercept point

  IBAD = 1 
  IF ( ABS( SV( 2 ) - 0.5 ) > ABS( SV( IBAD ) - 0.5 ) ) IBAD = 2                                                       
  IF ( ABS( SV( 3 ) - 0.5 ) > ABS( SV( IBAD ) - 0.5 ) ) IBAD = 3                                                       

  IGOOD1 = 1
  IGOOD2 = 2
  IF ( IBAD == 1 ) THEN 
     IGOOD1 = 3 
  ELSE IF ( IBAD == 2 ) THEN 
     IGOOD2 = 3 
  ENDIF

  ! The side with the lesser RVC is the inside                    

  IF ( RVC( IGOOD1 ) < RVC( IGOOD2 ) ) THEN 
     INSIDE  = IGOOD1   ;   OUTSIDE = IGOOD2 
  ELSE 
     INSIDE  = IGOOD2   ;   OUTSIDE = IGOOD1 
  ENDIF

  SIN      = SV( INSIDE ) 
  SIN      = MIN( MAX( SIN,  0.0 ), 1.0 ) 
  RIN      = RV( INSIDE ) 

  SOUT     = SV( OUTSIDE ) 
  SOUT     = MIN( MAX( SOUT, 0.0 ), 1.0 ) 
  ROUT     = RV( OUTSIDE ) 

  ! Get values of modes at Z = SD and (X, Y) = intercept points   

  ICOR1 = ICOR(  INSIDE, 1 )   ;   ICOR2 = ICOR(  INSIDE, 2 ) 
  ICOR3 = ICOR( OUTSIDE, 1 )   ;   ICOR4 = ICOR( OUTSIDE, 2 ) 

  ! Interpolate to get modal values at source                     
  R = 0.0 

  IF ( RIN /= ROUT ) THEN 
     ALPHA = ( R - RIN ) / ( ROUT - RIN ) 
     ALPHA = MIN( MAX( ALPHA,  0.0 ), 1.0 ) 
  ELSE 
     ALPHA = 0.0 
  ENDIF

  DO L = 1, MPROP 
     PHIIN(  L ) = PHIS( L, ICOR1 ) + SIN  * ( PHIS( L, ICOR2 ) - PHIS( L, ICOR1 ) )   
     PHIOUT( L ) = PHIS( L, ICOR3 ) + SOUT * ( PHIS( L, ICOR4 ) - PHIS( L, ICOR3 ) )   
     CONST(  L ) = PHIIN(L) + ALPHA * ( PHIOUT(L) - PHIIN(L) ) 
  END DO

  ! Obtain values of modes at z = Rd and (x, y)=intercept points      

  CALL INTER1( IELT, INSIDE,  SIN, MPROP, M, MAXM, CK, PHIR, NODE, ISET, CKIN,  PHIIN  )          
  CALL INTER1( IELT, OUTSIDE, SOUT, MPROP, M, MAXM, CK, PHIR, NODE, ISET, CKOUT, PHIOUT )          

  RETURN 
END SUBROUTINE FIRST
!**********************************************************************C
SUBROUTINE OUT( IELT, NEWELT, OUTSIDE, ROUT, ADJELT, ISET, XS, YS,&
     &   TSX, TSY, X, Y, NODE, MPROP, M, MAXM, CK, PHIR, CKOUT, PHIOUT)                                       

  ! Given an element number and a side through which prop path enters 
  ! Computes                                                          
  !    OUTSIDE    the side through which path exits                   
  !    ROUT       the range at which path exits                       
  !    Mode values                                                    

  INTEGER NODE( 3, * ), ADJELT( 3, * ), OUTSIDE, M( * ), ICOR( 3, 2 ), ISET( * )
  REAL    X( * ), Y( * ) 
  COMPLEX PHIOUT( * ), PHIR( MAXM, * ), CKOUT( * ), CK( MAXM, * ) 
  DATA ( (ICOR( IS, J), IS = 1, 3), J = 1, 2) /1, 2, 3, 2, 3, 1/ 

  ! ICOR maps a side (1, 2 OR 3) and a local node (1 OR 2) to a       
  !        corner (1, 2, or 3) of the triangle                        
  ! WRITE( *, * ) '   Crossing into new element = ', NEWELT           

  ! If no adj elt then exit. Previous values of KOUT, PHIOUT retained 

  IF ( NEWELT == 0 ) THEN 
     ROUT = 9.9999E9 
     RETURN 
  ENDIF

  ! *** Loop over sides to find outside ***                           

  SOUT = 999.0 

  DO ISIDE = 1, 3 

     ! Don't try an outside the same as the inside             
     IF ( ADJELT( ISIDE, NEWELT ) /= IELT ) THEN 

        NODE1 = NODE( ICOR( ISIDE, 1 ), NEWELT ) 
        NODE2 = NODE( ICOR( ISIDE, 2 ), NEWELT ) 

        X1S = X( NODE1 ) - XS           ;   Y1S = Y( NODE1 ) - YS                                               
        TX  = X( NODE2 ) - X( NODE1 )   ;   TY  = Y( NODE2 ) - Y( NODE1 ) 

        DELTA = TSX * TY - TSY * TX 

        ! *** Radial parallel to side? ***                            

        IF ( DELTA == 0.0 ) THEN 
           ST    = 999.0 
        ELSE 
           ROUTT = ( X1S * TY  - Y1S * TX  ) / DELTA 
           ST    = ( X1S * TSY - Y1S * TSX ) / DELTA 
        ENDIF
        ! If intercept is in segment, store the side number       
        IF ( ABS( ST - 0.5 ) < ABS( SOUT - 0.5) ) THEN 
           OUTSIDE = ISIDE 
           SOUT    = ST 
           ROUT    = ROUTT 
        ENDIF
     ENDIF
  END DO

  ! Obtain values of modes at intercept points                        

  CALL INTER1( NEWELT, OUTSIDE, SOUT, MPROP, M, MAXM, CK, PHIR, NODE, ISET, CKOUT, PHIOUT )          

  RETURN 
END SUBROUTINE OUT
!**********************************************************************C
SUBROUTINE INTER1( IELT, ISIDE, S, MPROP, M, MAXM, CK, PHIR, NODE, ISET, CKINT, PHIINT )          

  ! Given                                                             
  !    IELT         the element                                       
  !    ISIDE        the side                                          
  !    S            the proportional distance along the side          

  ! Returns                                                           
  !    CKINT, PHIINT  interpolated modal values                       
  !    MPROP          number of propagating modes                     

  INTEGER NODE( 3, * ), M( * ), ICOR( 3, 2 ), ISET( * ) 
  COMPLEX PHIINT( * ), PHIR( MAXM, * ), CKINT( * ), CK( MAXM, * )
  DATA ( (ICOR( IS, J), IS = 1, 3), J = 1, 2) /1, 2, 3, 2, 3, 1/ 

  ! ICOR maps a side (1, 2 OR 3) and a local node (1 OR 2) to a       
  !        corner (1, 2, or 3) of the triangle                        

  NODE1 = NODE( ICOR( ISIDE, 1 ), IELT )   ;   NODE2 = NODE( ICOR( ISIDE, 2 ), IELT )                                         
  ISET1 = ISET( NODE1 )                    ;   ISET2 = ISET( NODE2 ) 
  MPROP = MIN( MPROP, M( ISET1 ), M( ISET2 ) ) 

  ! Extrapolation is blocked by making sure s is in [0, 1]                                   

  S = MIN( MAX( S, 0.0 ), 1.0 ) 

  DO I = 1, MPROP 
     CKINT(  I ) =   CK( I, ISET1 ) + S * (   CK( I, ISET2 ) -   CK( I, ISET1 ) )  
     PHIINT( I ) = PHIR( I, ISET1 ) + S * ( PHIR( I, ISET2 ) - PHIR( I, ISET1 ) )  
  END DO

  RETURN 
END SUBROUTINE INTER1
