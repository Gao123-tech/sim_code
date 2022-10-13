      SUBROUTINE EVALGB( CK, PHI, PHIR, M, MAXM, IELTSC, ADJELT, ISET,  &
     &   X, Y, NNODES, NODE, NELTS, XS, YS, THETA, NTHETA,              &
     &   RMINM, RMAXM, NR, MACT, OPTION, P )                            
                                                                        
!     Computes 3-D pressure field using adiabatic mode theory           
                                                                        
!     Uses gaussian beam tracing for horizontal refraction              
!     Note beam curvature change at interfaces still needed             
!     Phase may be off by e(i pi / 4 ) but TL is correct.               
                                                                        
      INTEGER FLPFIL, RAYFIL 
      COMPLEX CI 
      PARAMETER ( FLPFIL = 5, RAYFIL = 21, PI = 3.141592, DEGRAD = PI / 180.0, CI = ( 0.0, 1.0 ) )

      LOGICAL EXITED 
      INTEGER M( * ), NODE( 3, * ), ADJELT( 3, * ), ISET( * ) 
      REAL X( * ), Y( * ), THETA( * ), RADVEC( 2, 81 ), XV( 50000 ), YV( 50000 )                                       
      COMPLEX CK(MAXM, *), PHI(MAXM, *), PHIR(MAXM, 3), P(NTHETA, *),   &
         CONST, PHIA, PHIB, PA, PB, QA, QB, TAUA, TAUB, CA, CB, EPS, EPSOPT, CX, CY                                            
      CHARACTER OPTION*( * ) 
                                                                        
      OPEN( FILE = 'RAYFIL', UNIT = RAYFIL ) 
                                                                        
      READ( FLPFIL, * ) ALPHA1, ALPHA2, NALPHA 
      READ( FLPFIL, * ) STEPG, NSTEPS 
      READ( FLPFIL, * ) EPMULT 
      ALPHA1 = DEGRAD * ALPHA1 
      ALPHA2 = DEGRAD * ALPHA2 
                                                                        
!     *** Initialization ***                                            
                                                                        
      IBWIN2 = 25 
      DALPHA = ( ALPHA2 - ALPHA1 ) / ( NALPHA - 1 )
                                                                        
      DO ITHETA = 1, NTHETA 
         TH = DEGRAD * THETA( ITHETA ) 
         RADVEC( 1, ITHETA ) = COS( TH ) 
         RADVEC( 2, ITHETA ) = SIN( TH )
         P( ITHETA, 1:NR ) = 0.0
      ENDDO 
                                                                        
!     *** Loop over beam takeoff angle ***                              
                                                                        
ANGLE: DO IALPHA = 1, NALPHA 
!         if ( ialpha == 112 ) then                                   
         ALPHA = ALPHA1 + ( IALPHA - 1 ) * DALPHA 
         TSX = COS( ALPHA ) 
         TSY = SIN( ALPHA ) 
         WRITE( *, * ) 'IALPHA, TSX, TSY', IALPHA, TSX, TSY 
                                                                        
!        *** Loop over modes ***                                        
                                                                        
         DO MODE = 1, MACT 
                                                                        
!           --- Get mode excitation coef and initialize                 
                                                                        
            IELT = IELTSC 
                                                                        
            CALL NEWELT( IELT, NODE, X, Y, CK, MODE, M, MAXM, ISET,     &
               ISET1, ISET2, ISET3, X1, Y1, X2, Y2, X3, Y3, D12, D13, D23, DELTA, CX, CY, MPROP )                    
                                                                        
            IF ( MPROP < MODE ) CYCLE ANGLE

!           --- Evaluate modes at source depth                          
            DB1 = XS * Y1 - YS * X1 
            DB2 = XS * Y2 - YS * X2 
            DB3 = XS * Y3 - YS * X3 
                                                                        
!           --- Compute areas of triangles                              
            A1 = (  D23 - DB3 + DB2 ) / DELTA 
            A2 = (  DB3 - D13 - DB1 ) / DELTA 
            A3 = ( -DB2 + DB1 + D12 ) / DELTA 
            CA = A1 / CK(MODE, ISET1) + A2 / CK(MODE, ISET2) + A3 / CK(MODE, ISET3)            
            PHIA = A1 * PHIR(MODE, 1) + A2 * PHIR(MODE, 2)   + A3 * PHIR(MODE, 3)              
                                                                        
!           --- Space filling in far field:                             
                                                                        
            IF ( OPTION(5:5) == 'F' ) THEN 
               HWIDTH = 2.0 / ( ( 1.0 / CA ) * DALPHA ) 
               EPSOPT = 0.5 * HWIDTH ** 2 
            ENDIF 
                                                                        
!           --- Minimum width at RMAXM                                  
                                                                        
            IF ( OPTION(5:5) == 'M' ) THEN 
               HWIDTH = SQRT( 2.0 * CA * RMAXM ) 
               EPSOPT = 0.5 * HWIDTH ** 2 
            ENDIF 
                                                                        
            EPS = EPMULT * CI * EPSOPT 
                                                                        
            CONST = PHIA * SQRT( EPS / CA ) * DALPHA 
            XA    = XS 
            YA    = YS 
            XIA   = TSX / CA 
            ETAA  = TSY / CA 
            PA    = 1.0 
            QA    = EPS 
            TAUA  = 0.0 
            KMAHA = 1 
                                                                        
!           --- Evaluate modes at rcvr depth                            
            PHIA = A1 * PHI(MODE, ISET1) + A2 * PHI(MODE, ISET2) + A3 * PHI(MODE, ISET3)        
                                                                        
!           *** March forward in range ***                              
                                                                        
            EXITED = .FALSE. 
            STEP = STEPG 
                                                                        
            DO ISTEP = 1, NSTEPS 
               XB   = XA   + STEP * CA * XIA 
               YB   = YA   + STEP * CA * ETAA 
               XIB  = XIA  - STEP * CX / ( CA * CA ) 
               ETAB = ETAA - STEP * CY / ( CA * CA ) 
               PB   = PA 
               QB   = QA   + STEP * CA * PA 
               TAUB = TAUA + STEP / CA 
                                                                        
                              ! Update step size                        
               STEP = STEPG 
                                                                        
               IF ( OPTION(6:6) == 'R' ) THEN 
                  XV( ISTEP ) = XA 
                  YV( ISTEP ) = YA 
               ENDIF 
                                                                        
!              --- Compute KMAH index                                   
               KMAHB = KMAHA 
               IF ( REAL( QB ) < 0.0 ) THEN 
                  QAT = AIMAG( QA ) 
                  QBT = AIMAG( QB ) 
                  IF ( ( QAT < 0.0 .AND. QBT >= 0.0 ) .OR. &
                       ( QAT > 0.0 .AND. QBT <= 0.0 ) ) KMAHB = -KMAHA
               ENDIF 
                                                                        
!              --- Mode interpolation coefs                             
                                                                        
               IF ( .NOT. EXITED ) THEN 
                  DB1 = XB * Y1 - YB * X1 
                  DB2 = XB * Y2 - YB * X2 
                  DB3 = XB * Y3 - YB * X3 
!                 --- Compute areas of triangles                        
                  A1 = (  D23 - DB3 + DB2 ) / DELTA 
                  A2 = (  DB3 - D13 - DB1 ) / DELTA 
                  A3 = ( -DB2 + DB1 + D12 ) / DELTA 
                                                                        
!                 Crossing into new element?                            
                                                                        
!                 SHOULD CHECK FOR A STEP WHICH CROSSES TWO ELTS!       
                                                                        
                  IF ( ABS(A1)+ABS(A2)+ABS(A3) > 1.00001 ) THEN 
                                                                        
!                    --- Identify the side through which exitting       
                     IF ( A1 < 0.0 ) ISIDE = 2 
                     IF ( A2 < 0.0 ) ISIDE = 3 
                     IF ( A3 < 0.0 ) ISIDE = 1 
                     IELT = ADJELT(ISIDE, IELT) 
!                    IF ( MODE == 1 ) THEN                            
!                       WRITE( *, * ) '   NEW ELEMENT', IELT            
!                    ENDIF                                              
                                                                        
!                    --- Normal elt transition or exit into free space? 
                                                                        
                     IF ( IELT == 0 ) THEN 
                        EXITED = .TRUE. 
                        CX = 0.0 
                        CY = 0.0 
                     ELSE 
                        CALL NEWELT(IELT, NODE, X, Y, CK, MODE, M, MAXM, ISET, ISET1, ISET2, ISET3, &
                           X1, Y1, X2, Y2, X3, Y3, D12, D13, D23, DELTA, CX, CY, MPROP )        
                                                                        
!                       --- If mode cuts off, skip to next mode         
                        IF ( MPROP < MODE ) EXIT
                     ENDIF 
                  ENDIF 
                                                                        
!                 --- Evaluate modes at the rcvr depth                  
                  CB   = A1 /  CK(MODE, ISET1) + A2 /  CK(MODE, ISET2) + A3 /  CK(MODE, ISET3)          
                  PHIB = A1 * PHI(MODE, ISET1) + A2 * PHI(MODE, ISET2) + A3 * PHI(MODE, ISET3)      
               ENDIF 
                                                                        
!              *** Compute beam influence ***                           
                                                                        
               CALL INFLU( XA-XS, YA-YS, XIA, ETAA, PA, QA, TAUA, CA,   &
                  KMAHA, PHIA, XB-XS, YB-YS, XIB, ETAB, PB, QB,         &
                  TAUB, CB, KMAHB, PHIB, RADVEC, NTHETA, RMINM, RMAXM, NR, IBWIN2, CONST, P )                  
                                                                        
               XA    = XB 
               YA    = YB 
               XIA   = XIB 
               ETAA  = ETAB 
               PA    = PB 
               QA    = QB 
               TAUA  = TAUB 
               CA    = CB 
               KMAHA = KMAHB 
               PHIA  = PHIB 
                    ! Next step                                         
            ENDDO 
                                                                        
!           --- Optionally dump rays to disk                            
                                                                        
            IF ( OPTION(6:6) == 'R' ) THEN 
               WRITE( RAYFIL, * ) ISTEP - 1 
               DO ISTEP = 1, ISTEP - 1 
                 WRITE( RAYFIL, * ) XV( ISTEP ), YV( ISTEP ) 
               ENDDO 
            ENDIF 

         ENDDO   ! next mode 
!        endif                                                          
                                             
      END DO ANGLE   ! next beam take-off angle
                                                                        
      RETURN 
      END                                           
!**********************************************************************C
      SUBROUTINE NEWELT( IELT, NODE, X, Y, CK, MODE, M, MAXM, ISET,     &
     &   ISET1, ISET2, ISET3, X1, Y1, X2, Y2, X3, Y3,                   &
     &   D12, D13, D23, DELTA, CX, CY, MPROP )                          
                                                                        
!     Given elt number, returns info which is constant in elt           
                                                                        
      INTEGER M( * ), NODE( 3, * ), ISET( * ) 
      REAL X( * ), Y( * ) 
      COMPLEX CK( MAXM, * ), CX, CY 
                                                                        
      NODE1 = NODE( 1, IELT ) 
      NODE2 = NODE( 2, IELT ) 
      NODE3 = NODE( 3, IELT ) 
                                                                        
      ISET1 = ISET( NODE1 ) 
      ISET2 = ISET( NODE2 ) 
      ISET3 = ISET( NODE3 ) 
                                                                        
!     --- If mode cuts off, return to origin and do next mode           
      MPROP = MIN( M(ISET1), M(ISET2), M(ISET3) ) 
      IF ( MPROP < MODE ) RETURN 
                                                                        
      X1 = X( NODE1 ) 
      Y1 = Y( NODE1 ) 
      X2 = X( NODE2 ) 
      Y2 = Y( NODE2 ) 
      X3 = X( NODE3 ) 
      Y3 = Y( NODE3 ) 
                                                                        
      D12 = X1 * Y2 - Y1 * X2 
      D13 = X1 * Y3 - Y1 * X3 
      D23 = X2 * Y3 - Y2 * X3 
      DELTA = D23 - D13 + D12 
                                                                        
!     --- Gradient                                                      
      A1X = -Y3 + Y2 
      A2X =  Y3 - Y1 
      A3X = -Y2 + Y1 
      A1Y =  X3 - X2 
      A2Y = -X3 + X1 
      A3Y =  X2 - X1 
                                                                        
      CX = ( A1X/CK(MODE, ISET1) + A2X/CK(MODE, ISET2) + A3X/CK(MODE, ISET3) ) / DELTA        
      CY = ( A1Y/CK(MODE, ISET1) + A2Y/CK(MODE, ISET2) + A3Y/CK(MODE, ISET3) ) / DELTA        
                                                                        
      RETURN 
      END                                           
!**********************************************************************C
      SUBROUTINE INFLU(XA, YA, XIA, ETAA, PA, QA, TAUA, CA, KMAHA, PHIA,&
         XB, YB, XIB, ETAB, PB, QB, TAUB, CB, KMAHB, PHIB,              &
         RADVEC, NTHETA, RMINM, RMAXM, NR, IBWIN2, CONST, P )           
                                                                        
!     Computes contribution to receivers assuming beam cannot           
!     contribute to a radial in an incoming sense                       
                                                                        
      COMPLEX CI 
      PARAMETER ( CI = (0.0, 1.0) ) 
                                                                        
      REAL RADVEC( 2, * ), NA, NB, NSQ 
      COMPLEX P( NTHETA, * ), CONST, PHIA, PHIB, PA, PB, QA, QB,        &
     &   TAUA, TAUB, CA, CB, PHIMID, PMID, QMID, TAUMID, CMID, CONTRIB  
                                                                        
!     *** Loop over radials of receiver line and compute contribution **
                                                                        
      DELTAR = ( RMAXM - RMINM ) / ( NR - 1 ) 
                                                                        
      DO ITHETA = 1, NTHETA 
                                                                        
!        --- Compute intercept range, ra & index preceding rcvr         
                                                                        
         DELTAA = RADVEC(1, ITHETA)*XIA + RADVEC(2, ITHETA)*ETAA 
         IF ( ABS( DELTAA ) < 1.0E-5 ) CYCLE
         RA = ( YA * ETAA + XA * XIA ) / DELTAA 
                                                                        
         IF ( RA >= RMAXM ) CYCLE
         IF ( RA <= RMINM ) IR1 = 0 
         IF ( RA < RMAXM .AND. RA > RMINM ) IR1 = IFIX( REAL( ( RA - RMINM ) / DELTAR ) ) + 1

!        --- Compute intercept range, rb & index preceding rcvr         
         DELTAB = RADVEC(1, ITHETA)*XIB + RADVEC(2, ITHETA)*ETAB 
         IF ( ABS( DELTAB ) < 1.0E-5 ) CYCLE
         RB = ( YB * ETAB + XB * XIB ) / DELTAB 
                                                                        
         IF ( RB >= RMAXM+DELTAR ) IR2 = NR 
         IF ( RB <= RMINM ) CYCLE
         IF ( RB < RMAXM+DELTAR .AND. RB > RMINM ) IR2 = IFIX( REAL((RB-RMINM)/DELTAR) ) + 1
!        *** If a receiver is bracketted compute influence ***          
                                                                        
         IF ( IR2 > IR1 .AND. DELTAA*DELTAB > 0 ) THEN 
!           --- Normal distance                                         
            NA = ( XA*RADVEC(2, ITHETA) - YA*RADVEC(1, ITHETA) ) / ( CA * DELTAA )     
            NB = ( XB*RADVEC(2, ITHETA) - YB*RADVEC(1, ITHETA) ) / ( CB * DELTAB )     
                                                                        
            DO IR = IR1+1, IR2 
               R = RMINM + (IR-1)*DELTAR 
               W = (R-RA)/(RB-RA) 
               PMID = PA + W*(PB-PA) 
               QMID = QA + W*(QB-QA) 
               NSQ = (NA + W*(NB-NA))**2 
                                                                        
!              --- Within beam window?                                  
               IF ( -0.5*AIMAG(PMID/QMID)*NSQ < IBWIN2 ) THEN 
                  CMID = CA + W*(CB-CA) 
                  TAUMID = TAUA + W*(TAUB-TAUA) 
                  PHIMID = PHIA + W*(PHIB-PHIA) 
                                                                        
!                 --- Compute KMAH index                                
                  KMAH = KMAHA 
                  IF ( REAL(QMID) < 0.0 ) THEN 
                     QAT = AIMAG( QA ) 
                     QBT = AIMAG( QMID ) 
                     IF ( ( QAT < 0.0 .AND. QBT >= 0.0 ) .OR. ( QAT > 0.0 .AND. QBT <= 0.0 ) ) KMAH = -KMAH                
                  END IF 
                                                                        
                  CONTRIB = CONST*PHIMID*SQRT(CMID/QMID)* EXP(-CI*( TAUMID + 0.5*PMID/QMID*NSQ ) )           
                                                                        
                  IF ( KMAH < 0 ) CONTRIB = -CONTRIB 
                  P(ITHETA, IR) = P(ITHETA, IR) + CONTRIB 
               END IF
            END DO ! Next receiver on radial
         END IF
                          
         END DO   ! Next radial
                                                                        
         RETURN 
      END                                           
