      SUBROUTINE EVALMO( CA, CB, CKA, CKB, PHIA, PHIB, M,               &
     &   RD, NRD, ALPHA, NALPHA,                                        &
     &   R, OPT, P )                                                    
                                                                        
!     Computes Multi-Ocean pressure field                               
!     Normalized to pressure of source at 1 meter                       
                                                                        
!     OPT = X     Cartesian   (x, z) coordinates                        
!     OPT = R     Cylindrical (r, z) coordinates                        
!     OPT = S     Scaled cylindrical coordinates                        
!                    ( 1/r fall-off removed for shade plotting )        
                                                                        
!     ALPHA = vector of ocean weights                                   
                                                                        
      COMPLEX CI 
      PARAMETER ( MAXM = 1500, CI = ( 0.0, 1.0 ), PI = 3.1415926 ) 
                                                                        
      REAL RD( * ), ALPHA( * ) 
      COMPLEX CA( * ), CB( * ), PHIA( MAXM, * ), PHIB( MAXM, * ),       &
     &   CONST( MAXM ), HANK( MAXM ), SUM, CKINT( MAXM ), PHIINT,       &
     &   P( NRD, * ), CKA( MAXM ), CKB( MAXM )                          
      CHARACTER OPT*( * ) 
                                                                        
!     *** Step through oceans ***                                       
                                                                        
      DO 5000 IALPHA = 1, NALPHA 
                                                                        
!        ------ Compute weight W and interpolate                        
                                                                        
         W = ALPHA( IALPHA ) 
                                                                        
         DO 3000 L = 1, M 
                                                                        
            CONST( L ) = SQRT( 2.0 * PI ) * EXP( CI * PI / 4.0 ) *      &
     &                  ( CA( L ) + W * (  CB( L ) -  CA( L ) ) )       
            CKINT( L ) = CKA( L ) + W * ( CKB( L ) - CKA( L ) ) 
                                                                        
                                                                        
            IF ( OPT(1:1) .EQ. 'R' ) THEN 
!              ------ 'R' Cylindrical coordinates                       
               HANK( L ) = CONST( L ) * EXP( -CI * CKINT( L ) * R ) /   &
     &                                SQRT( CKINT( L ) * R )            
            ELSE IF ( OPT(1:1) .EQ. 'X' ) THEN 
!              ------ 'X' Cartesian coordinates                         
               HANK( L ) = CONST( L ) * EXP( -CI * CKINT( L ) * R ) /   &
     &                                CKINT( L )                        
            ELSE 
!              ------ 'S' Scaled cylindrical coord                      
               HANK( L ) = CONST( L ) * EXP( -CI * CKINT( L ) * R ) /   &
     &                                SQRT( CKINT( L ) )                
            ENDIF 
                                                                        
 3000    CONTINUE 
                                                                        
!        *** For each rcvr, add up modal contributions ***              
                                                                        
         DO 4000 I = 1, NRD 
            SUM = 0.0 
                                                                        
            DO 2000 MODE = 1, M 
               PHIINT = PHIA( MODE, I ) +                               &
     &            W * ( PHIB( MODE, I ) - PHIA( MODE, I ) )             
               SUM = SUM + PHIINT * HANK( MODE ) 
 2000       CONTINUE 
                                                                        
            P( I, IALPHA ) = SUM 
                                                                        
 4000    CONTINUE 
                                                                        
                 ! Next ALPHA                                           
 5000 END DO 
                                                                        
      RETURN 
      END                                           
