      SUBROUTINE EVALPDQ( CK, PHIR, PHIS, M, IELTSC, ADJELT, ISET,      &
     &   X, Y, NNODES, NODE, NELTS, XS, YS, THETA, NTHETA, RMINM, RMAXM, NR, MACT, OPT, P )

!     Computes 3-D pressure field using adiabatic mode theory           
!     Normalized to pressure of point source at 1 meter                 

!     Note RMINM must be zero

      COMPLEX CI
      PARAMETER ( PI = 3.141592, DEGRAD = PI/180.0, CI = ( 0.0, 1.0 ), MAXM = 100 )
      INTEGER M( * ), NODE( 3, * ), ADJELT( 3, * ), ISET( * ), OUTSIDE
      REAL    X( * ), Y( * ), THETA( * )
      COMPLEX PHIR( MAXM, * ), PHIIN( MAXM ), PHIOUT( MAXM ),  PHIS( MAXM, * ), COEF( MAXM ), &
     &        CK(   MAXM, * ), CKIN( MAXM ), CKOUT( MAXM ),  P( NTHETA, * )

      COMPLEX, ALLOCATABLE :: PHASEINC( : ), CKAVG( : )
      CHARACTER OPT*( * )

!     *** Loop over angle ***

      DELTAR = ( RMAXM - RMINM ) / ( NR - 1 )

      DO ITHETA = 1, NTHETA
         TSX = COS( DEGRAD * THETA( ITHETA ) )
         TSY = SIN( DEGRAD * THETA( ITHETA ) )

!        --- Get modal values                                          
         IELT = IELTSC
         WRITE( *, * )
         WRITE( *, * ) 'ITHETA, TSX, TSY', ITHETA, TSX, TSY

         CALL FIRST( IELT, OUTSIDE, RIN, ROUT, XS, YS, TSX, TSY,        &
     &      X, Y, MPROP, M, MAXM, CK, PHIR, PHIS, COEF, NODE, ISET, CKIN, PHIIN, CKOUT, PHIOUT )
         MPROP = MIN( MACT, MPROP )
 
         IF ( ITHETA == 1 ) ALLOCATE( PHASEINC( MPROP ), CKAVG( MPROP ) )

         COEF(     1:MPROP ) = SQRT( 2.0 * PI ) * EXP( CI * PI / 4.0 ) * COEF( 1:MPROP )                         
         CKAVG(    1:MPROP ) = 0.5 * ( CKIN( 1:MPROP ) + CKOUT( 1:MPROP ) ) 
         PHASEINC( 1:MPROP ) = EXP( -CI * CKAVG( 1:MPROP ) * DELTAR )

!        *** March forward in range ***                                 
                                                                        
         DO IR = 1, NR 
            RM = RMINM + ( IR - 1 ) * DELTAR 
            IF ( RM == 0.0 ) RM = DELTAR 
                                                                        
!           Crossing into new element?                                  

            DO WHILE ( RM > ROUT )

               ! --- Copy outside info to inside                          
               NEWELT = ADJELT( OUTSIDE, IELT ) 
               RIN = ROUT
               CKIN(     1:MPROP ) =  CKOUT( 1:MPROP ) 
               PHIIN(    1:MPROP ) = PHIOUT( 1:MPROP ) 
               CKAVG(    1:MPROP ) = 0.5 * ( CKIN( 1:MPROP ) + CKOUT( 1:MPROP ) ) 
               PHASEINC( 1:MPROP ) = EXP( -CI * CKAVG( 1:MPROP ) * DELTAR )

               ! --- Get new outside info                                 
               CALL OUT( IELT, NEWELT, OUTSIDE, ROUT, ADJELT, ISET,     &
     &            XS, YS, TSX, TSY, X, Y, NODE, MPROP, M, MAXM, CK, PHIR, CKOUT, PHIOUT )
               IELT = NEWELT 

            END DO   ! Check again

!           *** Compute modal contribution at this range ***

            COEF( 1:MPROP ) = COEF( 1:MPROP ) * PHASEINC( 1:MPROP )
            P( ITHETA, IR ) = SUM( COEF( 1:MPROP ) * PHIIN( 1:MPROP ) ) / SQRT( RM * CKIN( 1 ) ) 

         END DO ! next range

      END DO ! next bearing
                                                           
      RETURN 
      END                                           
