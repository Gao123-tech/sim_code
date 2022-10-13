SUBROUTINE EVALCM( RProf, NProf, phiS, phi, rd, Nrd, R, Nr, ck, M, Opt, P )                                         

  ! Computes pressure field using coupled mode theory                 
  ! Normalized to pressure of point source at 1 meter

  ! Opt = X     Cartesian   (X, z) coordinates                        
  ! Opt = R     Cylindrical (R, z) coordinates

  ! Note number of propagating modes is reset after first segment.    
  ! Thus M restricts the number of modes in the source field but
  ! thereafter energy is allowed to couple into higher-order modes.

  ! Should half-space contribution include imaginary part?         
  ! Then we need to handle the growing part of leaky waves.

  COMPLEX CI 
  PARAMETER ( MaxM = 10000, CI = ( 0.0, 1.0 ), pi = 3.1415926 )
  LOGICAL first 
  REAL      rd( * ), RProf( * ), r( * )
  COMPLEX   phiS( * ), phi( MaxM, * ), A( MaxM ), sum, P( Nrd, * ), ck( * )
  CHARACTER Opt*( * ), Title*80
  SAVE first
  DATA first /.TRUE./

  ! *** Compute ranges (in meters) where new profiles are used ***    

  IF ( first ) THEN  

     DO iProf = NProf, 2, -1 
        RProf( iProf ) = 500.0 * ( RProf( iProf ) + RProf( iProf-1 ) )
     END DO

     RProf(  NProf + 1 ) = 1.0E9 
     first = .FALSE.
  ENDIF

  ! *** Evaluate mode excitation coefficients, A(mode) ***            

  iProf = 1 

  ! THIS HAS ALREADY BEEN READ IN PLOTTLR!                            

  CALL GETMOD( iProf, ' ', MaxM, rd, Nrd, 'N', ck, phi,  M1, freq, Title )              

  M = MIN( M, M1 )                                                    

  IF ( Opt(1:1) == 'X' ) THEN   ! Cartesian coordinates
     A( 1:M ) =      SQRT( 2.0 * pi ) * EXP( CI * pi / 4.0 ) * phiS( 1:M ) / ck( 1:M )
  ELSE                            ! Cylindrical coordinates
     A( 1:M ) = CI * SQRT( 2.0 * pi ) * EXP( CI * pi / 4.0 ) * phiS( 1:M ) / SQRT( ck( 1:M) )
  ENDIF

  ! *** March forward in range ***                                    

  DO ir = 1, Nr 
     !        WRITE( *, * ) 'Range = ', r( ir )

     ! *** Crossing into new range segment? ***

     IF ( r( ir ) > RProf( iProf + 1 ) ) THEN 

        iProf = iProf + 1 

        ! Advance to interface
        IF ( ir == 1 ) THEN 
           CALL ADVPHA( RProf( iProf )              , A, ck, M )
        ELSE
           CALL ADVPHA( RProf( iProf ) - r( ir - 1 ), A, ck, M ) 
        ENDIF

        ! Here's where we cross over
        IF ( iProf  <= NProf ) THEN 
           CALL NEWPRO( iProf, ck, phi, M, rd, Nrd, A )
           WRITE( *, * ) 'New profile read', r(ir ), iProf, ' #modes = ', M
        ENDIF

        ! Are there other segments to cross?
        DO WHILE ( r( ir ) > RProf( iProf + 1 ) ) 
           iProf = iProf + 1
           CALL ADVPHA( RProf( iProf ) - RProf( iProf - 1 ),  A, ck, M )
           IF ( iProf <= NProf ) CALL NEWPRO( iProf, ck, phi, M, rd, Nrd, A )
        END DO

        ! Advance the remaining distance
        CALL ADVPHA( r( ir ) - RProf( iProf ), A, ck, M ) 

     ELSE 

        IF ( ir == 1 ) THEN 
           CALL ADVPHA( r( ir )              , A, ck, M )
        ELSE
           CALL ADVPHA( r( ir ) - r( ir - 1 ), A, ck, M ) 
        ENDIF

     ENDIF

     ! *** For each rcvr add up modal contributions ***

     DO ird = 1, Nrd              
        IF ( Opt(1:1) == 'R' .AND. r( ir ) /= 0.0 ) THEN
           P( ird, ir ) = SUM( A( 1:M ) * phi( 1:M, ird ) ) / SQRT( r( ir ) )
        ELSE
           P( ird, ir ) = SUM( A( 1:M ) * phi( 1:M, ird ) ) 
        ENDIF
     END DO

  END DO    ! next range step 

  RETURN 
END SUBROUTINE EVALCM
!**********************************************************************C
SUBROUTINE ADVPHA( DELTAR, A, ck, M ) 

  ! *** ADVance the PHAse of the coefficients ***                     

  COMPLEX, PARAMETER :: CI = ( 0.0, 1.0 ) 
  COMPLEX A( M ), ck( M )

  A = A * EXP( -CI * ck * DELTAR )

  RETURN 
END SUBROUTINE ADVPHA
!**********************************************************************C
SUBROUTINE NEWPRO( iProf, ck, phiR, MR, rd, Nrd, A ) 

  ! For a given profil number:                                        
  !     read in modes for current segment                             
  !     project the pressure field onto the new modes
  !     extract values of the modes at rcvr depths

  INTEGER, PARAMETER :: MaxM = 10000,  MaxN = 16001
  INTEGER    ird( Nrd ) 
  REAL       z(  MaxN ), rd( * ), W( Nrd )
  COMPLEX    P(  MaxN ), ck( * ), phiR( MaxM, * ), A( * ), sum1, &
       &           gamTL( MaxM ), gamBL( MaxM ), phiTL( MaxM ), phiBL( MaxM ),    &
       &           gamTR, gamBR, phiTR, phiBR, KTop2R, KBot2R, tail
  COMPLEX, ALLOCATABLE :: phi( : ), phiTmp( : )
  COMPLEX (KIND=8)     :: PEKRT, gamma2
  CHARACTER               BCBotR * 1, BCTopR * 1

  ! *** Compute pressure along the left of the interface ***          

  CALL PLEFT( iProf, A, ck, z, MR, P, Nr, NTot,                &
       &   BCTopR, rhoTR, KTop2R, depthTR, BCBotR, rhoBR, KBot2R, depthBR, &
       &   gamTL, gamBL, depthTL, depthBL, phiTL, phiBL, ML, MaxM )

  ! ***  Read in eigenfunctions and extract receiver values ***       

  ! Compute weights for mode interpolation at rcvr depths             

  CALL WEIGHT( z, NTot, rd, Nrd, W, ird )
  ALLOCATE( phi( Nr ), phiTmp( NTot ) )

  ModFil = 29 + iProf 

  DO mode = 1, MR
     READ( ModFil, REC = 6 + mode ) ( phi( iz ), iz = 1, Nr )

     IF ( BCTopR == 'A' ) THEN 
        phiTR = phi( 1 )
        gamma2 = ck( mode ) ** 2 - KTop2R
        gamTR = PEKRT( gamma2 )
     ENDIF

     IF ( BCBotR == 'A' ) THEN 
        phiBR = phi( Nr )
        gamma2 = ck( mode ) ** 2 - KBot2R
        gamBR = PEKRT( gamma2 )
     ENDIF

     ! tabulate the new mode on the grid from the previous segment


     DO iz = 1, NTot

        zT = z( iz ) 

        IF      ( zT > depthBR ) THEN
           IF ( BCBotR == 'A' ) phiTmp( iz ) = phiBR * EXP( -gamBR * ( zT - depthBR ) )
        ELSE IF ( zT < depthTR ) THEN
           IF ( BCTopR == 'A' ) phiTmp( iz ) = phiTR * EXP( -gamTR * ( depthTR - zT ) )
        ELSE
           phiTmp( iz ) = phi( iz )
        ENDIF

     END DO

     ! Compute new amplitudes:
     !       A = Integral[ P( z ) * phi( z ) dz ]
     !       (Repeat for each mode phi to produce excitation coef. A)
     !       Integral is done using trapezoidal rule

     sum1 = SUM( P( 1:NTot ) * phiTmp )
     !WRITE( *, * ) P( 1:10 )
     !WRITE( *, * ) phiTmp( 1:10 )

     IF ( BCTopR == 'A' ) THEN	! contribution from upper halfspace
        sum1   = sum1 + tail( z( 1    ), phiTL, gamTL, depthTL, ML, phiTR / rhoTR, gamTR, depthTR )
     ENDIF

     IF ( BCBotR == 'A' ) THEN      ! contribution from lower halfspace
        sum1   = sum1 + tail( z( NTot ), phiBL, gamBL, depthBL, ML, phiBR / rhoBR, gamBR, depthBR )
     ENDIF

     IF ( mode > ML ) A( mode ) = 0.0 
     WRITE( *, * ) mode, ABS( A( mode ) ), ABS( sum1 )

     A( mode ) = sum1

     ! *** Subtabulate modes at receiver depths ***                   

     DO ir = 1, Nrd 

        phiR( mode, ir ) = 0.0 
        IF      ( rd( ir ) < depthTR ) THEN
           !              --- Rcvr in upper halfspace
           IF ( BCTopR == 'A' ) phiR( mode, ir ) = phiTR * EXP( -gamTR * ( depthTR   - rd( ir  ) ) )
        ELSE IF ( rd( ir ) > depthBR ) THEN
           !              --- Rcvr in lower halfspace
           IF ( BCBotR == 'A' ) phiR( mode, ir ) = phiBR * EXP( -gamBR * ( rd( ir )  - depthBR   ) )
        ELSE
           iz = ird( ir ) 
           phiR( mode, ir ) = phi( iz ) + W( ir ) * ( phi( iz + 1 ) - phi( iz ) )
        ENDIF

     END DO

  END DO    ! next mode

  CLOSE( ModFil )

  WRITE( *, * ) 'depth-averaged power:', SUM( ABS( A( 1:MR ) ) ** 2 )

  RETURN 
END SUBROUTINE NEWPRO
!**********************************************************************C
SUBROUTINE PLEFT( iProf, A, ck, z, M, P, Nr, NTot,           &
     &   BCTop, rhoT, KTop2, depthT, BCBot, rhoB, KBot2, depthB,        &
     &   gamTL, gamBL, depthTL, depthBL, phiTL, phiBL, ML, MaxM )

  !     Computes the pressure field along the interface.                  
  !     Also returns information needed for the tails in the halfspaces   

  INTEGER, PARAMETER :: MaxN = 16001, Maxmed = 50
  INTEGER    N( Maxmed ) 
  REAL       zL( MaxN ), z( * ), rhoL( Maxmed ), rho( Maxmed ), depthL( Maxmed ), depth( Maxmed )
  COMPLEX    ck( * ),  P( * ), A( * ), gamTL( * ), gamBL( * ), phiTL( * ), phiBL( * ), KTop2, KBot2
  COMPLEX, ALLOCATABLE :: phi( : ), PL( : )
  COMPLEX (KIND=8) PEKRT, gamma2
  CHARACTER        Title * 80, Mater( Maxmed ) * 8, BCBot * 1, BCTop * 1, FileName * 8

  !     *** Read modal info at end of last segment ***                    

  CALL MODHDR( iProf - 1, FileName, Title, freq, Nmedia, NL, NMat, N, Mater, depthL, rhoL, &
       BCTop, rhoT, depthTL,  BCBot, rhoB, depthBL, ML, MaxM, zL, ck, KTop2, KBot2, ModFil )

  ML = M   ! we only know M amplitudes

  ! *** Compute pressure at the interface ***                         

  ALLOCATE( phi( NL ), PL( NL ) )
  PL = 0.0

  DO mode = 1, ML 
     READ( ModFil, REC = 6 + mode ) phi

     PL = PL + A( mode ) * phi

     ! Halfspace information

     phiTL( mode ) = A( mode ) * phi( 1  )   ;   phiBL( mode ) = A( mode ) * phi( NL )                       
     gamTL( mode ) = 0.0                     ;   gamBL( mode ) = 0.0

     ! we take the real part, gamma2, since leaky modes grow in depth
     IF ( BCTop == 'A' ) THEN
        gamma2 = DBLE( ck( mode ) ** 2 - KTop2 )
        gamTL( mode ) = PEKRT( gamma2 )
     END IF

     IF ( BCBot == 'A' ) THEN 
        gamma2 = ck( mode ) ** 2 - KBot2
        gamBL( mode ) = PEKRT( gamma2 )
     END IF

  END DO

  CLOSE( ModFil ) 

  ! *** Read modal data in new segment ***

  CALL MODHDR( iProf, FileName, Title, freq, Nmedia, Nr, NMat, N, Mater, depth, rho, &
       &   BCTop, rhoT, depthT, BCBot, rhoB, depthB, M, MaxM, z, ck, KTop2, KBot2, ModFil )

  IF ( z( 1 ) /= depthT .OR. z( Nr ) /= depthB ) THEN
     WRITE( *, * ) 'Fatal Error: modes must be tabulated throughout the ocean and sediment to compute the coupling coefficients'
     STop
  END IF

  ! *** Upslope? Extend the z vector with data from zL ***

  NTot = Nr 

  DO izL = 1, NL 
     IF ( zL( izL ) > z( NTot ) ) THEN
        NTot  = NTot + 1
        z( NTot ) = zL( izL )
     ENDIF
  END DO

  ! *** Retabulate the pressure on the new grid ***

  izL = 1 
  med = 1
  rhomed = rho( 1 )

  ! Depth of next interface below this one
  IF ( med < Nmedia ) THEN 
     DBelow = depth( med + 1 )
     rhoBel = rho(   med + 1 )
  ELSE
     DBelow = depthB 
     rhoBel = rhoB
  ENDIF

  DO iz = 1, NTot 
     zT = z( iz )

     ! Get medium density
     IF      ( zT < depthT ) THEN
        rhomed = rhoT
     ELSE IF ( zT > depthB ) THEN
        rhomed = rhoB
     ELSE IF ( med < Nmedia ) THEN
        IF   ( zT > DBelow ) THEN
           med    = med + 1
           rhomed = rho( med )
        ENDIF
     ENDIF

     ! depth of next interface Below this one
     IF ( med < Nmedia ) THEN
        DBelow = depth( med + 1 )   ;   rhoBel = rho( med + 1 )
     ELSE
        DBelow = depthB             ;   rhoBel = rhoB 
     ENDIF

     DO WHILE ( zT > zL( izL + 1 ) .AND. izL < NL - 1 ) 
        izL = izL + 1
     END DO

     ! Calculate P at that depth

     IF      ( zT > depthBL ) THEN       ! lower halfspace
        IF ( BCBot == 'A' ) THEN
           P( iz ) = SUM( phiBL(1:ML) * EXP( -gamBL(1:ML) * ( zT - depthBL ) ) )
        ENDIF

     ELSE IF ( zT < depthTL ) THEN       ! upper halfpace
        IF ( BCTop == 'A' ) THEN
           P( iz ) = SUM( phiTL(1:ML) * EXP( -gamTL(1:ML) * ( depthTL - zT ) ) )
        ENDIF
     ELSE 
        P( iz ) = PL( izL ) +                                       &
             ( zT - zL( izL ) ) / ( zL( izL + 1 ) - zL( izL ) ) *     &
             ( PL( izL + 1 ) - PL( izL ) )
     ENDIF

     ! *** compute mesh width, h ***

     IF ( iz == 1 ) THEN         ! first point
        h = 0.5 * ( z(  2   ) - z(   1      ) ) / rhomed

     ELSE IF ( iz == NTot ) THEN ! Last point
        h = 0.5 * ( z( NTot ) - z( NTot - 1 ) ) / rhomed

     ELSE                        ! Point just above or below the interface
        IF ( z( iz - 1 ) < DBelow .AND. z( iz + 1 ) > DBelow ) THEN                         

           h = 0.5 * ( z( iz + 1 ) / rhoBel - z( iz - 1 ) / rhomed &
                &                        - DBelow / rhoBel +      DBelow / rhomed )
        ELSE
           h = 0.5 * ( z( iz + 1 ) - z( iz   - 1 ) ) / rhomed
        ENDIF
     ENDIF

     P( iz ) = H * P( iz ) 

  END DO   ! next depth iz

  RETURN 
END SUBROUTINE PLEFT
!**********************************************************************C
COMPLEX FUNCTION tail( D, phiL, gamL, DL, ML, phiR, gamR, DR ) 

  COMPLEX gamL( ML ), phiL( ML ), gamR, phiR, FR

  FR =  phiR * EXP( -gamR * ( D - DR ) ) 

  IF ( D == DL ) THEN 
     tail = FR * SUM( phiL                             / ( gamL + gamR ) )
  ELSE
     tail = FR * SUM( phiL * EXP( -gamL * ( D - DL ) ) / ( gamL + gamR ) )
  ENDIF

  RETURN 
END FUNCTION tail
