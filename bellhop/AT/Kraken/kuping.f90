FUNCTION KUPING( SIGMA, ETA1SQ, RHO1, ETA2SQ, RHO2, P, U ) 

  ! Evalutates the imaginary perturbation due to interfacial roughness
  ! using the Kuperman-Ingenito formulation                           

  ! P is the pressure at the interface                                
  ! U is P'/RHO       "   "      "                                    

  REAL    (KIND=8  ) :: SIGMA, RHO1, RHO2
  COMPLEX (KIND=8)   :: KUPING, DEL, P, U, ETA1, ETA2, ETA1SQ, ETA2SQ, SCATRT, A11, A12, A21, A22
  COMPLEX, PARAMETER :: CI = (0.0, 1.0)

  KUPING = 0.0 
  IF ( SIGMA == 0.0 ) RETURN 

  ETA1 = SCATRT( ETA1SQ ) 
  ETA2 = SCATRT( ETA2SQ ) 

  DEL = RHO1 * ETA2 + RHO2 * ETA1 

  IF ( DEL /= 0.0 ) THEN 

     A11 = 0.5 * ( ETA1SQ - ETA2SQ ) - ( RHO2 * ETA1SQ - RHO1 * ETA2SQ ) * ( ETA1 + ETA2 ) / DEL
     A12 =  CI * ( RHO2 - RHO1 )**2 * ETA1 * ETA2 / DEL
     A21 = -CI * ( RHO2 * ETA1SQ - RHO1 * ETA2SQ )**2 / ( RHO1*RHO2*DEL)                        
     A22 = 0.5 * ( ETA1SQ - ETA2SQ ) + ( RHO2 - RHO1 ) * ETA1 * ETA2 * ( ETA1 + ETA2 ) / DEL

     KUPING = -SIGMA**2 * ( -A21 * P**2 + ( A11 - A22 ) * P * U + A12 * U**2 )
  ENDIF

  RETURN 
END FUNCTION KUPING

!**********************************************************************C

FUNCTION SCATRT( Z ) 

  ! Root for interfacial scatter                                      

  COMPLEX ( KIND=8 ) :: SCATRT, Z

  IF ( REAL( Z ) >= 0.0 ) THEN 
     SCATRT = SQRT( Z ) 
  ELSE 
     SCATRT = -( 0.0, 1.0 ) * SQRT( -Z ) 
  ENDIF

  RETURN 
END FUNCTION SCATRT
