SUBROUTINE ZSECX( X2, TOL, IT, MAXIT, ERRMSG ) 

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  CHARACTER*80 ERRMSG 

  ! Secant method                                                     

  ERRMSG = ' ' 
  TEN = 10.0 
  X1 = X2 + 10.0 * TOL

  CALL FUNCT( X1, F1, IPOW1 )
  !WRITE( *, * )
  !WRITE( *, FMT="( 2G22.14, I5 )" ) SQRT( X1 ), F1, IPOW1

  DO IT = 1, MAXIT 
     X0    = X1   ;   F0    = F1 
     IPOW0 = IPOW1 
     X1    = X2 

     CALL FUNCT( X1, F1, IPOW1 ) 

     IF ( F1 == 0.0 ) THEN 
        SHIFT = 0.0 
     ELSE 
        SHIFT = ( X1 - X0 ) / ( 1.0 - F0 / F1 * TEN ** ( IPOW0 - IPOW1 ) )
     ENDIF

     X2 = X1 - SHIFT 

     !WRITE( *, FMT="( 2G22.14, I5 )" ) SQRT( X1 ), F1, IPOW1

     IF ( ABS( X2 - X1 ) < TOL .OR. ABS( X2 - X0 ) < TOL ) RETURN
  END DO

  ERRMSG = ' *** FAILURE TO CONVERGE IN SECANT' 

  RETURN 
END SUBROUTINE ZSECX
