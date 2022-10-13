SUBROUTINE ZSECCX( X2, TOL, IT, MAXIT, ERRMSG ) 
  IMPLICIT REAL ( KIND=8 ) (A-H, O-Z)
  COMPLEX ( KIND=8 ) :: X0, X1, X2, SHIFT, F0, F1, CNUM, CDEN
  CHARACTER*80 ERRMSG 

  ! Secant method                                                     

  ERRMSG = ' ' 
  IF ( TOL <= 0.0 ) THEN 
     ERRMSG = 'Non-positive tolerance specified' 
     STOP 
  ENDIF

  TEN    = 10.0 
  X1     = X2 + 100.0 * TOL 
  CALL FUNCT( X1, F1, IPOW1 ) 

  !WRITE( *, * )
  !WRITE( *, "( 4G17.9, I5 )" ) SQRT( X1 ), F1, IPOW1

  DO IT = 1, MAXIT 
     X0 = X1   ;   F0 = F1 
     IPOW0 = IPOW1 

     X1 = X2 
     CALL FUNCT( X1, F1, IPOW1 )

     ! ugly stuff to block overflows by forcing shift to be bounded
     CNUM = F1 * ( X1 - X0 ) 
     CDEN = F1 - F0 * TEN ** ( IPOW0 - IPOW1 )

     IF ( ABS( CNUM ) >= ABS( CDEN * X1 ) ) THEN 
        SHIFT = 100.0 * TOL * ABS( X1 ) 
     ELSE 
        SHIFT = CNUM / CDEN 
     ENDIF

     X2 = X1 - SHIFT 
     !WRITE( *, "( 4G17.9, I5 )" ) SQRT( X1 ), F1, IPOW1

     IF ( ABS( X2 - X1 ) + ABS( X2 - X0 ) < TOL ) RETURN                       
  ENDDO

  ERRMSG = ' *** FAILURE TO CONVERGE IN SECANT' 

  RETURN 
END SUBROUTINE ZSECCX
