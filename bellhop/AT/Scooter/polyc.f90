COMPLEX FUNCTION PC( x0, x, f, N, ErrorMessage)

  ! Polynomial approximant of order N at x0

  COMPLEX x0, x( N ), f( N ), ft( N ), h( N )
  CHARACTER*80 ErrorMessage

  ErrorMessage = '      '

  ! Initialize arrays
  h(  1:N ) = x( 1:N ) - x0
  ft( 1:N ) = f( 1:N )

  ! Recursion for solution

  IF ( N >= 2) THEN
     DO I = 1, N-1
        DO J = 1, N-I
           !         ft( J ) = ( h( J+I ) * ft( J ) - h( J ) * ft( J+1 ) ) / &
           !                                   ( h( J+I ) - h( J ) )
           ft( J ) = ft( J ) + h( J ) * ( ft( J ) - ft( J+1 ) ) / &
                &                             ( h( J+I ) - h( J ) )
        END DO
     END DO
  ENDIF

  PC = ft( 1 )

  RETURN
END FUNCTION PC
