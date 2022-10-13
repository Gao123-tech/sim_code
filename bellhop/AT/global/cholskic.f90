      SUBROUTINE CHOLESKI( A, NACT, N )

!     Factors A= L L* using Choleski's method
!     This only works if A is positive definite and hermitian

!     MICHAEL B. PORTER 12/25/92

!     NACT = ACTUAL ROW DIMENSION OF A
!     N    = SIZE OF THE SYSTEM

!     This implementation is not the most efficient
!     Some loops should be rearranged

      IMPLICIT NONE
      INTEGER I, J, K, NACT, N
      COMPLEX A( NACT, N ), T

! ******************************************************
! FORWARD ELIMINATION
! ******************************************************

      DO K = 1, N - 1

!        *** SUBTRACT MULTIPLE OF ROW K FROM ROWS K+1 TO N ***

         DO I = K + 1, N

            DO J = I, N
               A( I, J ) = A( I, J ) - CONJG( A( K, I ) ) * A( K, J ) / A( K, K )
            END DO

         END DO

!        *** Divide row K by sqrt of diagonal elt ***

         IF ( AIMAG( A( K, K ) ) /= 0.0 .OR. REAL( A( K, K ) ) <= 0.0 ) THEN

              WRITE( *, * ) 'Matrix not positive definite: Choleski fails'
              STOP
         ENDIF

         T = 1.0 / SQRT( A( K, K ) )

         DO J = K, N
            A( K, J ) = T * A( K, J )
         END DO
      END DO

!     -----------------------------

      IF ( AIMAG( A( N, N ) ) /= 0.0 .OR. REAL( A( N, N ) ) <= 0.0 ) THEN
           WRITE( *, * ) 'Matrix not suitable for Choleski decompostion'
!           STOP
      ENDIF

      A( N, N ) = SQRT( A( N, N ) )

      RETURN
      END

!----------------------------------------------------------------------C

      SUBROUTINE BACKSUB( A, B, NACT, N )

      IMPLICIT NONE
      INTEGER I, J, NACT, N
      COMPLEX A( NACT, N ), B( N ), T

! ******************************************************
! FORWARD-SUBSTITUTION
! ******************************************************

      B( 1 ) = B( 1 ) / A( 1, 1 )

      DO I = 2, N
         T = 0.0
         DO J = 1, I - 1
            T = T + CONJG( A( J, I ) ) * B( J )  ! a(i,j)=a(j,i)*
         END DO
         B( I  ) = ( B( I ) - T ) / A( I, I )
      END DO

! *******************************************
! BACK-SUBSTITUTION
! *******************************************

      B( N ) = B( N ) / A( N, N )

      DO I = N - 1, 1, -1
         T = 0.0
         DO J = I + 1, N
            T = T + A( I, J ) * B( J )
         END DO
         B( I )=( B( I ) - T ) / A( I, I )
      END DO

      RETURN
      END
