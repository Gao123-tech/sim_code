SUBROUTINE FACTOR( N, A, IERR, RV, RVU )

  ! MICHAEL B. PORTER 7/1/85

  ! THIS SUBROUTINE IS BASED ON TINVIT IN EISPACK
  ! USES GAUSSIAN ELIMINATION WITH PIVOTING TO FACTOR A
  ! SYMMETRIC TRIDIAGONAL LINEAR SYSTEM

  !  ON INPUT
  !     N IS THE ORDER OF THE MATRIX.
  !     A CONTAINS THE INPUT MATRIX.

  !  ON OUTPUT
  !     RV1, RV2, RV3 AND RV4 CONTAIN THE FACTORED MATRIX

  COMPLEX U, XU, A(2, N), RV(2, N), RVU(2, N)

  ! LU DECOMPOSITION WITHOUT INTERCHANGES

  XU = 1.0
  U = A(1,1)

  IF ( N > 1 ) THEN
     DO I = 2, N-1
        XU = A( 2, I ) / U
        RV( 2, I   ) = XU
        RVU( 1, I-1 ) = 1.0 / U
        RVU( 2, I-1 ) = A( 2, I )
        U = A( 1, I   ) - XU * A( 2, I )
     END DO
  ENDIF

  XU = A( 2, N ) / U
  RV( 2, N   ) = XU
  RVU( 1, N-1 ) = 1.0 / U
  RVU( 2, N-1 ) = A( 2, N )
  U = A( 1, N ) - XU * A( 2, N )

  IF( U == 0.0 ) WRITE( *, * ) 'SINGULAR MATRIX'
  RVU(1,N) = 1.0 / U
  RVU(2,N) = 0.0

  RETURN
END SUBROUTINE FACTOR

SUBROUTINE BACKSB(N,A,IERR,RV,RVU,B)

  !  MICHAEL B. PORTER 7/1/85

  !  THIS SUBROUTINE IS BASED ON TINVIT IN EISPACK
  !  PERFORMS BACK-SUBSTITUTION FOR A
  !     SYMMETRIC TRIDIAGONAL LINEAR SYSTEM

  !  ON INPUT

  !     N IS THE ORDER OF THE MATRIX.
  !     D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
  !     E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
  !       IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
  !     B CONTAINS THE RIGHT HAND SIDE (AX=B)

  !  ON OUTPUT

  !     ALL INPUT ARRAYS ARE UNALTERED EXCEPT
  !     B WHICH CONTAINS THE SOLUTION

  !  RV IS A TEMPORARY STORAGE ARRAY.

  COMPLEX U, V, A(2,N), RV(2,N), RVU(2,N), B(N)

  ! *** FORWARD ELIMINATION ***

  DO I = 2, N
     B( I ) = B( I ) - RV( 2, I ) * B( I-1 )
  END DO

  ! *** BACK SUBSTITUTION (RESULT IN B) ***

  B( N ) = B( N ) * RV( 1, N )

  IF ( N > 1 ) THEN
     V = RVU( 2, N-1 ) * B( N )
     DO I = N-1, 1, -1


        !        B( I ) = ( B( I ) - B( I + 1 ) * RVU( 2, I ) ) * RVU( 1, I )
        B( I ) = ( B( I ) - B( I + 1 ) * RVU( 2, I ) )
        !         B( I ) = ( B( I ) - V ) * RVU( 1, I )
        !         B( I ) = ( RVU( 2, I ) - V ) * RVU( 1, I )
        !         V = RVU( 2, I - 1 ) * B( I )

     END DO
  ENDIF

  RETURN
END SUBROUTINE BACKSB
