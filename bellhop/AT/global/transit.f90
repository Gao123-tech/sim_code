      FUNCTION TRANSIT( X, SIGMA )

!     Computes a value on the transition curve

      PARAMETER ( N = 500 )

      LOGICAL INIT
      REAL    A( N )
      SAVE INIT, A, XMIN, XMAX, DELTAX
      DATA INIT /.FALSE./

      IF ( .NOT. INIT ) THEN

!        *** Initialize ***

         XMAX   =  4.0 * SIGMA
         XMIN   = -4.0 * SIGMA
         DELTAX = ( XMAX - XMIN ) / ( N - 1 )

         A( 1 ) = 0.0
         DO I = 2, N
            XT     = XMIN + ( I - 1 ) * DELTAX
            A( I ) = A( I - 1 ) + DELTAX * GAUSS( XT, SIGMA )
         END DO

         INIT = .TRUE.
      ENDIF

!     *** Evaluate distribution at x ***

      IF      ( X <= XMIN ) THEN
         TRANSIT = 0.0
      ELSE IF ( X >= XMAX ) THEN
         TRANSIT = 1.0
      ELSE
         I = ( X - XMIN ) / DELTAX + 1
         TRANSIT = A( I )
      ENDIF

      RETURN
      END

!**********************************************************************!

      FUNCTION GAUSS( X, SIGMA )

!     NORMAL DISTRIBUTION

      PARAMETER ( PI = 3.141592 )

      IF      ( SIGMA > 0.01 ) THEN
         GAUSS = EXP( -( X / SIGMA ) ** 2 / 2.0 ) / ( SIGMA * SQRT( 2.0 * PI ) )
      ELSE IF ( X > 0.0      ) THEN
         GAUSS = 1.0
      ELSE
         GAUSS = 0.0
      ENDIF

      RETURN
      END
