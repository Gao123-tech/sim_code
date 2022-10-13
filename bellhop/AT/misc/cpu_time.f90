!     Last change:  MBP  17 Jun 2002   12:29 pm

SUBROUTINE CPU_TIME( T )

! Read the time

COMMON /TIMDATA/ ITICKS
REAL ( KIND=8 ), INTENT( OUT ) :: T

CALL SYSTEM_CLOCK( ITICKS, ICR )

T = REAL ( ITICKS ) / REAL( ICR )

RETURN
END
