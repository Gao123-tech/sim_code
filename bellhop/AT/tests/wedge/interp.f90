      PROGRAM INTERP

C     Interpolates to produce the intermediate profiles for slope.

      PARAMETER ( NR = 51, RSTART =   0.0, REND = 4.0, DSTART = 200.0, DEND = 0.0 )

      DELR = ( REND - RSTART ) / ( NR - 1 )
      DELD = ( DEND - DSTART ) / ( NR - 1 )

      DO IR = 1, NR

         R    = RSTART + ( IR - 1 ) * DELR
         D    = DSTART + ( IR - 1 ) * DELD

         WRITE( *, * ) 'sedslope', IR, MAX( 1.0, D )

      END DO

      STOP
      END
