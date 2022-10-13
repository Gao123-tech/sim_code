FUNCTION PEKRT( Z )

  ! At one time, this was used to return the 'Pekeris branch cut'     
  ! which is just a particular branch of the square root that         
  ! exposes many 'leaky' or 'virtual' modes.                          

  ! The current version implements a particular branch which was convenient                                                        

  COMPLEX (KIND=8) :: PEKRT, Z

  IF ( REAL( Z ) >= 0.0 ) THEN 
     PEKRT = SQRT( Z ) 
  ELSE 
     PEKRT = (0.0, 1.0) * SQRT( -Z ) 
  ENDIF

  RETURN

END FUNCTION PEKRT
