MODULE SdRdRMod
  ! Reads in source depths, receiver depths, and receiver ranges

  IMPLICIT NONE
  SAVE

  INTEGER NSD, NRD, NR
  INTEGER, ALLOCATABLE :: ISD( : ), IRD( : )
  REAL,    ALLOCATABLE ::  SD( : ),  RD( : ), WS( : ), WR( : ), R( : )

CONTAINS

  SUBROUTINE SDRD( ENVFIL, PRTFIL, ZMIN, ZMAX )

    IMPLICIT NONE
    INTEGER ENVFIL, PRTFIL, IS, IR, IAllocStat
    REAL ZMIN, ZMAX

    !     *** Read source depths ***

    READ( ENVFIL, * ) NSD

    IF ( NSD <= 0 ) THEN
       WRITE( PRTFIL, * ) ' NSD = ', NSD
       CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Number of sources must be positive'  )
    ENDIF

    IF ( ALLOCATED( SD ) ) DEALLOCATE( SD, WS, ISD )
    ALLOCATE( SD( MAX( 3, NSD ) ), WS( NSD ), ISD( NSD ), Stat = IAllocStat )
    IF ( IAllocStat /= 0 ) THEN
       WRITE( PRTFIL, * ) 'NSD = ', NSD
       CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Too many sources'  )
    END IF

    SD( 3 ) = -999.9
    READ( ENVFIL, * ) ( SD( IS ), IS = 1, NSD )

    CALL SUBTAB( SD, NSD )
    CALL SORT(   SD, NSD )

    WRITE( PRTFIL, * )
    WRITE( PRTFIL, * ) 'Number of sources   = ', NSD
    IF ( NSD >= 1  ) WRITE( PRTFIL, "( 5G14.6 )" ) ( SD( IS ), IS = 1, MIN( NSD, 51 ) )
    IF ( NSD > 51 ) WRITE( PRTFIL, * ) ' ... ', SD( NSD )

    ! *** Read receiver depths ***

    READ( ENVFIL, * ) NRD

    IF ( NRD <= 0 ) THEN
       WRITE( PRTFIL, * ) ' NRD = ', NRD
       CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Number of receivers must be positive'  )
    ENDIF

    IF ( ALLOCATED( RD ) ) DEALLOCATE( RD, WR, IRD )
    ALLOCATE( RD( MAX( 3, NRD ) ), WR( NRD ), IRD( NRD ), Stat = IAllocStat  )
    IF ( IAllocStat /= 0 ) THEN
       WRITE( PRTFIL, * ) 'NRD = ', NRD
       CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Too many receivers'  )
    END IF

    RD( 3 ) = -999.9
    READ( ENVFIL, * ) ( RD( IR ), IR = 1, NRD )

    CALL SUBTAB( RD, NRD )
    CALL SORT(   RD, NRD )

    WRITE( PRTFIL, * )
    WRITE( PRTFIL, * ) 'Number of receivers = ', NRD
    IF ( NRD >= 1 ) WRITE( PRTFIL, "( 5G14.6 )" ) ( RD( IR ), IR = 1, MIN( NRD, 51 ) )
    IF ( NRD > 51 ) WRITE( PRTFIL, * ) ' ... ', RD( NRD )

    ! *** Check for SD/RD in upper or lower halfspace ***

    DO IS = 1, NSD
       IF      ( SD( IS ) < ZMIN ) THEN
          SD( IS ) = ZMIN
          CALL ERROUT( PRTFil, 'W', 'SdRdRMod', 'Source above the top bdry has been moved down' ) 
       ELSE IF ( SD( IS ) > ZMAX ) THEN
          SD( IS ) = ZMAX
          CALL ERROUT( PRTFil, 'W', 'SdRdRMod', 'Source below the bottom bdry has been moved up' ) 
       ENDIF
    END DO

    DO IR = 1, NRD
       IF      ( RD( IR ) < ZMIN ) THEN
          RD( IR ) = ZMIN
       ELSE IF ( RD( IR ) > ZMAX ) THEN
          RD( IR ) = ZMAX
       ENDIF
    END DO

    RETURN
  END SUBROUTINE SDRD

  !********************************************************************

  SUBROUTINE RANGES( ENVFIL, PRTFIL )

    ! *** Read receiver ranges ***

    IMPLICIT NONE
    INTEGER ENVFIL, PRTFIL, IR, IAllocStat

    READ(  ENVFIL, * ) NR
    WRITE( PRTFIL, * )
    WRITE( PRTFIL, * ) 'Number of ranges   = ', NR

    IF ( ALLOCATED( R ) ) DEALLOCATE( R )
    ALLOCATE( R( MAX( 3, NR ) ), Stat = IAllocStat )
    IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFIL, 'F', 'RANGES', 'Too many range points' )

    R( 3 ) = -999.9
    READ( ENVFIL, * ) ( R( IR ), IR = 1, NR )

    CALL SUBTAB( R, NR )
    CALL SORT(   R, NR )

    WRITE( PRTFIL, "( 5G14.6 )" ) ( R( IR ), IR = 1, MIN( NR, 51 ) )
    IF ( NR > 51 ) WRITE( PRTFIL, * ) ' ... ', R( NR )

    R( 1:NR ) = 1000.0 * R( 1:NR )   ! Convert ranges to meters

    ! For a point source can't have receiver at origin
    ! IF ( OPT(1:1) == 'R' .AND. R( 1 ) <= 0.0 ) 

    !IF ( R( 1 ) <= 0.0 ) R( 1 ) = MIN( 1.0, R( 2 ) )

    RETURN
  END SUBROUTINE ranges

END MODULE SdRdRMod
