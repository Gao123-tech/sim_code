PROGRAM FIELD3D 

  ! Generate file of replica vectors
  ! Multiple receivers depths are handled inefficiently.

  USE SdRdRMod

  INTEGER, PARAMETER :: PRTFIL = 6,SHDFil = 25, MaxNT = 201, MaxElt = 20000, MaxNode = 9000, MaxM = 200, MaxSET = 9000

  ! Some of the above parameters also occur in readin below           
  ! if you change MaxM, change it in eval3d.f, evalpdq.f too!         

  INTEGER   Node( 3, MaxElt ), M( MaxSET )
  REAL      theta( MaxNT ), X( MaxNode ), Y( MaxNode )                                
  COMPLEX   CK( MaxM, MaxSET ), PhiR( MaxM, MaxSET ), PhiST( MaxM, 3 )                   
  CHARACTER OPT*50, TITLE*80, FILNAM( MaxNode )*50, TitleEnv*80, FileName*6

  INTEGER, ALLOCATABLE :: ADJElt( :, : ), ISET( : )
  COMPLEX, ALLOCATABLE :: P( :, : ), PhiS( :, :, : )

  ! *** Read in all the user input ***

  CALL READIN( TITLE, OPT, MLIMIT, XS, YS, RMIN, RMAX, theta, Ntheta, &
       X, Y, FILNAM, NNodes, Node, NElts )

  ALLOCATE( ADJElt( 3, NElts ), ISET( NNodes ), PhiS( MaxM, NSD, 3 ) )

  ! *** Build ADJElt and identify the source element ***

  CALL GETADJ( Node, NElts, ADJElt ) 
  IEltsC = ISCElt( XS, YS, Node, NElts, X, Y ) 

  ! Read modes at the source depths

  MSRC = MaxM 
  DO ICOR = 1, 3 
     INode = Node( ICOR, IEltsC ) 

     CALL GETMOD( 0, FILNAM( INode ), MaxM, SD, NSD, 'N', CK( 1, ICOR ), PhiS( 1, 1, ICOR ), M( ICOR ), freq, TitleEnv )
     MSRC = MIN( MSRC, M( ICOR ) ) 
  END DO

  ! *** Write header ***

  FileName = 'SHDFIL'
  IF ( NRD == 1 ) THEN 
     CALL WriteHeader( FileName, TITLE, SD, NSD, theta, Ntheta, R, NR, freq, ATTEN, 'POLAR', XS, YS, RD(1)    )
     ALLOCATE( P( Ntheta, NR ) ) 
  ELSE IF ( NSD == 1 ) THEN 
     CALL WriteHeader( FileName, TITLE, RD, NRD, theta, Ntheta, R, NR, freq, ATTEN, 'POLAR', XS, YS, SD(1)    )
     ALLOCATE( P( NRD, Ntheta ) ) 
  ELSE IF ( Ntheta == 1 ) THEN 
     CALL WriteHeader( FileName, TITLE, SD, NSD, RD,    NRD,    R, NR, freq, ATTEN, 'CARTE', XS, YS, theta(1) )
     ALLOCATE( P( NRD, NR ) ) 
  ENDIF

  ! ****** MAIN Loop: over receiver depth ******

  DO IR = 1, NRD 
     WRITE( *, * ) 'IR, RD( IR ) = ', IR, RD( IR ) 

     ! Get modes at the receiver depth
     CALL SLURP( NNodes, FILNAM, MaxSET, MaxM, RD( IR ), ISET, NSETS, M, CK, PhiR )                                  

     ! *** Loop over source depth ***

     DO IS = 1, NSD 
        WRITE( *, * ) 'IS, SD( IS ) = ', IS, SD( IS )
        PhiST( 1:MSRC, : ) = PhiS( 1:MSRC, IS, : ) ! Get modes at the source depth

        ! Call the appropriate routine to evaluate the field

        IF ( OPT(1:3) == 'STD' ) THEN 
           ! STANDARD (ignores hor. refraction)
           CALL EVAL3D( CK, PhiR, PhiST, M, IEltsC, ADJElt,         &
                ISET, X, Y, NNodes, Node, NElts, XS, YS,              &
                theta, Ntheta, RMIN, RMAX, NR, MLIMIT, OPT, P )       
        ELSE IF ( OPT(1:3) == 'PDQ' ) THEN 
           ! Quick (ignores and ignores)
           CALL EVALPDQ( CK, PhiR, PhiST, M, IEltsC, ADJElt,        &
                ISET, X, Y, NNodes, Node, NElts, XS, YS,              &
                theta, Ntheta, RMIN, RMAX, NR, MLIMIT, OPT, P )       
        ELSE IF ( OPT(1:3) == 'GBT' ) THEN 
           ! GAUSSIAN BEAM (include hor. refraction)
           CALL EVALGB( CK, PhiR, PhiST, M, MaxM, IEltsC, ADJElt,   &
                ISET, X, Y, NNodes, Node, NElts, XS, YS,              &
                theta, Ntheta, RMIN, RMAX, NR, MLIMIT, OPT, P )       
        ELSE 
           CALL ERROUT( PRTFIL, 'F', 'FIELD3D', 'Unknown option' ) 
        ENDIF

        ! Write out the field

        IF ( NRD == 1 ) THEN
           IREC = 6 + ( IS - 1 ) * Ntheta
           DO I = 1, Ntheta
              IRec = IRec + 1
              WRITE( SHDFil, REC = IRec ) ( P( I, J ), J = 1, Nr )
           END DO
        ELSE IF ( NSD == 1 ) THEN 
           IREC = 6 + ( IR - 1 ) * Ntheta
           DO I = 1, Ntheta
              IRec = IRec + 1
              WRITE( SHDFil, REC = IRec ) ( P( I, J ), J = 1, Nr )
           END DO
        ELSE IF ( Ntheta == 1 ) THEN 
           IREC = 6 + ( IS - 1 ) * NRD + (IR-1)
           IRec = IRec + 1
           WRITE( SHDFil, REC = IRec ) ( P( I, J ), J = 1, Nr )
        ENDIF

     END DO   ! Next source   depth 

  END DO   ! Next receiver depth

  STOP 
END PROGRAM FIELD3D
!**********************************************************************C
SUBROUTINE READIN( TITLE, OPT, MLIMIT, XS, YS,  &
     &   RMIN, RMAX, theta, Ntheta, X, Y, FILNAM, NNodes, Node, NElts )

  ! Reads in all the user input

  USE SdRdRMod

  INTEGER PRTFIL, FLPFIL 
  PARAMETER ( FLPFIL = 5, PRTFIL = 6, MaxNR  = 5001, MaxNRT  = 102000,  MaxNT = 201, MaxNode = 9000,   MaxM = 200 )

  INTEGER   Node( 3, * ) 
  REAL      theta( * ), X( * ), Y( * ) 
  CHARACTER OPT*50, TITLE*80, FILNAM( * )*50 

  ! *** TITLE, OPT, MLIMIT ***

  READ(  FLPFIL, * ) TITLE 
  WRITE( PRTFIL, * ) TITLE 

  READ(  FLPFIL, * ) OPT 
  WRITE( PRTFIL, * ) 'OPT = ', OPT

  READ(  FLPFIL, * ) MLIMIT 
  WRITE( PRTFIL, * ) 'Number of modes = ', MLIMIT 
  MLIMIT = MIN( MLIMIT, MaxM ) 

  ! *** Read source/receiver information ***

  READ(  FLPFIL, * ) XS, YS 
  WRITE( PRTFIL, * ) 'Coords. of source = ', XS, YS 

  XS = 1000.0 * XS   ;   YS = 1000.0 * YS 

  ! Read source/rcvr depths
  CALL SDRD( FLPFIL, PRTFIL, 0.0, 1.0E6 )

  ! Read receiver ranges ***
  CALL RANGES( FLPFIL, PRTFIL )

  RMIN = R(  1 )
  RMAX = R( NR )

  ! Read angles for radials
  theta( 3 ) = -999.9 
  READ( FLPFIL, * ) Ntheta
  READ( FLPFIL, * ) ( theta( ITH ), ITH = 1, Ntheta )
  CALL SUBTAB( theta, Ntheta ) 

  IF ( Ntheta > MaxNT ) THEN 
     WRITE( PRTFIL, * ) 'MaxNT = ', MaxNT 
     CALL ERROUT( PRTFIL, 'F', 'FIELD3D-READIN', 'Too many radials') 
  ENDIF

  IF ( NR * Ntheta > MaxNRT ) THEN 
     WRITE( PRTFIL, * ) 'MaxNRT = ', MaxNRT 
     CALL ERROUT( PRTFIL, 'F', 'FIELD3D-READIN', 'Too many receiver points' )
  ENDIF

  IF ( NSD /= 1 .AND. NRD /= 1 .AND. Ntheta /= 1 ) THEN 
     CALL ERROUT( PRTFIL, 'F', 'FIELD3D-READIN', 'Multiple sources, rcvrs, and bearings in a single run' )   
  END IF

  ! *** Read nodal coordinates ***

  READ(  FLPFIL, * ) NNodes 
  WRITE( PRTFIL, * ) 'NNodes = ', NNodes 

  IF ( NNodes > MaxNode ) THEN 
     WRITE( PRTFIL, * ) 'MaxNode = ', MaxNode 
     CALL ERROUT( PRTFIL, 'F', 'FIELD3D-READIN', 'Too many nodes' ) 
  ENDIF

  DO I = 1, NNodes 

     READ( FLPFIL, * ) X( I ), Y( I ), FILNAM( I ) 

     X( I ) = 1000.0 * X( I )   ;   Y( I ) = 1000.0 * Y( I )

     ! Append the extension '.MOD' to FILNAM
     JJ = INDEX( FILNAM( I ), ' ' ) 
     FILNAM( I )( JJ:JJ+3 ) = '.mod' 

  END DO

  ! *** Read in element definitions ***

  READ(  FLPFIL, * ) NElts 
  WRITE( PRTFIL, * ) 'NElts  = ', NElts 

  DO IElt = 1, NElts 
     READ( FLPFIL, * ) ( Node( ICOR, IElt ), ICOR=1, 3 ) 
  END DO

  IF ( OPT(4:4) == 'T' ) THEN 
     WRITE( PRTFIL, * ) 'Performing a Tesselation check' 
     CALL TESCHK( X, Y, Node, NElts, PRTFIL ) 
     WRITE( PRTFIL, * ) 'Passed the Tesselation check' 
  ENDIF

  RETURN 
END SUBROUTINE READIN
!**********************************************************************C
SUBROUTINE GETADJ( Node, NElts, ADJElt ) 

  ! Constructs a table ADJElt(IElt, iside) which gives the
  ! element number which shares iside with element number IElt

  INTEGER Node( 3, * ), ADJElt( 3, NElts ), ICOR( 3, 2 )
  DATA ((ICOR(iside, J), iside=1, 3), J=1, 2) /1, 2, 3, 2, 3, 1/ 

  ! ICOR maps a side (1, 2 or 3) and a local node (1 or 2) to a
  ! corner (1, 2, OR 3) of the triangle

  ADJElt = 0

  ! Loop over each triangle ***

  DO IElt = 1, NElts 

     ! Loop over triangle sides ***

     SIDE: DO iside = 1, 3 

        IF ( ADJElt( iside, IElt ) == 0 ) THEN 

           Node1 = Node( ICOR( iside, 1 ), IElt ) 
           Node2 = Node( ICOR( iside, 2 ), IElt ) 

           ! Search through other elements to find common side
           DO IEltT = 1, NElts 
              IF ( IEltT /= IElt ) THEN 
                 DO isideT = 1, 3 
                    Node1T = Node( ICOR( isideT, 1 ), IEltT ) 
                    Node2T = Node( ICOR( isideT, 2 ), IEltT ) 

                    ! Do IElt and IEltT share this side?
                    IF ( ( Node1 == Node1T .AND. Node2 == Node2T ) .OR. &
                       & ( Node1 == Node2T .AND. Node2 == Node1T ) ) THEN           
                       ADJElt( iside,  IElt  ) = IEltT 
                       ADJElt( isideT, IEltT ) = IElt 
                       CYCLE SIDE
                    ENDIF

                 END DO   ! next side
              ENDIF
           END DO   ! next element
        ENDIF

     END DO SIDE   ! next side

  END DO   ! next element

  RETURN 
END SUBROUTINE GETADJ
!**********************************************************************C
FUNCTION ISCELT( XS, YS, Node, NElts, X, Y ) 

  !     Identifies the element containing the source at (XS, YS)          

  !     We define a function ENCL( XS, YS ) which is 1 when (XS , YS)     
  !     is inside a given triangle and decreases from 1 the further the   
  !     source moves away from the triangle.                              

  !     The element with the highest value of ENCL is identified as the so
  !     element.                                                          

  !     If several elements enclose, the highest numbered                 
  !     element is given posession                                        

  INTEGER Node( 3, * ) 
  REAL    X( * ), Y( * ) 

  ENCLMax = 0.0 

  DO IElt = 1, NElts 
     Node1 = Node( 1, IElt ) ; Node2 = Node( 2, IElt ) 
     Node3 = Node( 3, IElt ) 

     X1 = X( Node1 )   ;   Y1 = Y( Node1 ) 
     X2 = X( Node2 )   ;   Y2 = Y( Node2 ) 
     X3 = X( Node3 )   ;   Y3 = Y( Node3 ) 

     ! Compute areas of triangles
     DEltA = ( X2*Y3 - Y2*X3 ) - ( X1*Y3 - Y1*X3 ) + ( X1*Y2 - Y1*X2 )
     A1    = ( X2*Y3 - Y2*X3 ) - ( XS*Y3 - YS*X3 ) + ( XS*Y2 - YS*X2 )
     A2    = ( XS*Y3 - YS*X3 ) - ( X1*Y3 - Y1*X3 ) + ( X1*YS - Y1*XS )
     A3    = ( X2*YS - Y2*XS ) - ( X1*YS - Y1*XS ) + ( X1*Y2 - Y1*X2 )

     ENCL = ABS( DEltA ) / ( ABS( A1 ) + ABS( A2 ) + ABS( A3 ) ) 

     IF ( ENCL > ENCLMax ) THEN 
        ISCElt = IElt 
        ENCLMax = ENCL 
     ENDIF

  END DO

  RETURN 
END FUNCTION ISCELT
!**********************************************************************C
SUBROUTINE TESCHK( X, Y, Node, NElts, PRTFIL ) 

  !     Checks to see that triangulation is a tesselation                 
  !     that is, that there are no overlapping triangles                  
  !     (holes or triangles inside triangles are still possible)          

  INTEGER Node( 3, * ), ICOR( 3, 2 ), PRTFIL 
  REAL    X( * ), Y( * ) 

  DATA ((ICOR(iside, J), iside=1, 3), J=1, 2) /1, 2, 3, 2, 3, 1/ 

  ! ICOR maps a side (1, 2 or 3) and a local node (1 or 2) to a
  ! corner (1, 2, OR 3) of the triangle

  DO IElt1 = 1, NElts-1 
     DO iside1 = 1, 3 
        X1 = X( Node( ICOR( iside1, 1 ), IElt1 ) ) 
        Y1 = Y( Node( ICOR( iside1, 1 ), IElt1 ) ) 
        X2 = X( Node( ICOR( iside1, 2 ), IElt1 ) ) 
        Y2 = Y( Node( ICOR( iside1, 2 ), IElt1 ) ) 
        UX = X2 - X1   ;   UY = Y2 - Y1 

        DO IElt2 = IElt1 + 1, NElts 
           DO iside2 = 1, 3 
              X3 = X( Node( ICOR( iside2, 1 ), IElt2 ) ) 
              Y3 = Y( Node( ICOR( iside2, 1 ), IElt2 ) ) 
              X4 = X( Node( ICOR( iside2, 2 ), IElt2 ) ) 
              Y4 = Y( Node( ICOR( iside2, 2 ), IElt2 ) ) 
              VX = X4 - X3   ;    VY = Y4 - Y3 
              WX = X4 - X2   ;    WY = Y4 - Y2 
              DEL = UX * VY - UY * VX 

              IF ( ABS( DEL ) > MAX( ABS( UX*UY ), ABS( VX*VY ) ) / 10000.0 ) THEN 
                 S1 = ( UX * WY - UY * WX ) / DEL 
                 S2 = ( VX * WY - VY * WX ) / DEL 
                 IF ( 0.001 < S1 .AND. S1 < 0.999 .AND.       &
                      &                    0.001 < S2 .AND. S2 < 0.999 ) THEN      
                    WRITE( PRTFIL, * ) 'Sides cross for elts = ', IElt1, IElt2      
                    WRITE( PRTFIL, * ) 'SIDE 1:' 
                    WRITE( PRTFIL, * ) '   (', X1, Y1, ')' 
                    WRITE( PRTFIL, * ) '   (', X2, Y2, ')' 
                    WRITE( PRTFIL, * ) '   S = ', S1 

                    WRITE( PRTFIL, * ) 'SIDE 2:' 
                    WRITE( PRTFIL, * ) '   (', X3, Y3, ')' 
                    WRITE( PRTFIL, * ) '   (', X4, Y4, ')' 
                    WRITE( PRTFIL, * ) '   S = ', S2 
                    STOP 
                 ENDIF
              ENDIF

           END DO
        END DO

     END DO
  END DO

  RETURN 
END SUBROUTINE TESCHK
!**********************************************************************C
SUBROUTINE SLURP( NNodes, FILNAM, MaxSET, MaxM, RD, ISET, NSETS, M, CK, PhiR )                                     

  ! Reads in the values of the modes at the given receiver depth
  ! for every node in the triangulation

  INTEGER, PARAMETER :: PRTFIL = 6 
  INTEGER   M( * ), ISET( * ) 
  COMPLEX   CK( MaxM, * ), PhiR( MaxM, * ) 
  CHARACTER FILNAM( * )*50, TitleEnv*80 

  NSETS = 0 

  Node: DO INode = 1, NNodes 

     ! Check if the modes have already been read
     IF ( INode >= 2 ) THEN 
        DO JNode = 1, INode-1 
           IF ( FILNAM( INode ) == FILNAM( JNode ) ) THEN 
              ! Copy previously read modes
              ISET( INode ) = ISET( JNode )  
              CYCLE Node  ! process next node 
           ENDIF
        END DO
     ENDIF

     NSETS = NSETS + 1 
     IF ( NSETS > MaxSET ) THEN 
        WRITE( PRTFIL, * ) 'MaxSET = ', MaxSET 
        CALL ERROUT( PRTFIL, 'F', 'FIELD3D-SLURP', 'Too many mode sets' )               
     ENDIF
     ISET( INode ) = NSETS 

     ! Check for 'DUMMY' elts (acoustic absorbers)
     IF ( FILNAM( INode )(1:5) == 'DUMMY' ) THEN 
        M( NSETS ) = 0 
     ELSE 
        ! Necessary to read in modes
        WRITE( *, * ) 'Reading INode', INode 
        CALL GETMOD( 0, FILNAM( INode ), MaxM, RD, 1, 'N', CK( 1, NSETS ), PhiR( 1, NSETS ), M( NSETS ), freq, TitleEnv )
     ENDIF

  ENDDO Node

  RETURN 
END SUBROUTINE SLURP
