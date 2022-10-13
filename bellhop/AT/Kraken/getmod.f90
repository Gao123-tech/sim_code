SUBROUTINE GETMOD( IProf, FileName, MaxM, RD, NRD, Comp, CK, PhiR, M, Freq, Title )                                     

  ! Read in modes and extract values at rcvr depths                   

  ! INPUT:                                                            
  !    IProf    unit number  for the mode set
  !    FileName name of file for the mode set
  !    MaxM     row dimension of PhiR in calling program
  !    RD       vector of receiver depths where modes are to be evaluate
  !    NRD      number of such receivers
  !    Comp     component (vertical, horizontal,...) for elastic problem
  !                 ignored for purely acoustic problems

  ! OUTPUT:                                                           
  !    CK     vector of eigenvalues                                   
  !    PhiR   matrix of tabulated modes                               
  !    M      number of modes
  !    Freq   frequency                                               
  !    Title  title in mode file

  ! If IProf <> 0                                                     
  !    we assume the mode file is assigned to unit
  !    29 + IProf and no open is necessary                            
  ! else
  !    we open a file with name FileName to read the modes              
 
  INTEGER, PARAMETER :: PRTFIL = 6, MaxN = 16001, MaxMed = 51
  INTEGER   N( MaxMed ), IRD( NRD )
  REAL      RD( * ), Z( MaxN ), W( NRD ), Depth( MaxMed ), Rho( MaxMed )
  COMPLEX   PhiR( MaxM, * ), PhiT( MaxN ), CK( * ), KTop2, KBot2
  CHARACTER Title*( * ), FileName*( * ), Mater( MaxMed )*8, Comp*( *), BCTop*1, BCBot*1

  ! *** Read the header data from the mode file ***

  CALL MODHDR( IProf, FileName, Title, Freq, NMedia, NTot, NMat, N, Mater, Depth, Rho,  &
       BCTop, RhoT, DepthT, BCBot, RhoB, DepthB, M, MaxM, Z, CK, KTop2, KBot2, ModFil )

  IF ( M <= 0 ) RETURN  ! no modes? quick return.

  CALL WEIGHT( Z, NTot, RD, NRD, W, IRD )   ! *** Locate indices of receiver points ***

  ! *** Loop over receiver depths to check for safe interpolation ***

  ! Receivers must be within a fraction of a wavelength
  ! of tabulated pts. We accept one wavelength at 1500 m/s

  Tol = 1500.0 / Freq 

  DO IR = 1, NRD 

     IZ = IRD( IR ) 
     WT = ABS( MIN( W( IR ), 1.0 - W( IR ) ) ) 

     IF ( RD( IR ) < DepthT ) THEN        ! Rcvr in upper halfspace

        CST = 0.0   ! should be passed by MODHDR
        IF ( CST /= 0.0 .OR. BCTop(1:1) /= 'A' ) THEN 
           WRITE( PRTFIL, * ) 'Receiver depth: ', RD( IR )
           WRITE( PRTFIL, * ) 'Highest valid depth: ', DepthT
           CALL ERROUT( PRTFIL, 'F', 'GETMOD', 'Rcvr above highest valid depth' )
        ENDIF

     ELSE IF ( RD( IR ) > DepthB ) THEN   ! Rcvr in lower halfspace

        CSB = 0.0   ! should be passed by MODHDR
        IF ( CSB /= 0.0 .OR. BCBot(1:1) /= 'A' ) THEN 
           WRITE( PRTFIL, * ) 'Receiver depth: ', RD( IR )
           WRITE( PRTFIL, * ) 'Lowest valid depth: ', DepthB 
           CALL ERROUT( PRTFIL, 'F', 'GETMOD', 'Rcvr below lowest valid depth' )
        ENDIF

     ELSE IF ( NTot > 1 ) THEN            ! Rcvr between two grid points
        IF ( WT * ( Z( IZ + 1 ) - Z( IZ ) ) > Tol ) THEN
           WRITE( PRTFIL, * ) 'Nearest depths: ', Z( IZ ), Z( IZ + 1 )
           WRITE( PRTFIL, * ) 'Tolerance: ', Tol 
           CALL ERROUT( PRTFIL, 'F', 'GETMOD', 'Modes not tabulated near requested pt.' )
        ENDIF
     ELSE                                 ! Rcvr near a single grid point
        IF ( ABS( RD( IR ) - Z( IZ ) ) > Tol ) THEN 
           WRITE( PRTFIL, * ) 'Rd, Tabulation depth ', RD( IR), Z( IZ )
           WRITE( PRTFIL, * ) 'Tolerance: ', Tol 
           CALL ERROUT( PRTFIL, 'F', 'GETMOD', 'Modes not tabulated near requested pt.' )
        ENDIF
     ENDIF

  END DO   ! next receiver depth

  ! *** Read in the modes ***

  DO Mode = 1, M 

     CALL GETONE( Mode, ModFil, NTot, NMat, W, IRD, N, Mater, NMedia, Comp, &
          KTop2, DepthT, BCTop, KBot2, DepthB, BCBot, RD, NRD, CK, PhiT )

     PhiR( Mode, 1:NRD ) = PhiT( 1:NRD )

  END DO   ! next mode

  CLOSE( ModFil ) 

  RETURN 
END SUBROUTINE GETMOD

!**********************************************************************C

SUBROUTINE MODHDR( IProf, FileName, Title, Freq, NMedia, NTot, NMat, N, Mater, Depth, Rho, &
     &   BCTop, RhoT, DepthT, BCBot, RhoB, DepthB, M, MaxM, Z, CK, KTop2, KBot2, ModFil )

  ! Reads the header information from ModFil                          
  ! Note T suffix means top
  !      B suffix means bottom                                        

  ! The mode file may be specified either by a profile number (IProf) 
  !    or a name FileName
  ! If IProf =  0 then we assume you provide FileName
  ! If IProf <> 0 then a file name of the form ModFilxxx is built

  ! FileName is the user-provided file name                             
  ! FileNameT is the temporary name we build
  ! These have to be two separate variables do ensure there
  ! is space allocated to construct the file name even when           
  ! the user calls us with FileName = ' '                               


  INTEGER, PARAMETER :: PRTFIL = 6 
  REAL,    PARAMETER :: PI = 3.141592
  INTEGER   N( * ) 
  REAL      Depth( * ), Z( * ), Rho( * ) 
  COMPLEX   CK( * ), CPT, CST, CPB, CSB, KTop2, KBot2
  CHARACTER Title*( * ), FileName*( * ), Mater( * )*8, BCTop*1, BCBot*1, FileNameT*80

  ! *** open ModFil ***

  IF ( IProf == 0 ) THEN 
     ModFil  = 30
     FileNameT = FileName
  ELSE
     ModFil = 29 + IProf
     WRITE( FileNameT, FMT = "( 'MODFIL', I4.4 )" ) IProf
  ENDIF

  !INQUIRE( FILE = FileNameT, RECL = LRecL )
  OPEN( UNIT = ModFil, FILE = FileNameT, STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED', &
       RECL = 100, IOSTAT = iostat )
  IF ( IOSTAT /= 0 ) CALL ERROUT( PrtFil, 'F', 'MODHDR', 'Unable to open the mode file' )

  READ( ModFil, REC = 1 ) LRecL
  CLOSE( UNIT = ModFil )
  OPEN( UNIT = ModFil, FILE = FileNameT, STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED', &
       RECL = 4 * LRecL, IOSTAT = iostat )

  ! *** Read header info ***

  READ( ModFil, REC = 1 ) LRecL, Title(1:80), Freq, NMedia, NTot, NMat                  
  READ( ModFil, REC = 2 ) ( N( Med ), Mater( Med ), Med = 1, NMedia)
  READ( ModFil, REC = 3 ) BCTop(1:1), CPT, CST, RhoT, DepthT, BCBot(1:1), CPB, CSB, RhoB, DepthB
  READ( ModFil, REC = 4 ) ( Depth( Med ), Rho( Med ), Med = 1, NMedia )
  READ( ModFil, REC = 5 ) M, LRecL

  IF ( M > MaxM ) THEN 
     WRITE( PRTFIL, * ) 'M = ', M, '   MaxM = ', MaxM
     CALL ERROUT( PRTFIL, 'F', 'MODHDR', 'Insufficient storage to read all the modes: increase MaxM' )
  ENDIF

  IF ( M == 0 ) RETURN 
  READ( ModFil, REC = 6 ) ( Z(   IZ ), IZ = 1, NTot ) 

  ! *** Read in eigenvalues, CK( I ) ***

  IFirst = 1 
  DO IREC = 1, 1 + ( 2 * M - 1 ) / LRecL
     ILast  = MIN( M, IFirst + LRecL / 2 - 1 )

     READ( ModFil, REC = 6 + M + IREC ) ( CK( Mode ), Mode = IFirst, ILast )      

     IFirst = ILast + 1 
  END DO

  IF ( BCTop(1:1) == 'A' ) KTop2 = ( 2.0 * PI * Freq / CPT ) **2 
  IF ( BCBot(1:1) == 'A' ) KBot2 = ( 2.0 * PI * Freq / CPB ) **2

  RETURN 
END SUBROUTINE MODHDR

!**********************************************************************C

SUBROUTINE GETONE( Mode, ModFil, NTot, NMat, W, IRD, N, Mater, NMedia, Comp, &
     &   KTop2, DepthT, BCTop, KBot2, DepthB, BCBot, RD, NRD, CK, PhiR )

  ! Read in a single eigenfunction and extract receiver values
  ! Results are returned in PhiR

  LOGICAL    TufLuk 
  INTEGER    N( * ), IRD( * )
  REAL       RD( * ), W( * ) 
  COMPLEX    PhiR( * ), CK( * ), Phi( NMat ), GamT, GamB, KTop2, KBot2
  COMPLEX (KIND=8) :: PEKRT, Gamma2
  CHARACTER  Mater( * )*8, Comp*( *), BCTop*1, BCBot*1

  READ( ModFil, REC = 6 + Mode ) ( Phi( J ), J = 1, NMat ) 

  ! *** Is there an elastic medium in the problem? ***

  TufLuk = .FALSE. 
  IF ( ANY( Mater( 1:NMedia ) == 'ELASTIC' ) ) TufLuk = .TRUE.

  ! Extract the component specified by 'Comp'

  IF ( TufLuk ) CALL EXTRACT( Phi, N, Mater, NMedia, Comp ) 

  ! *** Extract values at receiver depths ***

  GamT = 0.0
  GamB = 0.0 

  ! n.b. should be using real( ck(mode)) for KRAKEN
  IF ( BCTop(1:1) == 'A' ) THEN 
     Gamma2 = CK( Mode ) ** 2 - KTop2
     GamT = PEKRT( Gamma2 )
  END IF

  IF ( BCBot(1:1) == 'A' ) THEN 
     Gamma2 = CK( Mode ) ** 2 - KBot2
     GamB = PEKRT( Gamma2 )
  END IF

  DO IR = 1, NRD 

     IF ( RD( IR ) < DepthT ) THEN      ! Rcvr in upper halfspace
        PhiR( IR ) = Phi( 1    ) * EXP( -GamT * ( DepthT - RD( IR  ) ) )

     ELSE IF ( RD( IR ) > DepthB ) THEN ! Rcvr in lower halfspace
        PhiR( IR ) = Phi( NTot ) * EXP( -GamB * ( RD( IR ) - DepthB ) )

     ELSE IF ( NTot > 1 ) THEN 
        IZ = IRD( IR )
        PhiR( IR ) = Phi( IZ ) + W( IR ) * ( Phi( IZ + 1 ) - Phi( IZ ) )

     ELSE                               ! mode is tabulated at only one depth
        IZ = IRD( IR ) 
        PhiR( IR ) = Phi( IZ ) 
     ENDIF

  END DO

  RETURN 
END SUBROUTINE GETONE
!**********************************************************************C
SUBROUTINE EXTRACT( Phi, N, Mater, NMedia, Comp ) 

  ! For elastic problems where a stress-displacement vector is output,
  ! extracts the desired component                                    

  INTEGER   N( * ) 
  COMPLEX   Phi( * ) 
  CHARACTER Mater( * )*8, Comp*( *)

  J = 1 
  K = 1 

  DO Med = 1, NMedia 

     DO I = 1, N( Med ) + 1
        SELECT CASE ( Mater( Med ) )
        CASE ( 'ACOUSTIC' )
           Phi( J ) = Phi( K )
           K = K + 1
        CASE ( 'ELASTIC' )
           SELECT CASE ( Comp )
           CASE ( 'H' )
              Phi( J ) = Phi( K     )
           CASE ( 'V' )
              Phi( J ) = Phi( K + 1 )
           CASE ( 'T' )
              Phi( J ) = Phi( K + 2 )
           CASE ( 'N' )
              Phi( J ) = Phi( K + 3 )
           END SELECT

           K = K + 4
        END SELECT
        J = J + 1 
     END DO

  END DO

  RETURN 
END SUBROUTINE EXTRACT
