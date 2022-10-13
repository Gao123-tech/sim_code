!     Last change:  MBP  22 Jul 2000   12:49 pm
      PROGRAM MODASC 
                                                                        
!     Converts mode file to ascii format                                
                                                                        
      INTEGER BINFIL, ASCFIL 
      PARAMETER ( BINFIL = 20, ASCFIL = 6 )

      COMPLEX   CPT, CST, CPB, CSB 
      CHARACTER TITLE*( 80 ), BCTOP*1, BCBOT*1

      INTEGER,   ALLOCATABLE :: N( : )
      REAL,      ALLOCATABLE :: Z(: ), RHO( : ), DEPTH( : )
      COMPLEX,   ALLOCATABLE :: PHI( : ), CK( : )
      CHARACTER, ALLOCATABLE :: MATER( : )*8 
                                                                        
!     *** Read header ***                                               
                                                                        
      !INQUIRE( FILE = FILNAMT, RECL = LRECL )
      OPEN( UNIT = BINFIL, FILE = 'MODFIL', STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED', RECL = 100 )
      READ( BINFIL, REC = 1 ) LRECL
      CLOSE( UNIT = BINFIL )
      OPEN( UNIT = BINFIL, FILE = 'MODFIL', STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED', RECL = 4 * LRECL )
                                                                        
      READ( BINFIL, REC = 1 ) LRECL, TITLE(1:80), FREQ, NMEDIA, NTOT, NMAT

      ALLOCATE( N( NMEDIA), MATER( NMEDIA), DEPTH( NMEDIA ), RHO( NMEDIA ), Z( NTOT ), PHI( NTOT ) )

      READ( BINFIL, REC = 2 ) ( N( MED ), MATER( MED ), MED = 1, NMEDIA)                     
      READ( BINFIL, REC = 3 ) BCTOP(1:1), CPT, CST, RHOT, DEPTHT, BCBOT(1:1), CPB, CSB, RHOB, DEPTHB                             
      READ( BINFIL, REC = 4 ) ( DEPTH( MED ), RHO( MED ), MED = 1, NMEDIA )               
      READ( BINFIL, REC = 5 ) M, LRECL

      ALLOCATE( CK( M ) )

      READ( BINFIL, REC = 6 ) ( Z( IZ ), IZ = 1, NTOT ) 
                                                                        
      ! *** Read in eigenvalues, CK( I ) ***
                                                                        
      IFIRST = 1 
      DO IREC = 1, 1 + ( 2 * M - 1 ) / LRECL 
         ILAST  = MIN( M, IFIRST + LRECL / 2 - 1 ) 
                                                                        
         READ( BINFIL, REC = 6 + M + IREC ) ( CK( MODE ), MODE = IFIRST, ILAST )      
                                                                        
         IFIRST = ILAST + 1 
      END DO 
                                                                        
      ! *** Write header ***
                                                                        
      WRITE( ASCFIL, * ) LRECL 
      WRITE( ASCFIL, * ) '''', TITLE(1:70), '''' 
      WRITE( ASCFIL, * ) FREQ, NMEDIA, NTOT, NMAT, M 
                                                                        
      DO MED = 1, NMEDIA 
         WRITE( ASCFIL, * ) N( MED ), DEPTH( MED ), RHO( MED ), '''', MATER( MED ), ''''                        
      ENDDO 
                                                                        
      WRITE( ASCFIL, * ) '''', BCTOP(1:1), '''', CPT, CST, RHOT, DEPTHT 
      WRITE( ASCFIL, * ) '''', BCBOT(1:1), '''', CPB, CSB, RHOB, DEPTHB 
                                                                        
      WRITE( ASCFIL, * ) 
      DO IZ = 1, NTOT 
         WRITE( ASCFIL, * ) Z( IZ ) 
      END DO 
                                                                        
      ! *** Write out eigenvalues ***
                                                                        
      WRITE( ASCFIL, * ) 
      DO MODE = 1, M 
         WRITE( ASCFIL, * ) REAL( CK( MODE ) ), AIMAG( CK( MODE ) ) 
      ENDDO 
                                                                        
      ! *** Loop to read and write the modes ***
                                                                        
      DO MODE = 1, M 
         READ(  BINFIL, REC = 6 + MODE ) ( PHI( IZ ), IZ = 1, NTOT ) 
         WRITE( ASCFIL, * ) 
         DO IZ = 1, NTOT 
            WRITE( ASCFIL, * ) REAL( PHI( IZ ) ), AIMAG( PHI( IZ ) ) 
         ENDDO 
      ENDDO 
                                                                        
      STOP 
      END                                           
