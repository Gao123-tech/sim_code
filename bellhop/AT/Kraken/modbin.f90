      PROGRAM MODBIN 
                                                                        
!     Converts asciii mode file to binary format                        
                                                                        
      INTEGER BINFIL, ASCFIL 

      PARAMETER ( BINFIL = 20, ASCFIL = 5 )

      COMPLEX   CPT, CST, CPB, CSB 
      CHARACTER TITLE*( 80 ), BCTOP*1, BCBOT*1

      INTEGER,   ALLOCATABLE ::   N( : ) 
      REAL,      ALLOCATABLE :: Z(: ), RHO( : ), DEPTH( : )
      COMPLEX,   ALLOCATABLE :: PHI( : ), CK( : )
      CHARACTER, ALLOCATABLE :: MATER( : )*8 

!     *** Read header ***                                               
                                                                        
      READ( ASCFIL, * ) LRECL 
      READ( ASCFIL, * ) TITLE 
      READ( ASCFIL, * ) FREQ, NMEDIA, NTOT, NMAT, M 

      ALLOCATE( N( NMEDIA), MATER( NMEDIA), DEPTH( NMEDIA ), RHO( NMEDIA ), Z( NTOT ), PHI( NTOT ), CK( M ) )
                                                                        
      DO MED = 1, NMEDIA 
         READ( ASCFIL, * ) N( MED ), DEPTH( MED ), RHO( MED ), MATER( MED )                     
      ENDDO 
                                                                        
      READ( ASCFIL, * ) BCTOP(1:1), CPT, CST, RHOT, DEPTHT 
      READ( ASCFIL, * ) BCBOT(1:1), CPB, CSB, RHOB, DEPTHB 
                                                                        
      DO IZ = 1, NTOT 
         READ( ASCFIL, * ) Z( IZ ) 
      END DO 
                                                                        
!     *** Write header ***                                              
                                                                        
      OPEN ( FILE = 'MODFIL', UNIT = BINFIL, STATUS = 'NEW', ACCESS = 'DIRECT', RECL = 4 * LRECL, FORM = 'UNFORMATTED' )    
                                                                        
      WRITE( BINFIL, REC = 1 ) LRECL, TITLE(1:80), FREQ, NMEDIA, NTOT, NMAT                  
      WRITE( BINFIL, REC = 2 ) ( N( MED ), MATER( MED ), MED = 1, NMEDIA)                
      WRITE( BINFIL, REC = 3 ) BCTOP(1:1), CPT, CST, RHOT, DEPTHT, BCBOT(1:1), CPB, CSB, RHOB, DEPTHB        
      WRITE( BINFIL, REC = 4 ) ( DEPTH( MED ), RHO( MED ), MED = 1, NMEDIA )               
      WRITE( BINFIL, REC = 5 ) M, LRECL 
      WRITE( BINFIL, REC = 6 ) ( Z(   IZ ), IZ = 1, NTOT ) 
                                                                        
!     *** Read in eigenvalues, CK( I ) ***                              
                                                                        
      READ( ASCFIL, * ) 
      DO MODE = 1, M 
         READ( ASCFIL, * ) CKR, CKI 
         CK( MODE ) = CMPLX( CKR, CKI ) 
      ENDDO 
                                                                        
!     *** Write out eigenvalues ***                                     
                                                                        
      IFIRST = 1 
      DO IREC = 1, 1 + ( 2 * M - 1 ) / LRECL 
         ILAST  = MIN( M, IFIRST + LRECL / 2 - 1 ) 
                                                                        
         WRITE( BINFIL, REC = 6 + M + IREC ) ( CK( MODE ), MODE = IFIRST, ILAST )      
                                                                        
         IFIRST = ILAST + 1 
      END DO 
                                                                        
!     *** Loop to read and write the modes ***                          
                                                                        
      DO MODE = 1, M 
         READ( ASCFIL, * ) 
         DO IZ = 1, NTOT 
            READ(  ASCFIL, * ) PHIR, PHII 
            PHI( IZ ) = CMPLX( PHIR, PHII ) 
         ENDDO 
         WRITE( BINFIL, REC = 6 + MODE ) ( PHI( IZ ), IZ = 1, NTOT ) 
      END DO 
                                                                        
      STOP 
      END                                           
