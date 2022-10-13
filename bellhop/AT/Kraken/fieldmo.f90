!     Last change:  MPB  22 Dec 2003    8:59 pm
      PROGRAM FIELDMO 
                                                                        
!     Generate file of replica vectors                                  

      USE SdRdRMod

      INTEGER PRTFIL, FLPFIL, SHDFil
      PARAMETER ( FLPFIL = 5, PRTFIL = 6, SHDFil = 25, MXM = 1500, MXNW = 4094, MXNWZ = 210000,  &
     &   MXNSD = 51, MXNRD = 201, MXNOC = 9 )                           
                                                                        
      INTEGER CTR( MXNOC ) 
      REAL      W( MXNW ), ALPHA( MXNOC )
      COMPLEX   CK( MXNOC, MXM ), CKTEMP( MXM), PHITEMP( MXM, MXNRD ),  &
     &          CKA( MXM ), CKB( MXM ),                                 &
     &          PHIS( MXNOC, MXM, MXNSD ),                              &
     &          PHIR( MXNOC, MXM, MXNRD ),                              &
     &          PHISA( MXM, MXNRD ), PHISB( MXM, MXNRD ),               &
     &          PHIRA( MXM, MXNRD ), PHIRB( MXM, MXNRD ),               &
     &          P( MXNWZ )                                              
      CHARACTER OPT*50, SHTITL*80, TITLE*80, COMP*1, FileName*6
                                                                        
!     Read source, rcvr depths and rcvr displacements                   
                                                                        
      SHTITL(1:1) = '$' 
      READ( FLPFIL, * ) SHTITL 
                                                                        
      READ( FLPFIL, * ) OPT 
!      CALL STR$UPCASE( OPT, OPT )                                      
      COMP = OPT( 3:3 ) 
                                                                        
      READ( FLPFIL, * ) MLIMIT 
      MLIMIT = MIN( MXM, MLIMIT ) 
                                                                        
      READ( FLPFIL, * ) NOC 
                                                                        
!     *** Read ocean weights ***                                        
                                                                        
      NW = 0 
      W( 3 ) = -999.9 
      READ( FLPFIL, * ) NW, ( W( IA ), IA = 1, NW ) 
                                                                        
      CALL SUBTAB( W, NW ) 
      WRITE( PRTFIL, * ) 
      WRITE( PRTFIL, * ) 'Number of ocean weights = ', NW 
      IF ( NW .GE. 1 ) WRITE( PRTFIL, * ) ( W( IA ), IA = 1, MIN( NW, 51 ) )
                                                                        
      WRITE( PRTFIL, * ) 
                                                                        
!     *** Read source/receiver depths ***                               
                                                                        
      ZMIN = -1.0E10 
      ZMAX = +1.0E10 
                                                                        
      CALL SDRD( FLPFIL, PRTFIL, ZMIN, ZMAX )
                                                                        
      WRITE( PRTFIL, * ) 
                                                                        
      IF ( NW * NRD .GT. MXNWZ )                                        &
     &   CALL ERROUT( PRTFIL, 'F', 'FIELD', 'Number of oceans x Depths too large' )
                                                                        
      READ( FLPFIL, * ) R 
      R = 1000.0 * R 
                                                                        
!     *** Read in modes ***                                             
                                                                        
      M = MLIMIT 
                                                                        
      DO 2000 IOC = 1, NOC 
                                                                        
         CALL GETMOD( IOC, ' ', MXM, SD, NSD, 'N', CKTEMP, PHITEMP, MNEW, FREQ, TITLE )
                                                                        
!        --- copy into the big matrix                                   
         DO 1000 MODE = 1, MNEW
               PHIS( IOC, MODE, 1:NSD ) = PHITEMP( MODE, 1:NSD )
 1000    CONTINUE 
                                                                        
         CALL GETMOD( IOC, ' ', MXM, RD, NRD, COMP, CKTEMP, PHITEMP, MNEW, FREQ, TITLE )
                                                                        
!        --- copy into the big matrix                                   
         DO 1800 MODE = 1, MNEW
            PHIR( IOC, MODE, 1:NRD ) = PHITEMP( MODE, 1:NRD )
            CK( IOC, MODE ) = CKTEMP( MODE ) 
 1800    CONTINUE 
                                                                        
         M = MIN( M, MNEW ) 
                 ! Next ocean                                           
 2000 END DO 
                                                                        
!     ------ Generate header
      FileName = 'SHDFIL'
      IF ( SHTITL(1:1) .EQ. '$' ) SHTITL = TITLE 
      CALL WriteHeader( FileName, SHTITL, SD, NSD, RD, NRD, W, NW, FREQ, ATTEN, '     ', 0.0, 0.0, 0.0 )
      IREC = 6 
                                                                        
!     *** MAIN LOOP: For each source evaluate and write the field ***   
                                                                        
      DO 8000 J = 1, NSD 
                                                                        
!        --- Initialize counter                                         
         DO 3700 IOC = 1, NOC 
            CTR( IOC ) = 0 
 3700    CONTINUE 
                                                                        
         DO 7000 I = 1, NW ** ( NOC - 2 ) 
                                                                        
!           --- increment the counter and assign the weight             
            IF ( NOC .GE. 3 ) THEN 
            DO 6500 IOC = 3, NOC 
               IF ( CTR( IOC ) .GT. NW ) THEN 
                  CTR( IOC ) = 1 
               ELSE 
                  CTR( IOC ) = CTR( IOC ) + 1 
               ENDIF 
               ALPHA( IOC ) = W( CTR( IOC ) ) 
 6500       CONTINUE 
            ENDIF 
                                                                        
!           ****** Interpolate the modes ******                         
                                                                        
            DO 6000 MODE = 1, M 
                                                                        
!              --- initialize                                           
               CKA( MODE ) = CK( 1, MODE ) 
               CKB( MODE ) = CK( 2, MODE )

               PHISA( MODE, 1:NSD ) = PHIS( 1, MODE, 1:NSD )
               PHISB( MODE, 1:NSD ) = PHIS( 2, MODE, 1:NSD )

               PHIRA( MODE, 1:NRD ) = PHIR( 1, MODE, 1:NRD )
               PHIRB( MODE, 1:NRD ) = PHIR( 2, MODE, 1:NRD )

!              --- add in weighted data from other oceans               
                                                                        
               IF ( NOC .GE. 3 ) THEN 
               DO 4000 IOC = 3, NOC 
!                 --- following can be done more efficiently but ...    
                  CKA( MODE ) = CKA( MODE ) + ALPHA( IOC ) * CK( IOC, MODE )
                  CKB( MODE ) = CKB( MODE ) + ALPHA( IOC ) * CK( IOC, MODE )

                  PHISA( MODE, 1:NSD ) = PHISA( MODE, 1:NSD ) + ALPHA( IOC ) * PHIS( IOC, MODE, 1:NSD )
                  PHISB( MODE, 1:NSD ) = PHISB( MODE, 1:NSD ) + ALPHA( IOC ) * PHIS( IOC, MODE, 1:NSD )

                  PHIRA( MODE, 1:NRD ) = PHIRA( MODE, 1:NRD ) + ALPHA( IOC ) * PHIR( IOC, MODE, 1:NRD )
                  PHIRB( MODE, 1:NRD ) = PHIRB( MODE, 1:NRD ) + ALPHA( IOC ) * PHIR( IOC, MODE, 1:NRD )

                         ! Next ocean                                   
 4000          CONTINUE 
               ENDIF 
                                                                        
                      ! Next mode                                       
 6000       CONTINUE 
                                                                        
            CALL EVALMO( PHISA( 1, J ), PHISB( 1, J ), CKA, CKB, PHIRA, PHIRB, M,   &
     &         RD, NRD, W, NW, R, OPT, P )

            ! following is broken ... P should is P( nrd, Nw )
            DO ii = 1, Nrd
               IRec = IRec + 1
               WRITE( SHDFil, REC = IRec ) ( P( jj ), jj = 1, Nw )
            END DO
  
                    ! Next weight                                       
 7000    CONTINUE 
                                                                        
                ! Next source depth                                     
 8000 END DO 
                                                                        
      STOP 
      END                                           
