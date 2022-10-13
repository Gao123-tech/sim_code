        SUBROUTINE SMOOTH( TLRAW, TL, NROWS, NTHETA, NR, DR, DR3DB )

!       Smooth out T.L. curve using Gaussian averaging ---
!          2*J3DB is the number of points averaged in
!          SIG    is the 'sharpness' coefficient

        PARAMETER ( LG3DB = .345388 )
        REAL TLRAW( NROWS, NR ), TL( NROWS, NR ), NORM
        REAL, ALLOCATABLE :: GCOEF( : )

        J3DB = INT( DR3DB / ( 2.0 * DR ) )
        NJ = 2 * J3DB

        ALLOCATE( GCOEF( 0: NJ ) )

!       *** If no smoothing then do a simple copy ***

        IF ( NJ == 0 ) THEN
           TL( 1:NTHETA, 1:NR ) = TLRAW( 1:NTHETA, 1:NR )
           RETURN
        END IF

!       *** Compute the filter coefficients ***

        SHRPCF = LG3DB / FLOAT( J3DB **2 )
        DO JR = 0, NJ
           JSQ = ( J3DB - JR ) ** 2
           GCOEF( JR ) = EXP( -SHRPCF * FLOAT( JSQ ) )
        END DO

!       *** Apply the filter ***

        DO JTH = 1, NTHETA
           DO IR = 1, NR
              TLSUM = 0.0
              NORM  = 0.0
              DO JR = -J3DB, J3DB
                 K1 = IR + JR
                 IF ( K1 > 0 .AND. K1 <= NR ) THEN
                    TLSUM = TLSUM + TLRAW( JTH, K1 ) * GCOEF( J3DB + JR)
                    NORM = NORM + GCOEF( J3DB + JR )
                 ENDIF
              END DO
              TL( JTH, IR ) = TLSUM / NORM
           END DO
        END DO

        RETURN
        END
