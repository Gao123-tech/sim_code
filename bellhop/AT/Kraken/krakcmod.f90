MODULE krakcmod

  SAVE

  INTEGER,          PARAMETER :: ENVFil = 5, PRTFil = 6, MODFil = 20, EVMFil = 22, MaxM = 5000, MaxMed = 500, NSets = 5
  REAL    (KIND=8), PARAMETER :: PI = 3.1415926535898D0, DegRad = PI / 180.0
  COMPLEX (KIND=8), PARAMETER :: CI = ( 0.0, 1.0 )

  INTEGER LOC( MaxMed ), NFACT, NLACT, NMEDIA, NG( MaxMed ), N( MaxMed ), NV( NSets ), &
          ISET, M, LRECL, MODECT, MODE, IPROF

  REAL (KIND=8) ::  ET( NSets ), DEPTH( MaxMed ), H( MaxMed ), HV( NSets ), VG( MaxM ),  &
     RhoT, RhoB, CMIN, CLOW, CHIGH, FREQ, OMEGA2, RMax,             &
     SIGMA( MaxMed ), BUMDEN, ETA, XI

  COMPLEX (KIND=8) :: CPT, CST, CPB, CSB, CK( MaxM ), EVMAT( NSets, MaxM ), EXTRAP( NSets, MaxM )

  CHARACTER MATER( MaxMed )*8, TOPOpt*8, BOTOpt*8, TITLE*80

  REAL    (KIND=8), ALLOCATABLE :: Rho( : )
  COMPLEX (KIND=8), ALLOCATABLE :: B1( : ), B2( : ), B3( : ), B4( : )

END MODULE krakcmod
