MODULE spamod

  SAVE

  INTEGER, PARAMETER :: ENVFil = 5, PRTFil = 6, GRNFil = 25, RTSFil = 35
  COMPLEX CI
  PARAMETER ( MaxN = 17000,  MaxMedium = 50, MaxIt = 1000000, CI = ( 0.0, 1.0 ), PI = 3.1415926535898D0 )

  INTEGER LOC( MaxMedium ), N( MaxMedium ), NMedia, NSig, Nk, NTot1, Nrr, NTout

  REAL C2R( MaxN ), C2I( MaxN ), Z( MaxN ), &
       &     RhoT, RhoB, H( MaxMedium ), CMin, CLow, CHigh, Omega2, DeltaK, &
       &     Deltat, CrossT, CMax, TStart, V, TMult, alpha, beta, FMin, FMax

  REAL (KIND=8) :: Depth( MaxMedium ), Rho( MaxN ), sigma( MaxMedium )
  COMPLEX CPT, CST, CPB, CSB

  CHARACTER Mater( MaxMedium )*8, TopOpt*8, BotOpt*8, Pulse*4, Title*80

  REAL, ALLOCATABLE :: k( : ), Tout( : ), RTSrd( :, : ), RTSrr( :, : )

  COMPLEX, ALLOCATABLE :: Green( :, :, : )

END MODULE spamod
