MODULE scomod

   SAVE

   INTEGER, PARAMETER :: ENVFil = 5, PRTFil = 6, GRNFil = 25, IRCFil = 12
   COMPLEX (KIND=8), PARAMETER :: CI = ( 0.0, 1.0 )
   PARAMETER ( MaxMedium = 100, PI = 3.1415926535898D0 )

   INTEGER    N( MaxMedium ), Loc( MaxMedium ), NFirstAcoustic, NLastAcoustic, NMedia, Nk
   REAL    (KIND=8) :: Depth( MaxMedium ), H( MaxMedium ), SIGMA( MaxMedium ), RhoT, RhoB, Cmin, Clow, CHigh, omega2, &
                     RMax, Atten, BumDen, Eta, Xi
   COMPLEX (KIND=8) :: CPT, CST, CPB, CSB
   CHARACTER Mater( MaxMedium )*8, TopOpt*8, BotOpt*8

END MODULE scomod
