SUBROUTINE ReadHeader( SHDFil, FileName, Title, Freq, Atten, DeltaR, PltTyp, xs, ys, Theta )

  USE SdRdRMod

  INTEGER   SHDFil
  CHARACTER Title*80, FileName*80, PltTyp*10

  ! *** Open file, read header ***

  IF ( SHDFil == 0 ) SHDFil = 25
  IF ( FileName(1:1) == ' ' ) FileName = 'SHDFIL'

  ! INQUIRE( FILE = FileName, RECL = IRECL )
  OPEN( UNIT = SHDFil,   FILE = FileName, STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED', RECL = 4, IOSTAT = IOStat )
  IF ( IOsTAT /= 0 ) CALL ERROUT( PRTFIL, 'F', 'RDHEAD', 'Unable to open shade file' )
  READ( SHDFil, REC = 1 ) LRecl
  CLOSE( UNIT = SHDFil )
  OPEN( UNIT = SHDFil,   FILE = FileName, STATUS = 'OLD', ACCESS = 'DIRECT', FORM = 'UNFORMATTED', RECL = 4 * LRecl )

  READ( SHDFil, REC = 1 ) LRecl, Title
  READ( SHDFil, REC = 2 ) PltTyp, XS, YS, Theta
  READ( SHDFil, REC = 3 ) Freq, NSD, NRD, NR

  ALLOCATE( SD( NSD ), RD( NRD ), R( NR ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFIL, 'F', 'RDHEAD', 'Too many source/receiver combinations' )

  XS = XS / 1000.0
  YS = YS / 1000.0

  READ( SHDFil, REC = 4 ) ( SD( I ), I = 1, NSD )
  READ( SHDFil, REC = 5 ) ( RD( I ), I = 1, NRD )
  READ( SHDFil, REC = 6 ) ( R(  I ), I = 1, NR  )

  DeltaR = R( NR ) - R( NR - 1 )

  RETURN
END SUBROUTINE ReadHeader
