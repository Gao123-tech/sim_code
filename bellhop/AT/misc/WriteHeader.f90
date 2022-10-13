SUBROUTINE WriteHeader( FileName, Title, SD, NSD, RD, NRD, R, NR, Freq, Atten, PlotType, XS, YS, Theta )

  ! Routine to write header to disk file

  ! FileName is usually SHDFIL for complex pressure or GRNFIL for a Green's function
  ! Title  is an arbitrary title
  ! SD     is a vector of source   depths, SD(1:NSD)
  ! RD     is a vector of receiver depths, RD(1:NRD)
  ! R      is a vector of receiver ranges, R(1:NR)
  ! Freq   is the frequency
  ! Atten  is the stabilizing attenuation (which is only
  !    important for FFP runs. Use zero for other cases.)

  IMPLICIT NONE
  INTEGER SHDFil
  PARAMETER ( SHDFil = 25 )

  INTEGER Nsd, Nrd, Nr, LRecl
  REAL (KIND=4) :: R( * ), RD( * ), SD( * ), Freq, XS, YS, THETA, Atten
  CHARACTER FileName*( 6 ), Title*( * ), PlotType*10

  LRecl = MAX( 40, NSD, NRD, 2 * NR )   ! words/record

  OPEN ( FILE = FileName, UNIT = SHDFil, STATUS = 'UNKNOWN', ACCESS = 'DIRECT', RECL = 4 * LRecl, FORM = 'UNFORMATTED')

  WRITE( SHDFil, REC = 1 ) LRecl, Title(1:80)
  WRITE( SHDFil, REC = 2 ) PlotType, XS, YS, Theta
  WRITE( SHDFil, REC = 3 ) Freq, NSD, NRD, NR, atten
  WRITE( SHDFil, REC = 4 ) SD( 1 : NSD )
  WRITE( SHDFil, REC = 5 ) RD( 1 : NRD )
  WRITE( SHDFil, REC = 6 ) R(  1 : NR  )

  RETURN
END SUBROUTINE WriteHeader

!**********************************************************************!

SUBROUTINE WRTFLD( P, NRD, NR, IRec )

  !     Write the field to disk

  INTEGER, PARAMETER :: SHDFil = 25
  COMPLEX P( NRD, * )

  DO I = 1, NRD
     IRec = IRec + 1
     WRITE( SHDFil, REC = IRec ) ( P( I, J ), J = 1, NR )
  END DO

  RETURN
END SUBROUTINE WRTFLD
