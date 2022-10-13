PROGRAM SPARC

  ! Finite element time domain fast-field program
  ! Michael B. Porter   10/87

  USE spamod

  CALL CPU_TIME( Tstart )

  CALL GETPAR

  H( 1:Nmedia ) = ( depth( 2:Nmedia + 1 ) - depth( 1:Nmedia ) ) / N( 1:Nmedia )

  CALL INIT
  CALL WRTHED
  CALL KERNEL

  CALL CPU_TIME( Tend )
  WRITE( PRTFIL, '( '' CPU TIME: '', G15.5 )' ) Tend - Tstart

  STOP
END PROGRAM SPARC

!**********************************************************************C

SUBROUTINE GETPAR

  ! Read in the ENVFIL data

  USE spamod
  USE SdRdRMod

  REAL    (KIND=8) :: freq, rhoTD, rhoBD, bumden, eta, xi, kMin, kMax
  COMPLEX (KIND=8) :: cpTD, csTD, cpBD, csBD

  ! *** Call READIN to get most of the environmental data ***

  TITLE = 'SPARC-   '
  CALL READIN( TITLE, freq, MaxMedium, Nmedia, &
       &   TopOpt, cpTD, csTD, rhoTD, bumden, eta, xi, N, SIGMA, depth, &
       &   BotOpt, cpBD, csBD, rhoBD, ENVFIL, PRTFIL )

  READ(  ENVFIL, *    ) cLow, cHigh   ! Spectral limits
  WRITE( PRTFIL, "( /, ' cLow = ', G12.5, '  cHigh = ', G12.5 )" ) cLow, cHigh
  
  READ(  ENVFIL, * ) RMax   ! Maximum range for calculations ***
  WRITE( PRTFIL, * ) 'RMax = ', RMax

  !  *** Read source/receiver depths ***
  ZMin = depth( 1 )
  ZMax = depth( Nmedia + 1 )
  CALL SDRD( ENVFIL, PRTFIL, ZMin, ZMax )

  omega2 = ( 2.0 * pi * freq ) ** 2

  cpT  = cpTD
  csT  = csTD
  rhoT = rhoTD
  cpB  = cpBD
  csB  = csBD
  rhoB = rhoBD

  READ( ENVFIL, * ) Pulse
  READ( ENVFIL, * ) fMin, fMax ! Upper and lower frequency limits
  WRITE( PRTFIL, * ) 'fMin, fMax = ', fMin, fMax

  ! integration parameters
  kMin = 2.0 * pi * fMin / cHigh
  kMax = 2.0 * pi * fMax / cLow
  IF ( cHigh > 1.0E6 ) kMin = 0.0

  Nk = 1000.0 * RMax * ( kMax - kMin ) / ( 2.0 * pi )

  WRITE( PRTFIL, * ) 'Nk = ', Nk

  ALLOCATE( k( Nk ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFIL, 'F', 'GETPAR', 'Too many pts in k-space'  )

  deltak = ( kMax - kMin ) / ( Nk - 1 )
  k( 1:Nk ) = kMin + (/ ( Ik, Ik = 0, Nk - 1 ) /) * deltak

  SELECT CASE ( Pulse(1:1) )
  CASE ( 'P' )
     WRITE( PRTFIL, * ) 'Pseudo-gaussian pulse'
  CASE ( 'R' )
     WRITE( PRTFIL, * ) 'Ricker wavelet'
  CASE ( 'A' )
     WRITE( PRTFIL, * ) 'Approximate Ricker wavelet'
  CASE ( 'S' )
     WRITE( PRTFIL, * ) 'Single sine source'
  CASE ( 'H' )
     WRITE( PRTFIL, * ) 'Hanning weighted four sine pulse'
  CASE ( 'N' )
     WRITE( PRTFIL, * ) 'N-wave pulse'
  CASE ( 'M' )
     WRITE( PRTFIL, * ) 'Miracle-wave pulse'
  CASE ( 'G' )
     WRITE( PRTFIL, * ) 'Gaussian pulse'
  CASE ( 'F' )
     WRITE( PRTFIL, * ) 'Source time series from File'
  CASE ( 'B' )
     WRITE( PRTFIL, * ) 'Source time series reversed from file'
  CASE DEFAULT
     CALL ERROUT( PRTFIL, 'F', 'GETPAR', 'Unknown source type'  )
  END SELECT

  ! *** Read receiver ranges ***

  CALL RANGES( ENVFIL, PRTFIL )

  ! *** Read in the output times ***

  READ( ENVFIL, * ) Ntout
  ALLOCATE( tout( MAX( 3, Ntout ) ), Stat = IAllocStat )
  IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFIL, 'F', 'GETPAR', 'Too many output times'  )

  tout( 3 ) = -999.9
  READ( ENVFIL, * ) ( tout( Itout ), Itout = 1, Ntout )

  WRITE( PRTFIL, * )
  WRITE( PRTFIL, * ) 'Number of output times = ', Ntout

  CALL SUBTAB( tout, Ntout )
  WRITE( PRTFIL,  "( 5G14.6 )"  ) ( tout( Itout ), Itout = 1, MIN( Ntout, 51 ) )

  ! *** Integration parameters ***
  ! alpha = 0.0  lumped     mass matrix,
  !         1.0  consistent mass matrix
  ! beta = 0.0  standard explicit,
  !        0.25 standard implicit

  READ(  ENVFIL, * ) tStart, tMult, alpha, beta, V
  WRITE( PRTFIL, * ) 'tStart = ', tStart
  WRITE( PRTFIL, * ) 'tMult  = ', tMult
  WRITE( PRTFIL, * ) 'alpha  = ', alpha
  WRITE( PRTFIL, * ) 'beta   = ', beta
  WRITE( PRTFIL, * ) 'V      = ', V

  CLOSE ( ENVFIL )

  IF ( ANY( SIGMA( 1:Nmedia ) /= 0.0 ) ) CALL ERROUT( PRTFIL, 'F', 'INIT', 'Rough interfaces not allowed'  )

  RETURN
END SUBROUTINE GETPAR
!**********************************************************************C
SUBROUTINE INIT

  ! Initializes arrays defining difference equations

  USE spamod
  USE SdRdRMod

  REAL    (KIND=8) :: freq
  COMPLEX (KIND=8) :: cp( MaxN ), cs( MaxN )
  CHARACTER TASK*8

  cMin   =  1.0E6
  cMax   = -1.0E6
  crosst =  1.0E6
  Loc( 1 ) = 0
  J = 0

  ! *** Loop over successive media ***

  DO medium = 1, Nmedia

     IF ( medium /= 1 ) Loc(medium) = Loc(medium-1) + N(medium-1) + 1
     N1 = N( medium ) + 1

     IF ( Loc( medium ) + N1 > MaxN ) THEN
        WRITE( PRTFIL, * ) 'FATAL ERROR: Insufficient storage for mesh'
        STOP               'FATAL ERROR: Insufficient storage for mesh'
     ENDIF

     TASK = 'TAB'
     CALL PROFIL( depth, cp, cs, rho( Loc( medium ) + 1 ), &
          &      medium, N1, freq, TopOpt(1:1), TopOpt(3:4), TASK, ENVFIL, PRTFIL )

     DO I = 1, N1
        cpR  = REAL( cp( I ) )
        cMin = MIN( cpR, cMin )
        cMax = MAX( cpR, cMax )

        crosst1 = REAL( H( medium ) / cp( I ) )
        crosst  = AMIN1( crosst1, crosst )

        J = J + 1
        cpT = cp( I )   ! convert to single precision
        c2R( J ) =  REAL( cpT**2 )  ! CHECK THIS
        c2I( J ) = AIMAG( cpT**2 ) / SQRT( omega2 ) / c2R( J )
     END DO

  END DO

  ! *** Tabulate z-coordinates ***

  Z( 1 ) = depth( 1 )
  J      = 2
  NTot1  = SUM( N( 1:Nmedia ) ) + 1

  DO medium = 1, Nmedia
     Z( J: J + N( medium ) - 1 ) = depth( medium ) + (/ ( I * H( medium ), I = 1, N( medium ) ) /)
     J = J + N( medium )
  END DO

  CALL WEIGHT( Z, NTot1, sd, Nsd, ws, isD ) ! Compute weights for source depth interpolation
  CALL WEIGHT( Z, NTot1, rd, Nrd, wr, irD ) ! Compute weights for rcvr   depth interpolation

  RETURN
END SUBROUTINE INIT
!**********************************************************************C
SUBROUTINE KERNEL

  ! Solve system for a sequence of k-values

  USE spamod
  USE SdRdRMod

  ! Allocate and clear matrices for storing time-series

  SELECT CASE ( TopOpt(4:4) )
  CASE ( 'D')
     ALLOCATE( RTSrd( Nrd, Ntout ) )
     RTSrd = 0.0
  CASE ( 'R')
     ALLOCATE( RTSrr( Nr,  Ntout ) )
     RTSRR = 0.0
  CASE DEFAULT
     ALLOCATE( Green( Ntout, Nrd, Nk ) )
  END SELECT

  deltat = tMult / SQRT( 1.0 / crosst**2 + ( 0.5 * cMax * k( Nk ) ) **2 ) ! Courant condition to set time step

  WRITE( PRTFIL, * ) 'Time step = ', deltat
  WRITE( PRTFIL, * ) 'Estimated fl. pt. ops (millions) = ', ( tout( Ntout ) / deltat ) * Nk * NTot1 / 25000

  ! *** Loop over spectral components ***

  DO Ik = 1, Nk
     x = k( Ik ) ** 2
     deltat = tMult / SQRT( 1.0 / crosst**2 + ( 0.5 * cMax * k( ik ) ) **2 )
     ! IF ( 10*(Ik/10) == Ik ) WRITE( 6, * ) 'Ik, Nk', Ik, Nk
     WRITE( 6, * ) 'Ik, Nk', Ik, Nk

     CALL MARCH( x, Ik )  ! March that component for all time

  END DO

  SELECT CASE ( TopOpt(4:4) )
  CASE ( 'S' )          ! *** Case of a snapshot ***
     DO Itout = 1, Ntout
        DO ir = 1, Nrd
           IG = ( Itout - 1 ) * Nrd + ir   ! Index of source/rcvr combo
           WRITE( GRNFIL, REC = 6 + IG) ( Green( Itout, ir, Ik ), Ik = 1, Nk )
        END DO
     END DO
     CLOSE( GRNFIL )

  CASE ( 'D' )   ! *** Write out RTS (vertical array) *
     Scale = 1.0 / SQRT( pi * r( 1 ) )
     DO Itout = 1, Ntout
        WRITE( RTSFIL, '( 12G15.6 )' ) tout( Itout ), ( Scale * RTSrd( ir, Itout ), ir = 1, Nrd )
     END DO
     CLOSE( RTSFIL )

  CASE ( 'R' )   !  *** Write out RTS (horizontal array) ***
     DO Itout = 1, Ntout
        WRITE( RTSFIL, '( 12G15.6 )' ) tout( Itout ), ( RTSrr( irr, Itout ), irr = 1, Nr )
     END DO
     CLOSE( RTSFIL )
  END SELECT

  RETURN
END SUBROUTINE KERNEL
!**********************************************************************C
SUBROUTINE WRTHED

  ! Writes headers for disk files

  USE spamod
  USE SdRdRMod
  CHARACTER FileName*6, PlotType*10

  SELECT CASE ( TopOpt(4:4) )
  CASE ( 'S' )          ! *** Case of a snapshot ***
     FileName = 'GRNFIL'
     freq   = 0.0
     Atten  = 0.0   ! no stabilizing attenuation (off-axis integration of G(k)) in a SPARC run
     PlotType = 'Green'
     XS = 0.0   ;   YS = 0.0   ;   Theta = 0.0
     CALL WriteHeader( FileName, Title, Tout, NTout, rd, Nrd, k, Nk, freq, Atten, PlotType, &
          XS, YS, Theta )
  CASE ( 'R' )          ! *** Horizontal array ***
     OPEN ( FILE = 'RTSFIL', UNIT = RTSFIL, STATUS = 'NEW', FORM = 'FORMATTED' )
     WRITE( RTSFIL, * ) '''' // TITLE(1:75) // ''''
     WRITE( RTSFIL, * ) Nr, ( r( irr ), irr = 1, Nr )
  CASE ( 'D' )          ! *** Vertical array ***
     OPEN ( FILE = 'RTSFIL', UNIT = RTSFIL, STATUS = 'NEW', FORM = 'FORMATTED' )
     WRITE( RTSFIL, * ) '''' // TITLE(1:75) // ''''
     WRITE( RTSFIL, * ) Nrd, ( rd( ir ), ir = 1, Nrd )
  END SELECT

  RETURN
END SUBROUTINE WRTHED
!**********************************************************************C
SUBROUTINE MARCH( x, Ik )

  ! March a single spectral component forward in time

  USE spamod
  USE SdRdRMod
  LOGICAL IniFlag
  COMPLEX AD2( NTot1 ), AE2( NTot1 ), AD1( NTot1 ), AE1( NTot1 ), AD0( NTot1 ), AE0( NTot1 ), &
       RV1( NTot1 ), RV2( NTot1 ), RV3( NTot1 ), RV4( NTot1 ), U0(  NTot1 ), U1(  NTot1 ), U2(  NTot1 )

  ! *** Assemble mass and stiffness matrices ***

  rkT  = SQRT( x )
  Node = 1
  L    = 1

  AD2( 1 ) = 0.0
  AD1( 1 ) = 0.0
  AD0( 1 ) = 0.0

  DO medium = 1, Nmedia
     hElt = H( medium )

     DO I = 1, N(medium)
        rhoElt = ( rho( L ) + rho( L + 1 ) ) / 2.0
        c2RElt = ( c2R( L ) + c2R( L + 1 ) ) / 2.0
        c2IElt = ( c2I( L ) + c2I( L + 1 ) ) / 2.0

        CALL CONTRIB( Node, x, rkT, V, rhoElt, c2RElt, c2IElt, &
             &        hElt, deltat, AD2, AE2, AD1, AE1, AD0, AE0, alpha, beta )

        Node = Node + 1
        L    = L    + 1
     END DO

     L = L + 1
  END DO

  CALL FACTOR( NTot1, AD2, AE2, RV1, RV2, RV3, RV4 )  ! * Factor A2 *

  ! * Initialize pressure vectors *

  U0 = 0.0
  U1 = 0.0
  U2 = 0.0

  ! Initializate parameters for bandpass filtering of the source time series

  IF ( Pulse(4:4) == 'L' .OR. Pulse(4:4) == 'B' ) THEN
     fLoCut = rkT * cLow / ( 2.0 * pi )
  ELSE
     fLoCut = 0.0
  ENDIF

  IF ( Pulse(4:4) == 'H' .OR. Pulse(4:4) == 'B' ) THEN
     fHiCut = rkT * cHigh / ( 2.0 * pi )
  ELSE
     fHiCut = 10.0 * fMax
  ENDIF

  time   = 0.0
  IniFlag = .TRUE.   ! need to tell source routine to refilter for each new value of k

  ! *** Begin forward march ***

  Itout = 1
  DO Itime = 1, MaxIT
     time = tStart + ( Itime - 1 ) * deltat

     ! * Take a step forward in time *

     CALL STEP( AD0, AE0, AD1, AE1, AD2, AE2, RV1, RV2, RV3, RV4, &
          &      U0, U1, U2, time, rkT, fLoCut, fHiCut, IniFlag )

     CALL EXTRACT( U0, U1, Ik, time, Itout, rkT ) ! Extract soln for desired receivers

     IF ( Itout > Ntout ) RETURN
  END DO

  RETURN
END SUBROUTINE MARCH

!**********************************************************************C

SUBROUTINE CONTRIB( Node, x, rkT, V, rhoElt, c2RElt, c2IElt, &
     &   hElt, deltat, AD2, AE2, AD1, AE1, AD0, AE0, alpha, beta )

  ! Computes the contribution for the given element

  COMPLEX, PARAMETER :: CI = ( 0.0, 1.0 )
  COMPLEX AD2( * ), AE2( * ), AD1( * ), AE1( * ), AD0( * ), &
      &   AE0( * ), Ke( 2, 2 ), Ce( 2, 2 ), Me( 2, 2 ), Y, Z

  !  alpha controls lumping and beta controls implicitness

  deltat2 = deltat ** 2
  rhoh    = rhoElt * hElt

  ! * Elemental stiffness matrix ( +k2 term ) *

  y = 1.0 + CI * rkT * V * c2IElt
  z = x * ( y - V * V / c2RElt )
  Ke( 1, 1 ) =  y / rhoh + (3.0-alpha) * hElt * z / rhoElt / 6.0
  Ke( 1, 2 ) = -y / rhoh +      alpha  * hElt * z / rhoElt / 6.0
  Ke( 2, 2 ) =  y / rhoh + (3.0-alpha) * hElt * z / rhoElt / 6.0

  ! * Elemental damping matrix *

  z = 2.0 * CI * rkT * V / c2RElt
  Ce( 1, 1 ) = c2IElt * ( 1.0/rhoh + (3.0-alpha) * hElt * x / rhoElt / 6.0) &
       &                           + (3.0-alpha) * hElt * z / rhoElt / 6.0
  Ce( 1, 2 ) = c2IElt * (-1.0/rhoh +      alpha  * hElt * x / rhoElt / 6.0) &
       &                           +      alpha  * hElt * z / rhoElt / 6.0
  Ce( 2, 2 ) = c2IElt * ( 1.0/rhoh + (3.0-alpha) * hElt * x / rhoElt / 6.0) &
       &                           + (3.0-alpha) * hElt * z / rhoElt / 6.0

  ! * Elemental mass matrix *

  Me( 1, 1 ) = (3.0-alpha) * hElt / ( rhoElt * c2RElt ) / 6.0
  Me( 1, 2 ) =      alpha  * hElt / ( rhoElt * c2RElt ) / 6.0
  Me( 2, 2 ) = (3.0-alpha) * hElt / ( rhoElt * c2RElt ) / 6.0

  ! * A2 matrix *

  AD2( Node   ) = AD2( Node ) + &
       &          Me(1,1) + 0.5 * deltat * Ce(1,1) + beta * deltat2 * Ke(1,1)
  AE2( Node+1 ) = Me(1,2) + 0.5 * deltat * Ce(1,2) + beta * deltat2 * Ke(1,2)
  AD2( Node+1 ) = Me(2,2) + 0.5 * deltat * Ce(2,2) + beta * deltat2 * Ke(2,2)

  ! * A1 matrix *

  AD1( Node   ) = AD1( Node ) + &
       &          2.0*Me(1,1) - ( 1.0 - 2.0*beta ) *deltat2 * Ke(1,1)
  AE1( Node+1 ) = 2.0*Me(1,2) - ( 1.0 - 2.0*beta ) *deltat2 * Ke(1,2)
  AD1( Node+1 ) = 2.0*Me(2,2) - ( 1.0 - 2.0*beta ) *deltat2 * Ke(2,2)

  ! * A0 matrix *

  AD0( Node   ) = AD0( Node ) &
       &          -Me(1,1) + 0.5 * deltat * Ce(1,1) - beta * deltat2 * Ke(1,1)
  AE0( Node+1 ) = -Me(1,2) + 0.5 * deltat * Ce(1,2) - beta * deltat2 * Ke(1,2)
  AD0( Node+1 ) = -Me(2,2) + 0.5 * deltat * Ce(2,2) - beta * deltat2 * Ke(2,2)

  RETURN
END SUBROUTINE CONTRIB

!**********************************************************************C

SUBROUTINE STEP( AD0, AE0, AD1, AE1, AD2, AE2, &
     &   RV1, RV2, RV3, RV4, U0, U1, U2, time, rkT, fLoCut, fHiCut, IniFlag )

  !     Take a time step

  !     This is the inner loop.  Note that a significant speed-up can
  !     be obtained by using real arithmetic and eliminating the
  !     tridiagonal solver if the Hilbert transform is bypassed and the
  !     explicit solver is used.

  USE spamod
  USE SdRdRMod

  LOGICAL IniFlag
  REAL    timeV( 1 )
  COMPLEX AD2( NTot1 ), AE2( NTot1 ), AD1( NTot1 ), AE1( NTot1 ), AD0( NTot1 ), &
       &        AE0( NTot1 ), RV1( NTot1 ), RV2( NTot1 ), RV3( NTot1 ), RV4( NTot1 ), &
       &        U0( NTot1 ), U1( NTot1 ), U2( NTot1 ), ST( Nsd )
  CHARACTER PulseTitle*60

  omega   = SQRT( omega2 )
  deltat2 = deltat ** 2

  ! *** Form U2TEMP = A1*U1 + A0*U0 + S ***

  ! Surface point
  J = 1
  U2(J) = AD1(J) * U1(J) + AE1(J+1) * U1(J+1) + AD0(J) * U0(J) + AE0(J+1) * U0(J+1)

  ! Interior points
  L = NTot1 - 1
  U2(2:L) = AD1(2:L) * U1(2:L) + AE1(2:L) * U1(1:L-1) + AE1(3:L+1) * U1(3:L+1) &
          + AD0(2:L) * U0(2:L) + AE0(2:L) * U0(1:L-1) + AE0(3:L+1) * U0(3:L+1)

  ! Bottom point
  J = NTot1
  U2(J) = AD1(J) * U1(J) + AE1(J) * U1(J-1) + AD0(J) * U0(J) + AE0(J) * U0(J-1)

  ! *** Source terms ***

  Ntpts = 1
  timeV( 1 ) = time 	! source expects a vector, not a scalar
  CALL SOURCE( timeV, ST, sd, Nsd, Ntpts, omega, fLoCut, fHiCut, Pulse, PulseTitle, IniFlag )

  medium = 1	  ! Assumes source in first medium
  ST = deltat2 * ST * EXP( -CI * rkT * V * time )

  DO is = 1, Nsd
     js   = isd( is ) + ( medium - 1 )

     U2( js   ) = U2( js   ) + ( 1.0 - ws( is ) ) * ST( is )
     U2( js+1 ) = U2( js+1 ) +         ws( is )   * ST( is )
  END DO

  ! *** Solve A2*U2 = U2TEMP ***

  ! Implicit or explicit solver?
  IF ( alpha == 0.0 .AND. beta == 0.0 ) THEN
     U2 = U2 / AD2
  ELSE
     CALL BACKSB( NTot1, RV1, RV2, RV3, RV4, U2 )
  ENDIF

  ! Do a roll

  U0 = U1
  U1 = U2

  ! * Boundary conditions (natural is natural) *

  IF ( TopOpt(2:2) == 'V' ) U1( 1 )     = 0.0
  IF ( BotOpt(1:1) == 'V' ) U1( NTot1 ) = 0.0

  RETURN
END SUBROUTINE STEP

!**********************************************************************C

SUBROUTINE EXTRACT( U0, U1, Ik, time, Itout, rkT )

  ! Extract solution (snapshot, vertical or horizontal array)

  USE spamod
  USE SdRdRMod

  COMPLEX U0( * ), U1( * ), UT1, UT2, U, const

  ! *** Three cases: snapshot, vertical, horizontal time series ***

  DO WHILE ( Itout <= Ntout .AND. time + deltat >= tout( Itout ) )
     ! note above can access tout( ntout + 1 ) which is outside array dimension
     ! but result is irrelevant
     ! exit statement below added to prevent that

     wt = ( tout( Itout ) - time ) / deltat  ! Weight for temporal interpolation:

     SELECT CASE ( TopOpt(4:4) )
     CASE ( 'S' ) ! *** Case of a snapshot ***
        const = EXP( CI * rkT * V * tout( Itout ) )
        DO ir = 1, Nrd
           ! Linear interpolation in depth
           I = Ird( ir )
           UT1 = U0( I ) + wr( ir ) * ( U0( I+1 ) - U0( I ) )
           UT2 = U1( I ) + wr( ir ) * ( U1( I+1 ) - U1( I ) )

           ! Linear interpolation in time
           Green( Itout, ir, Ik ) = const * ( UT1 + wt * ( UT2 - UT1 ) )
        END DO

     CASE ( 'D' ) ! *** Case of an RTS (vertical array) ***
        ! const = -SQRT( 2.0 ) * deltak * SQRT( rkT ) *  COS(      rkT * r( 1 ) - pi / 4.0 )
        const =  SQRT( 2.0 ) * deltak * SQRT( rkT ) * EXP( CI*( rkT * ( V * tout( Itout ) - r( 1 ) ) + pi / 4.0 ) )

        DO ir = 1, Nrd
           ! Linear interpolation in depth
           I = Ird( ir )
           UT1 = U0( I ) + wr( ir ) * ( U0( I+1 ) - U0( I ) )
           UT2 = U1( I ) + wr( ir ) * ( U1( I+1 ) - U1( I ) )

           ! Linear interpolation in time
           U = UT1 + wt * ( UT2 - UT1 )
           RTSrd( ir, Itout ) = RTSrd( ir, Itout ) + const * U
        END DO

     CASE ( 'R' ) ! *** Case of an RTS (horizontal array) ***
        IF ( time >= tout( 1 ) ) THEN
           ! Linear interpolation in depth
           I = Ird( 1 )
           T = ( rd( 1 ) - Z( I ) ) / ( Z( I + 1 ) - Z( I ) )

           UT1 = U0( I ) + T * ( U0( I+1 ) - U0( I ) )
           UT2 = U1( I ) + T * ( U1( I+1 ) - U1( I ) )

           ! Linear interpolation in time
           U = UT1 + wt * ( UT2 - UT1 )
           const = SQRT( 2.0 ) * EXP( CI * rkT * V * time )

           RTSrr( 1:Nr, Itout ) = RTSrr( 1:Nr, Itout ) + deltak * U *  &
                const * EXP( CI * ( -rkT * r( 1:Nr ) + pi / 4.0 ) ) * SQRT( rkT / r( 1:Nr ) )

        ENDIF
     END SELECT
     Itout = Itout + 1
     IF ( Itout == Ntout + 1 ) EXIT   ! did last time point, so exit the do loop
  END DO

  RETURN
END SUBROUTINE EXTRACT
