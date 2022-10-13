function [ U0, U1, U2 ] = step( AD0, AE0, AD1, AE1, AD2, AE2, U0, U1, U2, time, rkT, Pos )

% Take a time step

% This is the inner loop.  Note that a significant speed-up can
% be obtained by using real arithmetic and eliminating the
% tridiagonal solver if the Hilbert transform is bypassed and the
% explicit solver is used.

global WS Isd
global Pulse alpha beta V deltat
global omega Bdry

NTot1 = length( U0 );

deltat2 = deltat ^ 2;

% ** Form U2TEMP = A1*U1 + A0*U0 + S **

% Surface point
J = 1;
U2(J) = AD1(J) * U1(J) + AE1(J+1) * U1(J+1) + AD0(J) * U0(J) + AE0(J+1) * U0(J+1);

% Interior points
L = NTot1 - 1;
U2(2:L) = AD1(2:L) .* U1(2:L) + AE1(2:L) .* U1(1:L-1) + AE1(3:L+1) .* U1(3:L+1) ...
        + AD0(2:L) .* U0(2:L) + AE0(2:L) .* U0(1:L-1) + AE0(3:L+1) .* U0(3:L+1);

% Bottom point
J = NTot1;
U2(J) = AD1(J) * U1(J) + AE1(J) * U1(J-1) + AD0(J) * U0(J) + AE0(J) * U0(J-1);

% ** Source terms **

Ntpts = 1;
timeV( 1 ) = time; 	% source expects a vector, not a scalar
%source( timeV, ST, sd, Nsd, Ntpts, omega, fLoCut, fHiCut, Pulse, PulseTitle, IniFlag );
[ ST, PulseTitle ] = cans( time, omega, Pulse );
%fprintf( '\n time s %f %f', time, ST );

medium = 1;	  % Assumes source in first medium
ST = deltat2 * ST * exp( -i * rkT * V * time );

for is = 1 : Pos.Nsd
    js   = Isd( is ) + ( medium - 1 );

    U2( js   ) = U2( js   ) + ( 1.0 - WS( is ) ) * ST( is );
    U2( js+1 ) = U2( js+1 ) +         WS( is )   * ST( is );
end

% ** Solve A2*U2 = U2TEMP **

% Implicit or explicit solver?
if ( alpha == 0.0 && beta == 0.0 )
    U2 = U2 ./ AD2;
else
    BACKSB( NTot1, RV1, RV2, RV3, RV4, U2 );
end

% Do a roll
U0 = U1;
U1 = U2;

% * Boundary conditions (natural is natural) *
if ( Bdry.Top.Opt(2:2) == 'V' )
    U1( 1 )     = 0.0;
end
if ( Bdry.Bot.Opt(1:1) == 'V' )
    U1( NTot1 ) = 0.0;
end
