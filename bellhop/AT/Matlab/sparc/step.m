function [ U0, U1 ] = step( A0, A1, AD2, AE2, U0, U1, time, rkT, omega, Bdry, WS, Isd, Pulse, alpha, beta, V, deltat2 )

% Take a time step

% This is the inner loop.  Note that a significant speed-up can
% be obtained by using real arithmetic and eliminating the
% tridiagonal solver if the Hilbert transform is bypassed and the
% explicit solver is used.

NTot1   = length( U0 );

% ** Form U2TEMP = A1*U1 + A0*U0 + S **

U2 = A1 * U1 + A0 * U0;

%Ntpts = 1;
%timeV( 1 ) = time; 	% source expects a vector, not a scalar
%source( timeV, ST, sd, Nsd, Ntpts, omega, fLoCut, fHiCut, Pulse, PulseTitle, IniFlag );
[ ST, PulseTitle ] = cans( time, omega, Pulse );

ST = deltat2 * ST * exp( -i * rkT * V * time );
js = Isd;    % assumes source in first medium
U2( js   ) = U2( js   ) + ( 1.0 - WS ) * ST;
U2( js+1 ) = U2( js+1 ) +         WS   * ST;

% ** Solve A2*U2 = U2TEMP **

if ( alpha == 0.0 && beta == 0.0 )   % explicit solver
    U2 = U2 ./ AD2;
else
    backsb( NTot1, RV1, RV2, RV3, RV4, U2 );   % implicit solver
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