function march( c2R, c2I, rho, x, Ik, Pos, deltak, rr )

% Solve system for a sequence of k-values

global NMedia N H
global Pulse fMin fMax tStart alpha beta V
global deltat Ntout
global AD0 AE0 AD1 AE1 AD2 AE2
global cLow cHigh

MaxIT = 1e6;

% March a single spectral component forward in time

NTot1 = length( c2R );
AD0 = zeros( NTot1, 1 );
AE0 = zeros( NTot1, 1 );
AD1 = zeros( NTot1, 1 );
AE1 = zeros( NTot1, 1 );
AD2 = zeros( NTot1, 1 );
AE2 = zeros( NTot1, 1 );

% *** Assemble mass and stiffness matrices ***

rkT  = sqrt( x );

Node = 1;
L    = 1;

AD2( 1 ) = 0.0;
AD1( 1 ) = 0.0;
AD0( 1 ) = 0.0;

for medium = 1 : NMedia
    hElt = H( medium );

    for I = 1 : N( medium )
        rhoElt = ( rho( L ) + rho( L + 1 ) ) / 2.0;
        c2RElt = ( c2R( L ) + c2R( L + 1 ) ) / 2.0;
        c2IElt = ( c2I( L ) + c2I( L + 1 ) ) / 2.0;
        
        contrib( Node, x, rkT, V, rhoElt, c2RElt, c2IElt, ...
            hElt, deltat, alpha, beta );

        Node = Node + 1;
        L    = L    + 1;
    end

    L = L + 1;
end

%factor( NTot1, AD2, AE2, RV1, RV2, RV3, RV4 )  % * Factor A2 *

% * Initialize pressure vectors *

U0 = zeros( NTot1, 1 );
U1 = zeros( NTot1, 1 );
U2 = zeros( NTot1, 1 );

% Initializate parameters for bandpass filtering of the source time series

if ( Pulse(4:4) == 'L' || Pulse(4:4) == 'B' )
    fLoCut = rkT * cLow / ( 2.0 * pi );
else
    fLoCut = 0.0;
end

if ( Pulse(4:4) == 'H' || Pulse(4:4) == 'B' )
    fHiCut = rkT * cHigh / ( 2.0 * pi );
else
    fHiCut = 10.0 * fMax;
end

time   = 0.0;
IniFlag = 1;   % need to tell source routine to refilter for each new value of k

% *** Begin forward march ***

%[ AD0( 200 ) AE0( 200 ) AD1( 200 ) AE1( 200 ) AD2( 200 ) AE2( 200 ) ]

Itout = 1;
for Itime = 1 : MaxIT
    time = tStart + ( Itime - 1 ) * deltat;
    %fprintf( '\n time %i %f', Itime, time );

    % * Take a step forward in time *
    [ U0, U1, U2 ] = step( AD0, AE0, AD1, AE1, AD2, AE2, ...
        U0, U1, U2, time, rkT, Pos );
    Itout = extract( U0, U1, Ik, time, Itout, rkT, deltak, rr, Pos ); % Extract soln for desired receivers
    if ( Itout > Ntout )
        return;
    end
end

