function sparcM( filename )

% Wavenumber integration code for ocean acoustics problems
% Based on the earlier Fortran version
% MBP Dec. 24, 2005

clear global
tic

if ( isempty( filename ) )
    warndlg( 'No envfil has been selected', 'Warning' );
end

global omega Bdry
global NMedia N depth H
global Pulse fMin fMax tStart tMult alpha beta V
global tout Ntout
global cLow cHigh

envfil = [ filename '.env' ];
[ PlotTitle, freq, Bdry, fid ] = readenv( envfil );   % read in the environmental file

PlotTitle = ['SPARC(M) -' PlotTitle ];
omega  = 2 * pi * freq;

cLow   = fscanf( fid, '%f', 1 );   % lower phase speed limit
cHigh  = fscanf( fid, '%f', 1 );   % upper phase speed limit
fprintf( '\n cLow = %8.1f cHigh = %8.1f \n', cLow, cHigh )
fgetl( fid );

RMax  = fscanf( fid, '%f', 1 );   % read max range, Rmax
fprintf( 'RMax = %f \n', RMax )
fgetl( fid );

Pos = readsdrd( fid );    % read in the source and receiver depths

% *** souce information ***

Pulse = fgetl( fid );
Pulse = Pulse(2:end); % remove leading quote

switch ( Pulse(1:1) )
    case ( 'P' )
        fprintf( 'Pseudo-gaussian pulse' )
    case ( 'R' )
        fprintf( 'Ricker wavelet' )
    case ( 'A' )
        fprintf( 'Approximate Ricker wavelet' )
    case ( 'S' )
        fprintf( 'Single sine source' )
    case ( 'H' )
        fprintf( 'Hanning weighted four sine pulse' )
    case ( 'N' )
        fprintf( 'N-wave pulse' )
    case ( 'M' )
        fprintf( 'Miracle-wave pulse' )
    case ( 'G' )
        fprintf( 'Gaussian pulse' )
    case ( 'F' )
        fprintf( 'Source time series from File' )
    case ( 'B' )
        fprintf( 'Source time series reversed from file' )
    otherwise
        error( 'Unknown source type' )
end

fMin  = fscanf( fid, '%f', 1 );
fMax  = fscanf( fid, '%f', 1 ); % Upper and lower frequency limits
fgetl( fid );
fprintf( '\nfMin, fMax, %f %f', fMin, fMax )

kMin = 2.0 * pi * fMin / cHigh;   % minimum wavenumber
kMax = 2.0 * pi * fMax / cLow;    % maximum wavenumber
if ( cHigh > 1.0E6 )
    kMin = 0.0;
end

Nk = 1000.0 * RMax * ( kMax - kMin ) / ( 2.0 * pi );
fprintf( '\n\nNumber of wavenumbers, Nk = %i \n', Nk )
k = linspace( kMin, kMax, Nk );

[ rr,   Nrr   ] = readr( fid ); % Read receiver ranges
[ tout, Ntout ] = readt( fid ); % Read in the output times

% *** Integration parameters ***
% alpha = 0.0  lumped     mass matrix,
%         1.0  consistent mass matrix
% beta  = 0.0  standard explicit,
%         0.25 standard implicit

tStart = fscanf( fid, '%f', 1 );
tMult  = fscanf( fid, '%f', 1 );
alpha  = fscanf( fid, '%f', 1 );
beta   = fscanf( fid, '%f', 1 );
V      = fscanf( fid, '%f', 1 );

fprintf( '\n tStart = %f', tStart );
fprintf( '\n tMult  = %f', tMult );
fprintf( '\n alpha  = %f', alpha );
fprintf( '\n beta   = %f', beta );
fprintf( '\n V      = %f', V );
fclose( fid );      % close out the envfil

H( 1:NMedia ) = ( depth( 2:NMedia + 1 ) - depth( 1:NMedia ) ) ./ N( 1:NMedia );   % vector of mesh widths
[ c2R, c2I, rho, crosst, cMin, cMax ] = initsparc( Pos );
kernelsparc( c2R, c2I, rho, crosst, cMin, cMax, k, freq, Pos, rr, Bdry, PlotTitle );

fprintf( '\n' );
toc