function plotshdpol( filename )

% plots a single TL surface in dB (polar coordinates)

% open the file and read data
fid = fopen( filename, 'r' );

temp = fgetl( fid );
pltitl = fgetl( fid )

% xs  = fscanf( fid, '%f', 1 );
% ys  = fscanf( fid, '%f', 1 );
% shift coordinate system to arbitrary position
xs = 333;	% -12.0
ys = 315;	%  13.8

% receiver ranges

junk   = fscanf( fid, '%f', 1 );
nsd    = fscanf( fid, '%i', 1 );
nrd    = fscanf( fid, '%i', 1 );
nrr    = fscanf( fid, '%i', 1 );

sd     = fscanf( fid, '%f', nsd );
rd     = fscanf( fid, '%f', nrd );
rr     = fscanf( fid, '%f', nrr );

isd = 1;
for i = 1:isd
   temp1   = fscanf( fid, '%f', [ 2 * nrr, nrd ] );
   i, size(temp1)
end

zt = rd;
taker = 1:nrr;
rt = rr( taker );

rkm = rt / 1000.0;

% make plot polar

[th, r ] = meshgrid( zt, rkm );
th = ( 2 * pi / 360. ) * th;   % convert to radians
[x, y] = pol2cart( th, r );

x = x + xs * ones( size( x ) );
y = y + ys * ones( size( x ) );

tlt = 20.0 * log10( abs( temp1( 1:2:2*nrr, : )' + sqrt( -1 ) * temp1( 2:2:2*nrr, : )') );
tlt = tlt';

% *** plot ***

figure
surfc( x, y, tlt ); shading flat; ...
colormap( jet ); caxis( [ -90 -60 ] ); colorbar; view( 2 )
xlabel( 'Range (m)' ); ylabel( 'Range (m)' );
title( deblank( pltitl ) )
