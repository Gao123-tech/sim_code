function plotshd( filename )

% plots a single TL surface in dB

% open the file
fid = fopen(filename,'r');

% read

pltitl = fgetl( fid );
junk   = fgetl( fid );
junk   = fscanf( fid, '%f', 1 );
nsd    = fscanf( fid, '%i', 1 );
nrd    = fscanf( fid, '%i', 1 );
nrr    = fscanf( fid, '%i', 1 );

sd     = fscanf( fid, '%f', nsd );
rd     = fscanf( fid, '%f', nrd );
rr     = fscanf( fid, '%f', nrr );

isd = 1;
for ii = 1:isd
   temp1   = fscanf( fid, '%f', [ 2 * nrr, nrd ] );
   ii, size(temp1)
end

% complex pressure:

P =  temp1( 1:2:2*nrr, : )' + sqrt( -1 ) * temp1( 2:2:2*nrr, : )';
zt = rd;

taker = 1:nrr;   % ranges to take (subsampling)
rt = rr( taker );
tlt = 20.0 * log10( abs( P( :, taker ) ) );

rkm = rt / 1000.0;   % convert to km

figure
imagesc( rkm, zt, tlt );  caxis( [ -80 -40 ] ); ...
colormap( jet ); colorbar;
xlabel( 'Range (km)' ); ylabel( 'Depth (m)' );
title( deblank( pltitl ) )
%set(1,'PaperPosition', [ 0.25 0.25 5.0 3.5 ] )
%print -deps tl.ps

figure
plot( rkm, tlt( 26, : ) ); axis( [ 0 5 -80 -40 ] );
xlabel( 'Range (km)' ); ylabel( 'TL (dB)' );
title( deblank( pltitl ) )
%set(2,'PaperPosition', [ 0.25 0.25 5.0 3.5 ] )
%print -deps tlslice.ps
