% runtests
% These tests verify the point vs. line source options

clear all

% point source cases
bellhop 'LloydPointB'
plotshd( 'LloydPointB.shd', 4, 2, 1 );
caxis( [ 40 80 ] ); colorbar

bellhop 'LloydPoint_gbtB'
plotshd( 'LloydPoint_gbtB.shd', 4, 2, 3 );
caxis( [ 40 80 ] ); colorbar

copyfile( 'fieldsPoint.flp', 'fields.flp' );
scooter 'LloydS'
plotshd( 'LloydS.mat', 4, 2, 5 );
caxis( [ 40 80 ] ); colorbar

% line source cases
bellhop 'LloydLineB'
plotshd( 'LloydLineB.shd', 4, 2, 2 );
caxis( [ 0 25 ] ); colorbar

bellhop 'LloydLine_gbtB'
plotshd( 'LloydLine_gbtB.shd', 4, 2, 4 );
caxis( [ 0 25 ] ); colorbar

copyfile( 'fieldsLine.flp', 'fields.flp' )
scooter 'LloydS'
plotshd( 'LloydS.mat', 4, 2, 6 );
caxis( [ 0 25 ] ); colorbar

% exact solution
clear all
freq = 5;
k0 = 2 * pi * freq / 1500.0;
zs = 3000;
zr = 0:25:5000;
rr = 25:25:10000;
for iz = 1: length( zr )
   rmat1 = sqrt( rr.^2 + (zr( iz ) - zs )^2 );
   rmat2 = sqrt( rr.^2 + (zr( iz ) + zs )^2 );
   ppoint( iz, : ) = exp( i * k0 * rmat1 ) ./ rmat1 - exp( i * k0 * rmat2 ) ./ rmat2;
   pline( iz, : ) = besselh( 0, k0 * rmat1 ) - besselh( 0, k0 * rmat2 );
end

pline = sqrt( pi / 2 ) * pline; %normalization to roughly match acoustics toolbox

subplot( 4, 2, 7 )
pcolor( rr, zr, -20 * log10( abs( ppoint ) ) ); shading interp; view( 0, -90 )
caxis( [ 40 80 ] );colorbar
title( 'Exact point source solution' )

subplot( 4, 2, 8 )
pcolor( rr, zr, -20 * log10( abs( pline ) ) ); shading interp; view( 0, -90 )
caxis( [  0 25 ] );colorbar
title( 'Exact line source solution' )

