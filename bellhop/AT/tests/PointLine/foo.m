% runtests
% These tests verify the point vs. line source options

clear all

% exact solution
clear all
freq = 2000;
k0 = 2 * pi * freq / 1500.0;
zs = 50;
zr = 0:1:200;
rr = 1:1:500;
for iz = 1: length( zr )
   rmat1 = sqrt( rr.^2 + (zr( iz ) - zs )^2 );
   ppoint( iz, : ) = exp( i * k0 * rmat1 ) ./ rmat1;
   pline( iz, : ) = besselh( 0, k0 * rmat1 );
end

pline = sqrt( pi / 2 ) * pline; %normalization to roughly match acoustics toolbox

figure
jet = colormap( jet );
tej = flipud( jet );
colormap( tej );

subplot( 2, 1, 1 )
pcolor( rr, zr, -20 * log10( abs( ppoint ) ) ); shading interp; view( 0, -90 )
caxis( [ 40 80 ] );colorbar
title( 'Exact point source solution' )

subplot( 2, 1, 2 )
pcolor( rr, zr, -20 * log10( abs( pline ) ) ); shading interp; view( 0, -90 )
caxis( [  20 45 ] );colorbar
title( 'Exact line source solution' )

figure
plot( rr, abs( ppoint( 1, : ) ) )
hold on
plot( rr, abs( pline( 1, : ) ) )
