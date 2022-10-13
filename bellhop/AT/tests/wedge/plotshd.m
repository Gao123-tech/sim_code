% plots a single TL surface in dB
% compare results to Fig. 6.8, JKPS on p. 376

clear all

% read

[ pltitl, nsd, nrd, nrr, sd, rd, rr, pressure ] = read_shd( 'wedge.shd' );

zt = rd;
taker = 1:nrr;
rt = rr( taker );

rkm = rt / 1000.0;

figure

tlt = 20.0 * log10( abs( pressure ) );
pcolor( rkm, zt, tlt );  caxis( [ -80 -40 ] ); ...
shading flat; colormap( jet ); colorbar; view( 0, -90 );
xlabel( 'Range (km)' ); ylabel( 'Depth (m)' );
title( deblank( pltitl ) )
%set(1,'PaperPosition', [ 0.25 0.25 5.0 3.5 ] )
%print -deps tl.ps

tlt = -tlt;
figure
plot( rkm, tlt( 76, : ) ); axis( [ 0 4 40 90 ] );
xlabel( 'Range (km)' ); ylabel( 'TL (dB)' ); view( 0, -90 )
title( deblank( pltitl ) )

figure
plot( rkm, tlt( 16, : ) ); axis( [ 0 4 40 90 ] );
xlabel( 'Range (km)' ); ylabel( 'TL (dB)' ); view( 0, -90 )
title( deblank( pltitl ) )

%set(2,'PaperPosition', [ 0.25 0.25 5.0 3.5 ] )
%print -deps tlslice.ps
