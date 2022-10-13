% runtests
% These tests verify the surface and bottom reflection coefficents are incorporated correctly

clear all

bellhop( 'omni' )
bellhop( 'shaded' )

plotshd( 'omni.shd', 2, 1, 1 );
caxis( [ 50 100 ] ); colorbar( 'horiz' )

plotshd( 'shaded.shd', 2, 1, 2 );
caxis( [ 50 100 ] ); colorbar( 'horiz' )

