% runtests
% These tests verify the surface and bottom reflection coefficents are incorporated correctly

clear all

bellhopM( 'omni' )
bellhopM( 'shaded' )

plotshd( 'omni.mat', 2, 1, 1 );
caxis( [ 50 100 ] ); colorbar( 'horiz' )

plotshd( 'shaded.mat', 2, 1, 2 );
caxis( [ 50 100 ] ); colorbar( 'horiz' )

