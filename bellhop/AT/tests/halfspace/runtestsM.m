% runtests
% These tests verify the surface and bottom reflection coefficents are incorporated correctly

clear all

scooterM( 'lower_halfS' )
plotshd( 'lower_halfS.mat', 2, 2, 1 );
caxis( [ 40 80 ] ); colorbar

bellhopM( 'lower_halfB' )
plotshd( 'lower_halfB.mat', 2, 2, 2 );
caxis( [ 40 80 ] ); colorbar

scooterM( 'upper_halfS' )
plotshd( 'upper_halfS.mat', 2, 2, 3 );
caxis( [ 40 80 ] ); colorbar

bellhopM( 'upper_halfB' )
plotshd( 'upper_halfB.mat', 2, 2, 4 );
caxis( [ 40 80 ] ); colorbar
