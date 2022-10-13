% Dickins seamount test
% mbp

bellhopM( 'DickinsB' )
plotshd( 'DickinsB.mat', 2, 2, 1 )
caxis( [ 70 120 ] ); colorbar( 'horiz' )

bellhopM( 'DickinsFlatB' )
plotshd( 'DickinsFlatB.mat', 2, 2, 2 )
caxis( [ 70 120 ] ); colorbar( 'horiz' )

kraken( 'DickinsFlatK' )
plotshd( 'DickinsFlatK.shd', 2, 2, 3 )
caxis( [ 70 120 ] ); colorbar( 'horiz' )

scooterM( 'DickinsFlatS' )
plotshd( 'DickinsFlatS.mat', 2, 2, 4 )
caxis( [ 70 120 ] ); colorbar( 'horiz' )

