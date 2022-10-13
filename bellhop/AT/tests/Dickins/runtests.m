% Dickins seamount test
% mbp

bellhop( 'DickinsB' )
plotshd( 'DickinsB.shd', 2, 2, 1 )
caxis( [ 70 120 ] ); colorbar( 'horiz' )

bellhop( 'DickinsFlatB' )
plotshd( 'DickinsFlatB.shd', 2, 2, 2 )
caxis( [ 70 120 ] ); colorbar( 'horiz' )

kraken( 'DickinsFlatK' )
plotshd( 'DickinsFlatK.shd', 2, 2, 3 )
caxis( [ 70 120 ] ); colorbar( 'horiz' )

scooter( 'DickinsFlatS' )
plotshd( 'DickinsFlatS.mat', 2, 2, 4 )
caxis( [ 70 120 ] ); colorbar( 'horiz' )

