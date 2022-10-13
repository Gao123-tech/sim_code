% Surface duct test cases
% mbp

bellhopM( 'sductB' )
plotshd( 'sductB.mat', 2, 2, 1 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhopM( 'sductB_gb' )
plotshd( 'sductB_gb.mat', 2, 2, 2 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

kraken( 'sductK' )
plotshd( 'sductK.shd', 2, 2, 3 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

scooterM( 'sductS' )
plotshd( 'sductS.mat', 2, 2, 4 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )
