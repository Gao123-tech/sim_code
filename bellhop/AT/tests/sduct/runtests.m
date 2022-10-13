% Surface duct test cases
% mbp

bellhop( 'sductB' )
plotshd( 'sductB.shd', 2, 2, 1 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhop( 'sductB_gb' )
plotshd( 'sductB_gb.shd', 2, 2, 2 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

kraken( 'sductK' )
plotshd( 'sductK.shd', 2, 2, 3 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

scooter( 'sductS' )
plotshd( 'sductS.mat', 2, 2, 4 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )
