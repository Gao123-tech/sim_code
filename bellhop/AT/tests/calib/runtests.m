% calibration test cases
% mbp

% isovelocity calibration case

bellhop( 'calibB' )
plotshd( 'calibB.shd', 2, 2, 1 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

bellhop( 'calibB_gb' )
plotshd( 'calibB_gb.shd', 2, 2, 2 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

kraken( 'calibK' )
plotshd( 'calibK.shd', 2, 2, 3 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

scooter( 'calibS' )
plotshd( 'calibS.mat', 2, 2, 4 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

% gradient calibration case

bellhop( 'calibBgrad' )
plotshd( 'calibBgrad.shd', 2, 2, 1 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

bellhop( 'calibBgrad_gb' )
plotshd( 'calibBgrad_gb.shd', 2, 2, 2 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

kraken( 'calibKgrad' )
plotshd( 'calibKgrad.shd', 2, 2, 3 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

scooter( 'calibSgrad' )
plotshd( 'calibSgrad.mat', 2, 2, 4 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )
