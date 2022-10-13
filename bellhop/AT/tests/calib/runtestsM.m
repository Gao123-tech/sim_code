% calibration test cases
% mbp

% isovelocity calibration case

bellhopM( 'calibB' )
plotshd( 'calibB.mat', 2, 2, 1 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

bellhopM( 'calibB_gb' )
plotshd( 'calibB_gb.mat', 2, 2, 2 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

kraken( 'calibK' )
plotshd( 'calibK.shd', 2, 2, 3 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

scooterM( 'calibS' )
plotshd( 'calibS.mat', 2, 2, 4 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

% gradient calibration case

bellhopM( 'calibBgrad' )
plotshd( 'calibBgrad.mat', 2, 2, 1 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

bellhopM( 'calibBgrad_gb' )
plotshd( 'calibBgrad_gb.mat', 2, 2, 2 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

kraken( 'calibKgrad' )
plotshd( 'calibKgrad.shd', 2, 2, 3 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

scooterM( 'calibSgrad' )
plotshd( 'calibSgrad.mat', 2, 2, 4 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )
