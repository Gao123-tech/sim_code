% Munk profile test cases
% mbp

bellhop( 'MunkB_ray' )
figure
plotray( 'MunkB_ray' )

bellhop( 'MunkB_eigenray' )
figure
plotray( 'MunkB_eigenray' )

bellhop( 'MunkB_Coh' )
plotshd( 'MunkB_Coh.shd', 2, 2, 1 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhop( 'MunkB_gb' )
plotshd( 'MunkB_gb.shd', 2, 2, 2 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

kraken( 'MunkK' )
plotshd( 'MunkK.shd', 2, 2, 3 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

scooter( 'MunkS' )
plotshd( 'MunkS.mat', 2, 2, 4 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

% tests of incoherent, semi-coherent options

bellhop( 'MunkB_Coh' )
plotshd( 'MunkB_Coh.shd', 3, 2, 1 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhop( 'MunkB_Coh_gb' )
plotshd( 'MunkB_Coh_gb.shd', 3, 2, 2 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhop( 'MunkB_Semi' )
plotshd( 'MunkB_Semi.shd', 3, 2, 3 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhop( 'MunkB_Semi_gb' )
plotshd( 'MunkB_Semi_gb.shd', 3, 2, 4 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhop( 'MunkB_Inc' )
plotshd( 'MunkB_Inc.shd', 3, 2, 5 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhop( 'MunkB_Inc_gb' )
plotshd( 'MunkB_Inc_gb.shd', 3, 2, 6 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

% test of Green's function plotting
%figure;
%plotgrn( 'MunkS.grn' )
