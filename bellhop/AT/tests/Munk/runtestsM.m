% Munk profile test cases
% mbp

bellhopM( 'MunkB_ray' )
%figure
%plotray( 'MunkB_ray' )

%bellhop( 'MunkB_eigenray' )
%figure
%plotray( 'MunkB_eigenray' )

bellhopM( 'MunkB_Coh' )
plotshd( 'MunkB_Coh.mat', 2, 2, 1 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhopM( 'MunkB_gb' )
plotshd( 'MunkB_gb.mat', 2, 2, 2 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

kraken( 'MunkK' )
plotshd( 'MunkK.shd', 2, 2, 3 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

scooterM( 'MunkS' )
plotshd( 'MunkS.mat', 2, 2, 4 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

% tests of incoherent, semi-coherent options

bellhopM( 'MunkB_Coh' )
plotshd( 'MunkB_Coh.mat', 3, 2, 1 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhopM( 'MunkB_Coh_gb' )
plotshd( 'MunkB_Coh_gb.mat', 3, 2, 2 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhopM( 'MunkB_Semi' )
plotshd( 'MunkB_Semi.mat', 3, 2, 3 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhopM( 'MunkB_Semi_gb' )
plotshd( 'MunkB_Semi_gb.mat', 3, 2, 4 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhopM( 'MunkB_Inc' )
plotshd( 'MunkB_Inc.mat', 3, 2, 5 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

bellhopM( 'MunkB_Inc_gb' )
plotshd( 'MunkB_Inc_gb.mat', 3, 2, 6 )
caxis( [ 50 100 ] ); colorbar( 'horiz' )

% test of Green's function plotting
%figure;
%plotgrn( 'MunkS.grn' )
