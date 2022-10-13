function plotmode( filename, modes )
%
% plot the modes produced by KRAKEN
%
% useage: plotmode( filename, modes )
% modes is a vector of indices
% can be specified in any order and with duplicates
% mbp

if nargin == 1
  [ pltitl, freq, ck, z, phi ] = read_modes( filename );
else
  [ pltitl, freq, ck, z, phi ] = read_modes( filename, modes );
end

%figure; plot( z, phi );
%xlabel( 'Depth (m)' )
%view( 90, 90 )	% rotate

plot( real( ck ), imag( ck ), '.' );
title( pltitl )
xlabel( 'real( k )' );
ylabel( 'imag( k )' );

x = 1:size( phi, 2 );

figure; imagesc( x, z, real( phi ) ); colorbar
xlabel( 'Mode index' )
ylabel( 'Depth (m)' )
title( deblank( pltitl ) )

figure;
subplot( 1, 2, 1 )
plot( real( phi( :, 1 ) ), z )
xlabel( 'Mode 1' )
ylabel( 'Depth (m)')
set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature

subplot( 1, 2, 2 )
plot( real( phi( :, 2 ) ), z )
xlabel( 'Mode 2' )
ylabel( 'Depth (m)')
set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature

%figure; surf( modes, z, real( phi ) ); colorbar
