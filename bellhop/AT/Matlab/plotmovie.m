function plotmovie( filename )

% plots a single TL surface in dB
% useage: plotmovie filename
% where filename is a .mat file without the extension

% open the file
%eval( [ 'load ' filename ] );
[ PlotTitle, freq, atten, Pos, p ] = read_shd( filename );
p = squeeze( p );

nsd = length( Pos.s.depth )
nrd = length( Pos.r.depth )
nrr = length( Pos.r.range )

figure( 'units', 'normalized', 'outerposition', [ 0 0 0.5 0.5 ] );  % create full screen figure
colormap( jet )

newmap      = colormap;
newshortmap = newmap( 1:63, : );

for i = 1:nsd
   tlt = squeeze( real( p( i, :, : ) ) );
   % zero out first column for SPARC run
   %tlt( :, 1 ) = zeros( nrd, 1 );
   maxtlt = max( max( abs( tlt ) ) );
   maxtlt = 0.2 * max( maxtlt, 0.000001 );
   %maxtlt = 0.02/i;

   pcolor( Pos.r.range, Pos.r.depth, tlt ); caxis( [ -maxtlt, maxtlt ] ); ...
   shading interp; colormap( newshortmap ); colorbar;
   xlabel( 'Range (m)' ); ylabel( 'Depth (m)' );
   title( deblank( PlotTitle ) )
   set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature
   drawnow
   if i == 1
      M = moviein( nsd ); % initialize movie storage
   end
   
   A = getframe( gcf );
   M( :, i ) = A;

end

%save movie M
%movie2avi( M, 'movie', 'Quality', 100 );
