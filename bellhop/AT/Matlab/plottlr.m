function plottlr( filename, rdt )

% plottlr
% plots a single TL slice from the shade file
%
% useage:
% plotslic( filename, rdt )
% where
%   filename is the shadefile (with extension)
%   rdt is the receiver depth in m
%   if rdt is a vector then one plot is generated for each element
% mbp

% read

disp( 'PlotTlr uses the first source depth in the shade file; check OK' )
isd = 1

[ PlotTitle, freq, atten, Pos, pressure ] = read_shd( filename );

rkm = Pos.r.range / 1000.0;          % convert to km

tlt = abs( pressure );	            % this is really the negative of TL
ii = find( tlt == 0 );              % find places where pressure vanishes
tlt( ii ) = max( max( tlt ) ) / 1e10;      % and set it to a small number

tlt = -20.0 * log10( tlt );          % so there's no error when we take the log

% following logic is because interp1 won't interpolate a vector with only 1 element

if ( length( Pos.r.depth ) == 1 )
  tlslice = squeeze( tlt( isd, 1, 1 ) );
else
  tlslice = interp1( Pos.r.depth, squeeze( tlt( isd, :, : ) ), rdt );
end

plot( rkm, tlslice )
set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature
xlabel( 'Range (km)' ); ylabel( 'TL (dB)' );
title( deblank( PlotTitle ) )

% generate legend
for ird = 1: length( rdt )
    legendstr( ird, : ) = [ 'Depth = ', num2str( rdt( ird ) ), ' m' ];
end

legend( legendstr, 'Location', 'Best' )
legend( 'boxoff' )
drawnow
