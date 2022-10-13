filename= 'Test_00100.shd'


m=1, n=1, p=1; 

% plots a single TL surface in dB
% useage:
% plotshd( filename, m, n, p )
% (m, n, p) optional subplot spec
% '.shd' is the fault file extension if not specified
%
% mbp

% read

[ PlotTitle, freq, atten, Pos, pressure ] = read_shd( filename );
pressure = squeeze( pressure );

zt = Pos.r.depth;
taker = 1:length( Pos.r.range );
rt = Pos.r.range( taker );

rkm = rt / 1000.0;

if ( nargin == 1 )
  %figure
else
  if ( p == 1 )
      figure( 'units', 'normalized', 'outerposition', [ 0 0 1 1 ] ); % first subplot
  else
      hold on   % not first subplot
  end
  subplot( m, n, p )
end

tlt = abs( pressure );
ii = find( tlt < 1e-10 );                      % find places where pressure vanishes
icount = find( tlt > 1e-6 );                % for stats, only these values count
tlt( ii ) = 1e-10;               % and set it to a small number
tlt = -20.0 * log10( tlt );          % so there's no error when we take the log

% compute some statistics to automatically set the color bar

tlmed = median( tlt( icount ) );    % median value
tlstd = std( tlt( icount ) );       % standard deviation
tlmax = tlmed + 0.75 * tlstd;       % max for colorbar
tlmax = 10 * round( tlmax / 10 );   % make sure the limits are round numbers
tlmin = tlmax - 50;                 % min for colorbar

% optionally remove cylindrical spreading:
% tlt = tlt + ones( nrd, 1 ) * 10.0 * log10( rt )';

tej = flipud( jet );  % 'jet' colormap reversed
if ( size( tlt, 1 ) > 1 && size( tlt, 2 ) > 1 )
    pcolor( rt, zt, tlt );  ...
        shading flat; colormap( tej );
    caxis( [ tlmin, tlmax ] ); %colorbar( 'horiz' );
    set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature
    xlabel( 'Range (m)' ); ylabel( 'Depth (m)' );
    title( deblank( PlotTitle ) )
else
    if ( size( tlt, 2 ) == 1 )
        plot( rt, tlt )
        xlabel( 'Range (m)' ); ylabel( 'TL (dB)' )
        set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature
        title( deblank( PlotTitle ) )
    else
        plot( tlt, zt )
        set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature
        set( gca, 'Xdir', 'Reverse' )
        xlabel( 'TL (dB)' ); ylabel( 'Depth (m)' );
        title( deblank( PlotTitle ) )
        
    end
end
title( 'BELLHOPÏà¸É´«²¥ËðÊ§' )
drawnow
%set( gcf, 'PaperPosition', [ 0.25 0.25 6.0 3.0 ] )