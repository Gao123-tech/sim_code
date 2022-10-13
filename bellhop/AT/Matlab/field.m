function field( filename, zs, modes )
%
% calculates the field using modes produced by KRAKEN
%
% useage: field( filename, zs, modes )
% mbp

if nargin == 2
  [ pltitl, freq, ck, z, phi ] = read_modes( filename );
else
  [ pltitl, freq, ck, z, phi ] = read_modes( filename, modes );
end

% weight modes by mode excitation

isd = find( z >= zs );		% index of source depth
isd = isd( 1 );
[ zs z(isd ) ]

a = phi( isd, : );
phi = phi * diag( a, 0 );	% scale modes by a

% ******************************************************
% form pressure field
% ******************************************************

nr = 500
rmax = 10000.0
deltar = rmax / nr;
r = deltar : deltar : rmax; % nr ranges to 10 km
phase = diag( 1.0 ./ sqrt( ck ) ) * exp( -i * ck * r ) * diag( sqrt( 2 * pi ./ r ) );

p = phi * phase;
tl = 20.0 * log10( abs( p ) );

% remove NaN's
ii = isnan( tl );
tl( ii ) = -200;

% plot
peak = max( max( tl ) )

figure;
pcolor( r, z(1:1:ntot), tl( 1:1:ntot,:) )
caxis( [ peak - 50, peak - 10 ] ); shading interp; colormap( jet )
colorbar;
view( 0, -90 );
xlabel( 'Range (m)' );
ylabel( 'Depth (m)' );
title( pltitl )
