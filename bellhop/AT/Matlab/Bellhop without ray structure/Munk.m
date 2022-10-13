function [ c, gradc, crr, crz, czz, Layer ]= ssp( x,  TopOpt ) 

%global NSSP, Layer
%ZSSPV( MxSSP ), CSSPV( MxSSP ), CZV( MxSSP ), N2V( MxSSP ), N2ZV( MxSSP ), CVS( 4, MxSSP )

global zSSPV cSSPV czV

% ssp
% tabulates the sound speed profile
Npts = length( x( :, 1 ) );

Layer = ones( Npts, 1 );

% following logic does search to identify a layer for each ray
% if only one layer is used, then you can disable this ...
%Layer = find( x( :, 2 ) > zSSPV( : ) );

[ z, I ] = sort( x( :, 2 ) );   % sort the rays by depth so that we can identify their layers sequentially

ilayer = 1;
NSSPLayers = length( zSSPV ) - 1;

for iz = 1 : Npts
  while ( z( iz ) > zSSPV( ilayer + 1 ) & ilayer < NSSPLayers )
    ilayer = ilayer + 1;
  end
 Layer( iz ) = ilayer;
end

Layer( I ) = Layer; % re-order info to match original ray sequence

z = x( :, 2 );

c0 = 1500.0;

x    = 2.0 * ( z - 1300.0 ) / 1300.0;
DxDz = 2.0 / 1300.0;
c   = c0 * ( 1.0 + 0.00737 * ( x - 1.0 + exp( -x ) ) );
cz  = c0 * 0.00737 * ( 1.0 - exp( -x ) ) .* DxDz;
czz = c0 * 0.00737 * exp( -x ) .* DxDz ^2;

%c = c';
gradc = [ zeros(Npts, 1 ) cz ];

crr = zeros( Npts, 1 ) ;
crz = zeros( Npts, 1 );


