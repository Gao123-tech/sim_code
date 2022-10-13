function [ c, gradc, crr, crz, czz, Layer ]= ssp( x,  TopOpt ) 

global Layer zSSPV cSSPV czV HV

% ssp
% tabulates the sound speed profile
% also returns a vector Layer indicating the layer a depth point is in

Npts = length( x( :, 1 ) );

%Layer = find( x( :, 2 ) > zSSPV( : ) );
%Layer = ones( Npts, 1 );

NLayers = length( zSSPV ) - 1;

[ z, I ] = sort( x( :, 2 ) );

ilayer = 1;
Layer = [];

for iz = 1 : length( z )
  while ( z( iz ) > zSSPV( ilayer + 1 ) & ilayer < NLayers )
    ilayer = ilayer + 1;
  end
  w = z( iz ) - zSSPV( ilayer ); % distance into layer
  c(     I( iz ) ) = cSSPV( ilayer ) + w * czV( ilayer );
  cz(    I( iz ) ) = czV( ilayer );
  Layer( I( iz ) ) = ilayer;
end
  
%c  = interp1( zSSPV, cSSPV, z, 'linear', 'extrap' );
%cz = interp1( zSSPV( 1:end-1), czV, x( :, 2 ), 'nearest', 'extrap' );

c = c';
gradc = [ zeros(Npts, 1 ) cz' ];

crr = zeros( Npts, 1 );
crz = zeros( Npts, 1 );
czz = zeros( Npts, 1 );
