function reflect( I, ib, BeamType, BC, cHS, rhoHS, BotTop, TBdry, NBdry, SSP )
% Reflect a ray/beam off a boundary

global ray omega TopOpt

RADDEG = 180 / pi;   % used to convert radians to degrees

Tg = dot( ray( I ).Tray( ib, : )', TBdry( :, : )' );  % component of ray tangent, along boundary
Th = dot( ray( I ).Tray( ib, : )', NBdry( :, : )' );  % component of ray tangent, normal to boundary

% *** calculate the change in curvature ***
% Based on formulas given by Muller, Geoph. J. R.A.S., 79 (1984).

[ c, gradc, crr, crz, czz, Layer ] = ssp( ray( I ).x( ib, : ), SSP );

cn = gradc( :, 2 ) .* ray( I ).Tray( ib, 1 );
cs = gradc( :, 2 ) .* ray( I ).Tray( ib, 2 );   % assumes gradc( 2 ) = cr = 0

if ( strcmp( BotTop, 'TOP' ) )
    cn = -cn;    % flip sign for top reflection
end

RM = Tg ./ Th;
RN = RM' .* ( 4 * cn - 2 * RM' .* cs ) ./ c;

switch ( BeamType(2:2) )
case ( 'D' )
    RN = 2.0 * RN;
case ( 'Z' )
    RN = 0.0;
end

ray( I ).Tray( ib, 1 ) =  ray( I ).Tray( ib, 1 ) - 2.0 * ( Th .* NBdry( : , 1 )' )';
ray( I ).Tray( ib, 2 ) =  ray( I ).Tray( ib, 2 ) - 2.0 * ( Th .* NBdry( : , 2 )' )';

ray( I ).p( ib, 1 ) = ray( I ).p( ib, 1 ) + ray( I ).q( ib, 1 ) .* RN;
ray( I ).p( ib, 2 ) = ray( I ).p( ib, 2 ) + ray( I ).q( ib, 2 ) .* RN;

% *** account for phase change ***

switch ( BC )
case ( 'R' )                 % rigid
case ( 'V' )                 % vacuum
    ray( I ).Rfa(  ib ) = -ray( I ).Rfa( ib );
case ( 'F' )                 % file
    theInt = RADDEG * abs( atan2( Th, Tg ) );   % angle of incidence (relative to normal to bathymetry)
    %[ theta, RefC, phi ] = RefCO( theInt, rInt, phiInt, Npts  )
    ray( I ).Rfa(  ib ) = ray( I ).Rfa(  ib ) * rInt * exp( i * phiInt );
case ( 'A' )                 % half-space
    GK       = omega * Tg;   % wavenumber in direction parallel to bathymetry
    gamma1SQ = ( omega ./ c'  ).^ 2                      - GK.^ 2;
    gamma2SQ = ( omega  / cHS ) ^ 2 * ones( size( GK ) ) - GK.^ 2;
    gamma1   = sqrt( -gamma1SQ );
    gamma2   = sqrt( -gamma2SQ );
    Refl = ( rhoHS * gamma1 - gamma2 ) ./ ( rhoHS * gamma1 + gamma2 );
    ray( I ).Rfa(  ib ) = Refl.' .* ray( I ).Rfa(  ib );
    
    %jj = find ( abs( ray( I ).Rfa( ib ) ) < 1.0E-5 );   % kill a ray that has lost its energy in reflection
    %ray( I ).Rfa( ib( jj ) ) = 0.0;
        
    end
end
