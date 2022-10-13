function reflect( I, ib, Iseg, BeamType, BC, cHS, rhoHS, BotTop, ...
    TBdry, NBdry, theta, RefC, phi, Npts, zSSPV, cSSPV, czV )

global xv TrayV pV qV tauV Rfa cV Nsteps omega depthT depthB rhoB rhoT cpT cpB TopOpt MxN

RADDEG = 180 /pi;   % used to convert radians to degrees

Tg = dot( TrayV( ib, :, I )', TBdry( :, : )' );  % component of ray tangent, along boundary
Th = dot( TrayV( ib, :, I )', NBdry( :, : )' );  % component of ray tangent, normal to boundary

% *** calculate the change in curvature ***
% Based on formulas given by Muller, Geoph. J. R.A.S., 79 (1984).

[ c, gradc, crr, crz, czz, Layer ] = ssp( xv( ib, :, I ), zSSPV, cSSPV, czV, TopOpt );

cn = gradc( :, 2 ) .* TrayV( ib, 1, I );
cs = gradc( :, 2 ) .* TrayV( ib, 2, I );   % assumes gradc( 2 ) = cr = 0

if ( BotTop == 'TOP' )
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

TrayV( ib, 1, I ) =  TrayV( ib, 1, I ) - 2.0 * ( Th .* NBdry( :, 1 )' )';
TrayV( ib, 2, I ) =  TrayV( ib, 2, I ) - 2.0 * ( Th .* NBdry( :, 2 )' )';

pV( ib, 1, I ) = pV( ib, 1, I ) + qV( ib, 1, I ) .* RN;
pV( ib, 2, I ) = pV( ib, 2, I ) + qV( ib, 2, I ) .* RN;

% *** account for phase change ***

switch ( BC )
case ( 'R' )                 % rigid
case ( 'V' )                 % vacuum
    Rfa(  ib, I ) = -Rfa( ib, I );
case ( 'F' )                 % file
    theInt = RADDEG * abs( atan2( Th, Tg ) );   % angle of incidence (relative to normal to bathymetry)
    %[ theta, RefC, phi ] = RefCO( theInt, rInt, phiInt, Npts  )
    Rfa(  ib, I ) = Rfa(  ib, I ) * rInt * exp( i * phiInt );
case ( 'A' )                 % half-space
    GK     = omega * Tg;   % wavenumber in direction parallel to bathymetry
    gamma1SQ = ( omega ./ c'  ).^ 2                      - GK.^ 2;
    gamma2SQ = ( omega  / cHS ) ^ 2 * ones( size( GK ) ) - GK.^ 2;
    gamma1   = sqrt( -gamma1SQ );
    gamma2   = sqrt( -gamma2SQ );
    
    Refl = ( rhoHS * gamma1 - gamma2 ) / ( rhoHS * gamma1 + gamma2 );
    
    if ( abs( Refl ) < 1.0E-5 )   % kill a ray that has lost its energy in reflection
        Rfa(  ib, I ) = 0.0;
    else
        Rfa(  ib, I ) = Refl .* Rfa(  ib, I );
    end
end
