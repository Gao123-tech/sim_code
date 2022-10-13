function [ cp, cs,rho ] = profil( Depth, Med, N1, Freq, SSPType, AttenUnit )

% return the SSP values at user specified points

ILoc = Loc( Med )
N    = N1 - 1

ZT = linspace( Z( ILoc ), Z( ILoc + 1 ), N );
cp = interp( ZT, cpv );
cs = interp( ZT, csv );
rho = interp( ZT, rhov );

