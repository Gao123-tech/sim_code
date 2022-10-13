function influg( Ibeam, zs, alpha, RunType, Dalpha  )

global xv TrayV pV qV tauV Rfa cV Nsteps omega depthT depthB rhoB rhoT cpT cpB TopOpt MxN
global sd Nsd rd Nrd r Nr
global U NArr DelArr AArr AngArr MxNArr

% Computes the beam influence, i.e.
% the contribution of a single beam to the complex pressure
% This version uses a beam representation in Cartesian coordinates

% some ugly code to have RunType 'a' treated like 'A'

RunTypeE = RunType( 1 : 1 );
if ( RunTypeE == 'a' ) 
    RunTypeE = 'A';
end

DS = sqrt( 2.0 ) * sin( omega * zs * TrayV( 2, 1 ) );   % Lloyd mirror pattern
q0 = cV( Ibeam, 1 ) / Dalpha;   % Reference for J = Q0 / Q

caustic = 1;   % caustic is multiplied by i after each caustic
qOld    = 1.0;
Rat1    = sqrt( cos( alpha ) );

% *** Loop over steps ***

RA = xv( 1, 1 );
ir = 1;

for I = 2 : Nsteps( Ibeam )
    RB = xv( Ibeam, 1, I );
    
    % *** Loop over bracketted receiver ranges ***
    while( abs( RB - RA ) > 1.0e-4 & RB > r( ir ) )
        w = ( r( ir ) - RA ) / ( RB - RA );
        
        x     =    xv( Ibeam, :, I-1 ) + w * (    xv( Ibeam, :, I ) -    xv( Ibeam, :, I-1 ) );
        Tray  = TrayV( Ibeam, :, I-1 ) + w * ( TrayV( Ibeam, :, I ) - TrayV( Ibeam, :, I-1 ) );
        c     =    cV( Ibeam,    I-1 ) + w * (    cV( Ibeam, I    ) -    cV( Ibeam,    I-1 ) );
        q     =    qV( Ibeam, 1, I-1 ) + w * (    qV( Ibeam, 1, I ) -    qV( Ibeam, 1, I-1 ) );
        tau   =  tauV( Ibeam,    I-1 ) + w * (  tauV( Ibeam, I    ) -  tauV( Ibeam,    I-1 ) );
        tr = Tray( 1 ) * c;
        
        if q == 0
            A = 0;
            RadMax = 0;
            const = 0;
        else
            A  = abs( tr * q0 / q );
            RadMax = 1.0 / A;
            const = Rat1 * sqrt( c / abs( q ) );
        end
        
        % account for phase shifts at caustics
        if ( ( q < 0.0 & qOld >= 0.0 ) | ( q > 0.0 & qOld <= 0.0 ) )
            caustic = i * caustic;
        end
        
        % Coherent, semi-coherent, or incoherent TL:
        if ( RunTypeE == 'S' )
            const = DS * const;
        end
        
        const = const * A * caustic * Rfa( Ibeam, I );
        %[ ir, r( ir ), RB, x(2) ]
        deltaz = rd - x( 2 );    % ray to rcvr distance
        Adeltaz    = abs( deltaz );
        
        % *** Loop over receiver depths ***
        
        ii = find( Adeltaz < RadMax );
        switch( RunTypeE )
        case ( 'E' )      % eigenrays
            %WRTRAY( xv, Nsteps, DepthT, DEPTHB )
        case ( 'A' )      % arrivals
            ANGLE  = RADDEG * atan2( tray(2), tray(1) )
            %ADDArr( id, ir, amp( id), delay( ir ), ANGLE, AArr, DelArr, AngArr, NArr, MxNArr )
        case ( 'C'  )     % coherent TL
            if ( isempty( ii ) == 0 )
            amp(   ii ) = const * ( RadMax - Adeltaz( ii ) );
            delay( ii ) = tau + Tray( 2 ) * deltaz( ii );
            U( ii, ir ) = U( ii, ir ) + amp( ii )' .* exp( -i * omega * delay( ii )' );
        end
        otherwise      % incoherent/semi-coherent TL
            U( id, ir ) = U( id, ir ) + abs( amp )^2 * ( RadMax - Adeltaz( id ) );
        end
        
        qOld = q;
        ir = ir + 1;
        if ( ir > Nr )
            return
        end
    end   % Next receiver range
    
    RA = RB;
end   % Next step along the ray
