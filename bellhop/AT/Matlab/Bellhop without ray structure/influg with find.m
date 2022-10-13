function influg( ibeams, zs, alpha, RunType, Dalpha  )

global xv TrayV pV qV tauV Rfa cV Nsteps omega depthT depthB rhoB rhoT cpT cpB TopOpt MxN
global sd Nsd rd Nrd r Nr
global U NArr DelArr AArr AngArr MxNArr

% Computes the beam influence, i.e.
% the contribution of a single beam to the complex pressure
% This version uses a beam representation in Cartesian coordinates

% some ugly code to have RunType 'a' treated like 'A':
RunTypeE = RunType( 1 : 1 );
if ( RunTypeE == 'a' ) 
  RunTypeE = 'A';
end

Nbeams = max( ibeams );

DS = sqrt( 2.0 ) * sin( omega * zs * TrayV( ibeams, 2 ) );   % Lloyd mirror pattern
q0 = cV( 1, 1 ) / Dalpha;   % Reference for J = Q0 / Q

caustic = ones( Nbeams, 1);   % caustic is multiplied by i after each caustic
Rat1    = sqrt( cos( alpha( ibeams ) ) );

% *** step along beams ***

for is = 1 : max( Nsteps( ibeams ) )
  if ( is > 1 )
  icaus = find( ( ( qV( :, 1, is ) < 0.0 ) & ( qV( :, 1, is - 1 ) >= 0.0 ) ) | ...
                ( ( qV( :, 1, is ) > 0.0 ) & ( qV( :, 1, is - 1 ) <= 0.0 ) ) );
  caustic( icaus ) = i * caustic( icaus );   % apply the caustic phase change
  end

  % *** find indices of bracketted receiver ranges ***
  clear irmat
  for ibeam = ibeams
    irvec = find( r' >= xv( ibeam, 1, is    ) & ...
                  r' <  xv( ibeam, 1, is + 1) );
    %ir1 = findfirst( r'              >= xv( ibeam, 1, is    ) );
    %ir2 = findfirst( r( ir1 : end )' >  xv( ibeam, 1, is + 1) );
    % irvec = ir1 : ir1 + ir2-1
    if ( isempty( irvec ) == 0 )
      irmat( ibeam, 1:length( irvec ) ) = irvec;
    end
  end
  
  if ( exist( 'irmat' ) ~= 0 )
    for irt = 1 : size( irmat, 2 )   % looping across receiver ranges
      ib = find( irmat( :, irt ) ~= 0 );
      ir = irmat( ib, irt );
      
      if ( irt == 10000 )
      w = ( r( ir )           - xv( ib, 1, is ) ) ./ ...
        ( xv( ib, 1, is + 1 ) - xv( ib, 1, is ) );
      
      %clear z Tray c q tau tr Adeltaz RadMax
      clear Tray
      z            =    xv( ib, 2, is ) + w .* (    xv( ib, 2, is + 1 ) -    xv( ib, 2, is ) );
      Tray( :, 1 ) = TrayV( ib, 1, is ) + w .* ( TrayV( ib, 1, is + 1 ) - TrayV( ib, 1, is ) );
      Tray( :, 2 ) = TrayV( ib, 2, is ) + w .* ( TrayV( ib, 2, is + 1 ) - TrayV( ib, 2, is ) );
      c            =    cV( ib,    is ) + w .* (    cV( ib,    is + 1 ) -    cV( ib,    is ) );
      q            =    qV( ib, 1, is ) + w .* (    qV( ib, 1, is + 1 ) -    qV( ib, 1, is ) );
      tau          =  tauV( ib,    is ) + w .* (  tauV( ib,    is + 1 ) -  tauV( ib,    is ) );
      
      tr           = Tray( :, 1 ) .* c;
      
      % identify beams that have crossed a caustic
      caust = caustic( ib );
      icaus = find( ( ( q( : ) < 0.0 ) & ( qV( ib( : ), 1, is ) >= 0.0 ) ) | ...
                    ( ( q( : ) > 0.0 ) & ( qV( ib( : ), 1, is ) <= 0.0 ) ) );
      caust( icaus ) = i * caustic( ib( icaus ) );   % apply the caustic phase change
      
      A  = abs( tr .* q0 ./ q );
      RadMax = 1.0 ./ A;
      % Coherent, semi-coherent, or incoherent TL:
      if ( RunTypeE == 'S' )
        const = DS * const;
      end
      
      for ird = 1 : Nrd
        deltaz = rd( ird ) - z;    % ray to rcvr distance
        Adeltaz = abs( deltaz );
        ib1 = find( Adeltaz < RadMax );   % take subset of beams that contributes
        
        if ( isempty( ib1 ) == 0 )
          const = Rat1( ib( ib1 ) ) .* sqrt( c( ib1 ) ./ abs( q( ib1 ) ) ) .* A( ib1 ) .* ...
            caust( ib1 ) .* Rfa( ib( ib1 ), is  );
          amp   = const .* ( RadMax( ib1 ) - Adeltaz( ib1 ) );
          delay = tau( ib1 ) + Tray( ib1, 2 ) .* deltaz( ib1 );
          
          switch( RunTypeE )
          case ( 'E' )      % eigenrays
            %WRTRAY( xv, Nsteps, DepthT, DEPTHB )
            
          case ( 'A' )      % arrivals
            angle  = 180 / pi * atan2( Tray( ib1, 2 ), Tray( ib1, 1 ) );
            AddArr( ird, ir( ib1 ), amp, delay, angle )
            
          case ( 'C'  )     % coherent TL
            contri = amp .* exp( -i * omega * delay );
            % need to do this as a loop because the contributions
            % may be scattered or not (duplicate indicies in ib1 )
            for ib3 = 1 : length( contri )
              U( ird, ir( ib1( ib3 ) ) ) = U( ird, ir( ib1( ib3 ) ) ) + contri( ib3 );
            end
            
          otherwise      % incoherent/semi-coherent TL
            U( id,  ir ) = U( id, ir ) + abs( amp )^2 * ( RadMax - Adeltaz( id ) );
          end
        end
      end
      end   % next receiver depth
      
    end   % next receiver range
  end
end   % next receiver range
