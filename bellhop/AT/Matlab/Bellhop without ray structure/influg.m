function influg( ibeams, zs, alpha, RunType, Dalpha  )

global xv TrayV pV qV tauV Rfa cV Nsteps omega depthT depthB rhoB rhoT cpT cpB TopOpt MxN
global sd Nsd rd Nrd rr Nrr
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

for is = 1 : max( Nsteps( ibeams ) ) - 1
   if ( is > 1 )
      icaus = find( ( ( qV( :, 1, is ) < 0.0 ) & ( qV( :, 1, is - 1 ) >= 0.0 ) ) | ...
         ( ( qV( :, 1, is ) > 0.0 ) & ( qV( :, 1, is - 1 ) <= 0.0 ) ) );
      caustic( icaus ) = i * caustic( icaus );   % apply the caustic phase change
   end
   
   % *** find indices of bracketted receiver ranges ***
   % following assume r is uniformly spaced
   ib = uint16( find( Nsteps > is ) ); % only process beams for which step, is, has not gone past last value for that beam
   deltar = ( rr( Nrr ) - rr( 1 ) ) / ( Nrr - 1 );
   
   ir1 = ceil(  1 + ( xv( ib, 1, is     ) - rr( 1 ) ) / deltar );
   ir2 = floor( 1 + ( xv( ib, 1, is + 1 ) - rr( 1 ) ) / deltar );
   ir1 = max( ir1, 1 );
   ir2 = min( ir2, Nrr );
   
   % only keep beams that bracket a set of receivers
   ii  = find( ir2 >= ir1 );
   ir1 = ir1( ii );
   ir2 = ir2( ii );
   ib  = ib(  ii );
   
   if (  isempty( ii ) == 0 )
      for ir = 1 : max( ir2 - ir1 + 1 )   % looping across receiver ranges
         irt = ir1 + ir - 1;
         
         % only keep beams that go that far in range
         ii  = uint16( find( irt <= ir2 ) );
         irt = irt( ii );
         ir1 = ir1( ii );
         ir2 = ir2( ii );
         ib  = ib(  ii );
         
         w = ( rr( irt )            - xv( ib, 1, is ) ) ./ ...
             ( xv( ib, 1, is + 1 )  - xv( ib, 1, is ) );
         
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
            ib1 = uint16( find( Adeltaz < RadMax ) );   % take subset of beams that contributes
            
            if ( isempty( ib1 ) == 0 )
               const = Rat1( ib( ib1 ) ) .* Rfa( ib( ib1 ), is  );
               const = const .* sqrt( c( ib1 ) ./ abs( q( ib1 ) ) ) .* A( ib1 ) .* caust( ib1 );
               
               %         const = Rat1( ib( ib1 ) ) .* sqrt( c( ib1 ) ./ abs( q( ib1 ) ) ) .* A( ib1 ) .* ...
               %         caust( ib1 ) .* Rfa( ib( ib1 ), is  );
               amp   = const .* ( RadMax( ib1 ) - Adeltaz( ib1 ) );
               delay = tau( ib1 ) + Tray( ib1, 2 ) .* deltaz( ib1 );
               
               switch( RunTypeE )
                  case ( 'E' )      % eigenrays
                     %WRTRAY( xv, Nsteps, DepthT, DEPTHB )
                     
                  case ( 'A' )      % arrivals
                     angle  = 180 / pi * atan2( Tray( ib1, 2 ), Tray( ib1, 1 ) );
                     AddArr( omega, ird, irt( ib1 ), amp, delay, angle, MxNArr );
                  case ( 'C'  )     % coherent TL
                     contri = amp .* exp( -i * omega * delay );
                     % need to do this as a loop because the contributions
                     % may be scattered or not (there may be duplicate indices in ib1 )
                     for ii = 1 : length( contri )
                        U( ird, irt( ib1( ii ) ) ) = U( ird, irt( ib1( ii ) ) ) + contri( ii );
                     end
                     
                  otherwise      % incoherent/semi-coherent TL
                     U( id,  ir ) = U( id, ir ) + abs( amp )^2 * ( RadMax - Adeltaz( id ) );
               end
            end
         end   % next receiver depth
      end   % next receiver range
   end
end   % next receiver range
