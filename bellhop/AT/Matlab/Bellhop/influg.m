function influg( ibeams, zs, alpha, RunType, Dalpha  )

global ray Nsteps omega
global Pos
global U MxNArr

% Computes the beam influence, i.e.
% the contribution of a single beam to the complex pressure
% This version uses a beam representation in Cartesian coordinates

U = zeros( Pos.Nrd, Pos.Nrr );

% some ugly code to have RunType 'a' treated like 'A':
RunTypeE = RunType( 1 : 1 );
if ( RunTypeE == 'a' ) 
   RunTypeE = 'A';
end

Nbeams  = max( ibeams );
DS      = sqrt( 2.0 ) * sin( omega * zs * ray( 1 ).Tray( ibeams, 2 ) );   % Lloyd mirror pattern
q0      = ray( 1 ).c( 1 ) / Dalpha;   % Reference for J = Q0 / Q
caustic = ones( Nbeams, 1);   % caustic is multiplied by i after each caustic

if ( RunType( 4:4 ) == 'R' )   % point source
   Rat1 = sqrt( abs( cos( alpha( ibeams ) ) ) );
else
   Rat1 = ones( size( ibeams ) );
end

% *** step along beams ***

for is = 1 : max( Nsteps( ibeams ) ) - 1
   if ( is > 1 )
      icaus = find( ...
         ( ( ray( is ).q( :, 1 ) < 0.0 ) & ( ray( is - 1 ).q( :, 1 ) >= 0.0 ) ) | ...
         ( ( ray( is ).q( :, 1 ) > 0.0 ) & ( ray( is - 1 ).q( :, 1 ) <= 0.0 ) ) );
      caustic( icaus ) = i * caustic( icaus );   % apply the caustic phase change
   end
   
   % *** find indices of bracketted receiver ranges ***
   % following assume r is uniformly spaced
   ib = uint16( find( Nsteps > is ) ); % only process beams for which step, is, has not gone past last value for that beam
   deltar = ( Pos.r.range( Pos.Nrr ) - Pos.r.range( 1 ) ) / ( Pos.Nrr - 1 );
   
   ir1 = ceil(  1 + ( ray( is     ).x( ib, 1 ) - Pos.r.range( 1 ) ) / deltar + eps( ray( is ).x( ib, 1 ) ) );
   ir2 = floor( 1 + ( ray( is + 1 ).x( ib, 1 ) - Pos.r.range( 1 ) ) / deltar );
   ir1 = max( ir1, 1 );
   ir2 = min( ir2, Pos.Nrr );
   
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
         
         w = ( Pos.r.range( irt )        - ray( is ).x( ib, 1 ) ) ./ ...
             ( ray( is + 1 ).x( ib, 1 )  - ray( is ).x( ib, 1 ) );
         
         clear Tray
         
         z            = ray( is ).x(    ib, 2 ) + w .* ( ray( is + 1 ).x(    ib, 2 ) - ray( is ).x(    ib, 2 ) );
         Tray( :, 1 ) = ray( is ).Tray( ib, 1 ) + w .* ( ray( is + 1 ).Tray( ib, 1 ) - ray( is ).Tray( ib, 1 ) );
         Tray( :, 2 ) = ray( is ).Tray( ib, 2 ) + w .* ( ray( is + 1 ).Tray( ib, 2 ) - ray( is ).Tray( ib, 2 ) );
         c            = ray( is ).c(    ib    ) + w .* ( ray( is + 1 ).c(    ib    ) - ray( is ).c(    ib    ) );
         q            = ray( is ).q(    ib, 1 ) + w .* ( ray( is + 1 ).q(    ib, 1 ) - ray( is ).q(    ib, 1 ) );
         tau          = ray( is ).tau(  ib    ) + w .* ( ray( is + 1 ).tau(  ib    ) - ray( is ).tau(  ib    ) );
         
         tr           = Tray( :, 1 ) .* c;
         
         % identify beams that have crossed a caustic
         caust = caustic( ib );
         icaus = find( ( ( q( : ) < 0.0 ) & ( ray( is ).q( ib( : ), 1 ) >= 0.0 ) ) | ...
                       ( ( q( : ) > 0.0 ) & ( ray( is ).q( ib( : ), 1 ) <= 0.0 ) ) );
         caust( icaus ) = i * caustic( ib( icaus ) );   % apply the caustic phase change
         
         A  = abs( tr .* q0 ./ q );
         RadMax = 1.0 ./ A;
         
         for ird = 1 : Pos.Nrd
            deltaz  = Pos.r.depth( ird ) - z;    % ray to rcvr distance
            Adeltaz = abs( deltaz );
            ib1 = uint16( find( Adeltaz < RadMax ) );   % take subset of beams that contributes
            
            if ( isempty( ib1 ) == 0 )
               const = Rat1( ib( ib1 ) ) .* ray( is ).Rfa( ib( ib1 ) );
               const = const .* sqrt( c( ib1 ) ./ abs( q( ib1 ) ) ) .* A( ib1 ) .* caust( ib1 );
               if ( RunTypeE == 'S' ) % Coherent, semi-coherent, or incoherent TL:
                  const = DS( ib( ib1 ) ) .* const;
               end
         
                
               % const = Rat1( ib( ib1 ) ) .* sqrt( c( ib1 ) ./ abs( q( ib1 ) ) ) .* A( ib1 ) .* ...
               %    caust( ib1 ) .* Rfa( ib( ib1 ), is  );
               amp   = const .* ( RadMax( ib1 ) - Adeltaz( ib1 ) );
               delay = tau( ib1 ) + Tray( ib1, 2 ) .* deltaz( ib1 );
               
               switch( RunTypeE )
                  case ( 'E' )      % eigenrays
                     %WRTRAY( xv, Nsteps, DepthT, DEPTHB )
                     
                  case ( 'A' )      % arrivals
                     % need to do this as a loop because the contributions
                     % may be scattered or not (there may be duplicate indices in ib1 )
                     for ii = 1 : length( amp )
                        angle  = 180 / pi * atan2( Tray( ib1( ii ), 2 ), Tray( ib1( ii ), 1 ) );
                        AddArr( omega, ird, irt( ib1( ii ) ), amp( ii ), delay( ii ), angle, MxNArr );
                     end
                  case ( 'C'  )     % coherent TL
                     contri = amp .* exp( -i * omega * delay );
                     % need to do this as a loop because the contributions
                     % may not be scattered (there may be duplicate indices in ib1 )
                     for ii = 1 : length( contri )
                        U( ird, irt( ib1( ii ) ) ) = U( ird, irt( ib1( ii ) ) ) + contri( ii );
                     end
                     
                  otherwise      % incoherent/semi-coherent TL
                     W = ( RadMax( ib1 ) - Adeltaz( ib1 ) ) ./ RadMax( ib1 );   % hat function: 1 on center, 0 on edge
                     contri = ( abs( amp ) ./ W ).^2 .* W;
                     for ii = 1 : length( contri )
                        U( ird, irt( ib1( ii ) ) ) = U( ird, irt( ib1( ii ) ) ) + contri( ii );
                     end
               end  % close switch on RunTypeE
            end
         end   % next receiver depth
      end   % next receiver range
   end
end   % next receiver range
