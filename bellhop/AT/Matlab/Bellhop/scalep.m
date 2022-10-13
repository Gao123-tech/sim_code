function U = scalep( Dalpha, c, r, U, Nr, RunType, TopOpt, freq )

% Scale the pressure field

% Compute scale factor
switch ( RunType(2:2) )
case ( 'C' )
   const = -Dalpha * sqrt( freq ) / c;
case ( 'R' )
   const = -Dalpha * sqrt( freq ) / c;
otherwise
   const = -1.0;
end

% Thorpe attenuation?
if ( TopOpt(4:4) == 'T' )
   f2 = ( freq / 1000.0 )^2;
   alpha = 40.0 * f2 / ( 4100.0 + f2 ) + 0.1 * f2 / ( 1.0 + f2 );
   alpha = alpha / 914.4;     % dB / m
   alpha = alpha / 8.6858896; % Nepers / m
else
   alpha = 0.0;
end

% For incoherent RunType, convert intensity to pressure
if ( RunType(1:1) ~= 'C' )
    U = sqrt( real( U ) );
end

% add in attenuation
for ir = 1 : Nr
   if ( r( ir ) ~= 0 )
      factor = const * exp( -alpha * r( ir ) ) / sqrt( r( ir ) );
   else
      factor = 0.0;
   end
   U( :, ir ) = factor * U( :, ir );
end