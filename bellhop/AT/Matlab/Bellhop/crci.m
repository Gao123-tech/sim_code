function crci = crci( c, alpha, freq, AtUnit )

%     Converts real wave speed and attenuation to a single complex wave speed

%     6 caseS:
%     N for Nepers/meter
%     M for dB/meter      (M for Meters)
%     F for dB/m-kHZ      (F for frequency dependent)
%     W for dB/wavelength (W for Wavelength)
%     Q for Q
%     T for Thorpe

omega = 2.0 * pi * freq;

% *** Convert to Nepers/m ***

alphaT = 0.0;

switch ( AtUnit )
case ( 'N' )   % Nepers/m
   alphaT = alpha;
case ( 'M' )   % dB/meter
   alphaT = alpha / 8.6858896;
case ( 'F' )   % dB/m-kHZ
   alphaT = alpha * freq / 8685.8896;
case ( 'W' )   % dB/wavelength
   if ( c ~= 0.0 )
       alphaT = alpha * freq / ( 8.6858896 * c );
   end
case ( 'Q' )
   if( c * alpha ~= 0.0 )
       alphaT = omega / ( 2.0 * c * alpha );
   end
case ( 'T' )   % Thorpe
   f2     = ( freq / 1000.0 )^2;
   alphaT = 40.0 * f2 / ( 4100.0 + f2 ) + 0.1*f2 / ( 1.0 + f2 );
   alphaT = alphaT / 914.4;     % dB / m
   alphaT = alphaT / 8.6858896; % Nepers / m
end

%     *** Convert Nepers/m to equivalent imaginary sound speed ***

alphaT = alphaT * c^2 / omega;
crci   = c + i * alphaT;
