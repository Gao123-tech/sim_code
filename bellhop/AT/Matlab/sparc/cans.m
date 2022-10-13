function [ S, PulseTitle ] = cans( T, omega, Pulse )

%     Computes the source time series

%     T      is the time
%     omega  is some frequency characterizing the pulse
%     Pulse  is the letter indicating which pulse
%     S      is the time series

S = 0.0;
PulseTitle = Pulse; % must return this, even if T<=0

F = omega / ( 2.0 * pi );

if ( T > 0.0 )

    %     Evaluate selected Pulse

    switch ( Pulse(1:1 ) )
        case ( 'P' )   % Pseudo gaussian
            % (peak at 0, support [0, 3F]
            if ( T <= 1.0 / F )
                S = 0.75 - cos( omega * T ) + 0.25*cos( 2.0 * omega * T );
            end
            PulseTitle = 'Pseudo gaussian';

        case ( 'R' )   % Ricker wavelet
            % (peak at F, support [0, 2F]
            U = omega * T - 5.0;
            S = 0.5 * ( 0.25*U*U - 0.5 ) * sqrt( pi ) * exp( -0.25*U*U );
            PulseTitle = 'Ricker wavelet';

        case ( 'A' )    % Approximate Ricker wavelet
            % (peak at F, support [0, 2.5F]
            TC = 1.55 / F;
            if ( T <= TC )
                S = 0.48829  *    cos( 2.0 * pi * T / TC ) ...
                   -0.14128 * 4 * cos( 4.0 * pi * T / TC ) ...
                   +0.01168 * 9 * cos( 6.0 * pi * T / TC );
            end
            PulseTitle = 'Approximate Ricker wavelet';

        case ( 'S' )   % Single sine
            % (peak at F, support [0, infinity], nulls at nF
            if ( T <= 1.0 / F )
                S = sin( omega * T );
            end
            PulseTitle = 'Single sine';

        case ( 'H' )   % Hanning weighted four sine
            % (peak at F, support [0, infinity], first null at about 1.5F
            if ( T <= 4.0 / F )
                S = 0.5 * sin( omega * T ) * ( 1 - cos( omega * T / 4.0 ) );
            end
            PulseTitle = 'Hanning weighted four sine';

        case ( 'N' )   % N-wave
            % (peak at 1 F, support [0, 4F], [0,3F] OK
            if ( T <= 1.0 / F )
                S = sin( omega * T ) - 0.5 * sin( 2.0 * omega * T );
            end
            PulseTitle = 'N-wave';

        case ( 'M' )   % Miracle wave (has a Hilbert transform?)
            % (peak at 0, support [0, infinity]
            A  = 1.0 / ( 6.0 * F );
            T0 = 6.0 * A;
            TS = ( T - T0 ) / A;
            S  = 1.0 / ( 1.0 + TS * TS );
            % HS is the Hilbert transform of the time series
            % HS = TS / ( 1.0 + TS * TS );
            PulseTitle = 'Miracle wave';

        case ( 'G' )   % Gaussian
            % (peak at 0, support [0, infinity]
            % T0 is the peak time
            % A is the 3dB down time
            % NOTE S(0) = exp( -NSIG ** 2 )

            NSIG = 3;
            A  = 1.0 / F / ( 2.0 * NSIG );
            T0 = NSIG * A;
            S  = exp( -( ( T - T0 ) / A ) ^ 2 );
            PulseTitle = 'Gaussian';
        case ( 'T' )   % Tone
            % (peak at F, support [0, infinity]

            if ( T <= 0.4 )
                S = sin( omega * T );
            end
            PulseTitle = 'Tone';
        case ( 'C' )   % Sinc
            % ( uniform spectrum from [0, F]
            S = sin( omega * T ) / ( omega * T );
            PulseTitle = 'Sinc';
    end
end
