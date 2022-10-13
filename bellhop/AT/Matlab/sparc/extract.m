function [ Green, RTSrd, RTSrr, Itout ] = extract( U0, U1, Ik, time, Itout, rkT, deltak, rr, Pos, Z, WR, Ird, Green, RTSrd, RTSrr, tout, deltat, Ntout, Bdry, V )

% Extract solution (snapshot, vertical or horizontal array)

% *** Three cases: snapshot, vertical, horizontal time series ***

while ( Itout <= Ntout && time + deltat >= tout( Itout ) )
    % above can access tout( ntout + 1 ) which is outside array dimension but result is irrelevant
    wt = ( tout( Itout ) - time ) / deltat;  % Weight for temporal interpolation:

    switch ( Bdry.Top.Opt(4:4) )
        case ( 'S' ) % *** Case of a snapshot ***
            const = exp( i * rkT * V * tout( Itout ) );

            UT1 = U0( Ird ) + WR .* ( U0( Ird+1 ) - U0( Ird ) );   % Linear interpolation in depth
            UT2 = U1( Ird ) + WR .* ( U1( Ird+1 ) - U1( Ird ) );
            Green( Itout, :, Ik ) = const * ( UT1 + wt * ( UT2 - UT1 ) );   % Linear interpolation in time

        case ( 'D' ) % *** Case of an RTS (vertical array) ***
            %const = -sqrt( 2.0 ) * deltak * sqrt( rkT ) * cos( rkT * rr( 1 ) - pi / 4.0 );
            const =  sqrt( 2.0 ) * deltak * sqrt( rkT ) * exp( i*( rkT * ( V * tout( Itout ) - rr( 1 ) ) + pi / 4.0 ) );

            UT1 = U0( Ird ) + WR .* ( U0( Ird+1 ) - U0( Ird ) );   % Linear interpolation in depth
            UT2 = U1( Ird ) + WR .* ( U1( Ird+1 ) - U1( Ird ) );
            U   = UT1 + wt * ( UT2 - UT1 );                        % Linear interpolation in time
            RTSrd( :, Itout ) = RTSrd( :, Itout ) + real( const * U );

        case ( 'R' ) % *** Case of an RTS (horizontal array) ***
            if ( time >= tout( 1 ) )
                % Linear interpolation in depth
                I = Ird( 1 );
                T = ( rd( 1 ) - Z( I ) ) / ( Z( I + 1 ) - Z( I ) );

                UT1 = U0( I ) + T * ( U0( I+1 ) - U0( I ) );
                UT2 = U1( I ) + T * ( U1( I+1 ) - U1( I ) );

                % Linear interpolation in time
                U = UT1 + wt * ( UT2 - UT1 );
                const = sqrt( 2.0 ) * exp( i * rkT * V * time );

                RTSrr( :, Itout ) = RTSrr( :, Itout ) + real( deltak * U *  ...
                    const * exp( i * ( -rkT * r + pi / 4.0 ) ) * sqrt( rkT / rr ) );

            end
    end
    Itout = Itout + 1;
end

