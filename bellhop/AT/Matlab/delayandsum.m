
% plot replica time series
% mbp 8/96, Universidade do Algarve

clear all

ARRFIL = 'Case1.arr'
STSFIL = 'Ricker.wav'
c = 1500.0
T = 0.5

% which source position, rcvr. position?

%LoadArrfil
[ amp, delay, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, narrmat, nsd, nrd, nr, sd, rd, rr ] ...
    = read_arrivals_asc( ARRFIL );
ir = nr;
ird = nrd;
isd = nsd;

[ sts, sample_rate ] = wavread( STSFIL );
nts = length( sts );

sts_hilbert = hilbert( sts );

% compute the sum

deltat = 1 / sample_rate
nt = round( T / deltat )	% number of time points

last = narrmat( nr, nrd, nsd );

tstart = rr( ir ) / c - 0.1;   % min( delay( ir, 1:last, ird ) )
tend   = tstart + T - deltat;
time   = tstart:deltat:tend;

% compute channel transfer function

rts = zeros( nt, 1 );	% initialize the time series

for iarr = 1 : narrmat( ir, ird, isd )
  
  % compute time index for delay
  Tarr = delay( ir, iarr, ird, isd ) - tstart;
  it = round( Tarr / deltat + 1 );
  % s = Tarr / deltat + 1 - it;		% proportional interval
  
  
  if ( it >= 1 && it <= nt )
    if ( nt - it + 1 <= nts )
      %rts( it : nt ) = rts( it : nt ) + abs( amp( ir, iarr, ird ) ) * sts( 1: nt - it + 1, 1 );
      rts( it : nt ) = rts( it : nt ) + real( amp( ir, iarr, ird ) ) * real( sts_hilbert( 1: nt - it + 1, 1 ) ) ...
                                      - imag( amp( ir, iarr, ird ) ) * imag( sts_hilbert( 1: nt - it + 1, 1 ) );
    else    % trying to read too much data from sts
        'ddd'
      rts( it : it + nts - 1) = rts( it : it + nts - 1 ) + abs( amp( ir, iarr, ird ) ) * sts( 1: nts, 1 );
    end
  end
  
end   % next arrival, iarr.

wavwrite( rts / max( abs( rts ) ) , sample_rate, 'rts.wav' );
