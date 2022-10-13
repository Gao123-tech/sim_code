function AddArr( omega, id, ir, amp, delay, angle, MxNArr )

% ADDs the amplitude and delay for an ARRival into a matrix of same
% extra logic to keep only the strongest arrivals

% this one needs lots of work

global NArr DelArr AArr AngArr

Nbeams = length( ir );
Nt     = NArr( id, ir );    % # of arrivals

% 3 cases:
% iold   second of a pair of bracketting rays
% inew   new rays
%   ispace   of which there's space in the table to add a new ray
%   inospace or not

iold = [];
inew = 1: Nbeams;

% search for old arrivals which are possible bracketting pairs
iexist = find( Nt >= 1 );  % only look at slots where there is at least one arrival
if ( ~isempty( iexist ) )
   % search for entries in arrivals table with near same arrival time
   for ibeam = iexist
      isame = find( omega * abs( delay( ibeam ) - DelArr( id, ir( ibeam ), Nt( ibeam ) ) ) < 0.2 );
   end
   iold = iexist( isame );
   inew( iold ) = [];
end

inospace = [];
if ( isempty( inew ) )
   ispace = [];
else
   ispace = inew;
end

%ispace   = find( Nt( inew ) <  MxNArr )   % ones for which there's space to add an arrival
%inospace = find( Nt( inew ) >= MxNArr )   % no space for the rest, we'll replace the weakest arrival


% if new is stronger than weakest, keep new
% otherwise keep existing arrival
%for ibeam = inew
% find index of weakest arrival
%  if ( NArr( id, ir( ibeam ) ) > 0 )
%    [ Amin, IArr ] = min( abs( AArr( id, ir( ibeam ), NArr( id, ir( ibeam ) ) ) ) )

%    if ( abs( amp( inospace( ibeam ) ) ) > abs( AArr( id, ir( inospace( ibeam ) ), IArr ) ) )
%      inospace( ibeam ) = [];  % remove that beam from the list of beams to add
%    end
%  end
%end

if ( ~isempty( inospace ) )   % no space available; substitute new arrival for weakest one
   AArr(   id, ir( inospace ), IArr ) = amp(   inospace );  	 % amplitude
   DelArr( id, ir( inospace ), IArr ) = delay( inospace );	    % delay time
   AngArr( id, ir( inospace ), IArr ) = angle( inospace );	    % angle
end

if ( ~isempty( ispace ) )     % space available; add another arrival to the table
   for ibeam = ispace
      NArr(   id, ir( ibeam )                  ) = Nt(    ibeam ) + 1;    % # of arrivals
      AArr(   id, ir( ibeam ), Nt( ibeam ) + 1 ) = amp(   ibeam );		  % amplitude
      DelArr( id, ir( ibeam ), Nt( ibeam ) + 1 ) = delay( ibeam );		  % delay time
      AngArr( id, ir( ibeam ), Nt( ibeam ) + 1 ) = angle( ibeam );	     % angle
   end
end

if ( ~isempty( iold ) )       % second arrival of a bracketting pair; add in to existing slot
   for ibeam = iold
      AArr(   id, ir( ibeam ), Nt( ibeam ) ) =         AArr(   id, ir( ibeam ), Nt( ibeam ) ) + amp(   ibeam );
      DelArr( id, ir( ibeam ), Nt( ibeam ) ) = 0.5 * ( DelArr( id, ir( ibeam ), Nt( ibeam ) ) + delay( ibeam ) );
      AngArr( id, ir( ibeam ), Nt( ibeam ) ) = angle( ibeam );
   end
end