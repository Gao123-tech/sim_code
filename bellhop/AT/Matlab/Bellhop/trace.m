function trace( ibeams, deltas, xs, alpha, Amp0, BeamType, zBox, rBox, BotOpt, RunType )

global ray Nsteps Bdry
global xTop tTop nTop NatiPts
global xBot tBot nBot NbtyPts
global SSP
 
% Traces the beam corresponding to a particular take-off angle

% *** Initial conditions ***

[ c, gradc, crr, crz, czz, Layer ] = ssp( xs, SSP );

Nbeams = length( ibeams );
%DistBegTop = zeros( Nbeams, 1 );
%DistEndTop = zeros( Nbeams, 1 );
%DistBegBot = zeros( Nbeams, 1 );
%DistEndBot = zeros( Nbeams, 1 );
Nsteps     = zeros( Nbeams, 1 );

ray( 1 ).c    = ones( Nbeams, 1 ) * c;
ray( 1 ).x    = ones( Nbeams, 1 ) * xs;
ray( 1 ).Tray = [ cos( alpha( ibeams ) ) sin( alpha( ibeams ) ) ] / c;
ray( 1 ).p    = ones( Nbeams, 1 ) * [ 1.0 0.0 ];
ray( 1 ).q    = ones( Nbeams, 1 ) * [ 0.0 1.0 ];
ray( 1 ).tau  = zeros( Nbeams, 1 );
ray( 1 ).Rfa  = Amp0;

% second component of qv is not used in geometric beam tracing
% set I.C. to 0 in hopes of saving run time
if (RunType(2:2) == 'G' )
    ray( 1 ).qv = zeros( Nbeams, 2 ) ;
end

% *** identify the top segment above the source

IsegTopT = find( xTop( 1, 1:NatiPts) <= xs( 1 ) );

if ( IsegTopT( end ) > 0 && IsegTopT( end ) < NatiPts )
    IsegTop  = IsegTopT( end ) * ones( Nbeams, 1 );	% IsegTop MUST LIE IN [ 1, NatiPts-1 ]
else
    disp( 'Fatal Error: Top altimetry undefined above the source' )
end

% *** identify the bottom segment below the source

IsegBotT = find( xBot( 1, 1:NbtyPts) <= xs( 1 ) );

if ( IsegBotT( end ) > 0 && IsegBotT( end ) < NbtyPts )
    IsegBot  = IsegBotT( end ) * ones( Nbeams, 1 );	% IsegBot MUST LIE IN [ 1, NbtyPts-1 ]
else
    disp( 'Fatal Error: Bottom bathymetry undefined below the source' )
end

% *** Trace the beams ***

ib = 1 : Nbeams;
I  = 0;

MaxSteps = 6000000 / Nbeams

while ( length( ib ) > 0 )
    I  = I + 1;
    I0 = I;
    I1 = I + 1;
    if ( I > MaxSteps )
       warning( 'Terminating at code limit of MaxSteps' )
       break
    end
    
    %make sure shape is correct
ray( I1 ).c    = ones( Nbeams, 1 ) * c;
%ray( I1 ).x    = ones( Nbeams, 1 ) * xs;
%ray( I1 ).Tray = [ cos( alpha( ibeams ) ) sin( alpha( ibeams ) ) ] / c;
%ray( I1 ).p    = ones( Nbeams, 1 ) * [ 1.0 0.0 ];
ray( I1 ).q    = ones( Nbeams, 1 ) * [ 0.0 1.0 ];  % need to ensure q(Nbeams, 1) exists in INFLUG for caustic count
ray( I1 ).tau  = zeros( Nbeams, 1 );
ray( I1 ).Rfa  = ones( Nbeams, 1 );
    
          [ ray( I1 ).x( ib, :), ray( I1 ).Tray( ib, :), ray( I1 ).p( ib, :), ray( I1 ).q( ib, :), ray( I1 ).tau( ib ), ray( I1 ).Rfa( ib ), ray( I1 ).c( ib ) ] = ...
      step( ray( I0 ).x( ib, :), ray( I0 ).Tray( ib, :), ray( I0 ).p( ib, :), ray( I0 ).q( ib, :), ray( I0 ).tau( ib ), ray( I0 ).Rfa( ib ), ...
              xTop( :, IsegTop( ib ) )', nTop( :, IsegTop( ib ) )', ...
              xBot( :, IsegBot( ib ) )', nBot( :, IsegBot( ib ) )', deltas, Bdry.Top.Opt );

    % *** New altimetry segment? ***
    
    ii = uint16( find( ray( I1 ).x( ib, 1 ) < xTop( 1, IsegTop( ib )     )' | ...
                       ray( I1 ).x( ib, 1 ) > xTop( 1, IsegTop( ib ) + 1 )' ) );
    if ( isempty( ii ) == 0 )
        for iii = ii'
            IsegTopT = find( xTop( 1, : ) < ray( I1 ).x( ib( iii ), 1 ) );
            if ( isempty( IsegTopT ) == 1 )   % no altimetry point behind us
                IsegTop( ib( iii ) ) = 1;
            else
                IsegTop( ib( iii ) ) = min( IsegTopT( end ), NatiPts - 1);
            end
        end
    end

    % *** New bathymetry segment? ***
    ii = uint16( find( ray( I1 ).x( ib, 1 ) < xBot( 1, IsegBot( ib )     )' | ...
                       ray( I1 ).x( ib, 1 ) > xBot( 1, IsegBot( ib ) + 1 )' ) );
    if ( isempty( ii ) == 0 )
        for iii = ii'
            IsegBotT = find( xBot( 1, : ) < ray( I1 ).x( ib( iii ), 1 ) );
            if ( isempty( IsegBotT ) == 1 )   % no bathymetry point behind us
                IsegBot( ib( iii ) ) = 1;
            else
                IsegBot( ib( iii ) ) = min( IsegBotT( end ), NbtyPts - 1 );
                IsegBot( ib( iii ) );
            end
        end
    end
    
    % *** Reflections? ***
    % Tests that ray at step i is inside, and ray at step i+1 is outside
    % to detect only a crossing from inside to outside
    
    DBegTop( ib, 1 ) = ray( I ).x( ib, 1  ) - xTop( 1, IsegTop( ib ) )';  % vector pointing from top    to ray
    DBegTop( ib, 2 ) = ray( I ).x( ib, 2  ) - xTop( 2, IsegTop( ib ) )';
    DistBegTop( ib ) = dot( DBegTop( ib, : )', nTop( :, IsegTop( ib ) ) );
    
    DEndTop( ib, 1 ) = ray( I1 ).x( ib, 1 ) - xTop( 1, IsegTop( ib ) )';  % vector pointing from top    to ray
    DEndTop( ib, 2 ) = ray( I1 ).x( ib, 2 ) - xTop( 2, IsegTop( ib ) )';
    DistEndTop( ib ) = dot( DEndTop( ib, : )', nTop( :, IsegTop( ib ) ) );
    
    DBegBot( ib, 1 ) = ray( I ).x( ib, 1  ) - xBot( 1, IsegBot( ib ) )';  % vector pointing from bottom to ray
    DBegBot( ib, 2 ) = ray( I ).x( ib, 2  ) - xBot( 2, IsegBot( ib ) )';  % vector pointing from bottom to ray
    DistBegBot( ib ) = dot( DBegBot( ib, : )' , nBot( :, IsegBot( ib ) ) );
    
    DEndBot( ib, 1 ) = ray( I1 ).x( ib, 1 ) - xBot( 1, IsegBot( ib ) )';  % vector pointing from bottom to ray
    DEndBot( ib, 2 ) = ray( I1 ).x( ib, 2 ) - xBot( 2, IsegBot( ib ) )';  % vector pointing from bottom to ray
    DistEndBot( ib ) = dot( DEndBot( ib, : )', nBot( :, IsegBot( ib ) ) );
    
    CrossTop = find( DistBegTop( ib ) < 0.0 & DistEndTop( ib ) >= 0.0 );
    CrossBot = find( DistBegBot( ib ) < 0.0 & DistEndBot( ib ) >= 0.0 );
    
    if     ( isempty( CrossTop ) == 0 )
        BC(1:1) = Bdry.Top.Opt(2:2);
        reflect( I1, ib( CrossTop ), BeamType, BC, Bdry.Top.cp, Bdry.Top.rho, 'TOP', ...
            tTop( :, IsegTop( ib( CrossTop ) )  )', nTop( :, IsegTop( ib( CrossTop ) ) )', SSP )
    end
    
    if ( isempty( CrossBot ) == 0 )
        BC = Bdry.Bot.Opt(1:1);
        reflect( I1, ib( CrossBot ), BeamType, BC, Bdry.Bot.cp, Bdry.Bot.rho, 'BOT', ...
            tBot( :, IsegBot( ib( CrossBot ) )  )', nBot( :, IsegBot( ib( CrossBot ) ) )', SSP )
    end
    
    % *** Has the ray left the box, lost its energy, or exceeded storage limit? ***

    ikill = abs( ray( I1 ).x( ib, 1 ) ) > rBox | ...
                 ray( I1 ).x( ib, 2 )   > zBox | ...
            abs( ray( I1 ).Rfa( ib ) )  < 0.005;
 
    if any( ikill ) > 0
        keep = find( ikill == 0 );
        kill = find( ikill == 1 );
        Nsteps( ib( kill ) ) = I1;
        ib = ib( keep );
    end
    
end   % Next step

% if you fall out of the loop, close up any left over beams
if ( length( ib ) ~= 0 )
    Nsteps( ib ) = I1-1;
end