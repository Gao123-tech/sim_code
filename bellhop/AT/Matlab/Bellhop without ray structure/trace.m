function trace( ibeams, deltas, xs, alpha, BeamType, zBox, rBox, BotOpt, RunType )

global xv TrayV pV qV tauV Rfa cV Nsteps omega depthT depthB rhoB rhoT cpT cpB TopOpt MxN
global xTop tTop nTop NatiPts
global xBot tBot nBot NbtyPts
global thetaTop RTop phiTop NTopPts thetaBot RBot phiBot NBotPts
global zSSPV cSSPV czV
 
% Traces the beam corresponding to a particular take-off angle

% *** Initial conditions ***

[ c, gradc, crr, crz, czz, Layer ] = ssp( xs, zSSPV, cSSPV, czV, TopOpt );

Nbeams = length( ibeams );

cV(       :, 1 ) = ones( Nbeams, 1 ) * c;
xv(    :, :, 1 ) = ones( Nbeams, 1 ) * xs;
TrayV( :, :, 1 ) = [ cos( alpha( ibeams ) ) sin( alpha( ibeams ) ) ] / c;
pV(    :, :, 1 ) = ones( Nbeams, 1 ) * [ 1.0 0.0 ];
qV(    :, :, 1 ) = ones( Nbeams, 1 ) * [ 0.0 1.0 ];
tauV(     :, 1 ) = 0.0;
Rfa(      :, 1 ) = 1.0;

% second component of qv is not used in geometric beam tracing
% set I.C. to 0 in hopes of saving run time
if (RunType(2:2) == 'G' )
    qV( :, :,  1 ) = zeros( Nbeams, 2 ) ;
end

% *** identify the top segment above the source

IsegTopT = find( xTop( 1, 1:NatiPts) <= xs( 1 ) );

if ( IsegTopT( end ) > 0 & IsegTopT( end ) < NatiPts )
    IsegTop  = IsegTopT( end ) * ones( Nbeams, 1 );	% IsegTop MUST LIE IN [ 1, NatiPts-1 ]
else
    disp( 'Fatal Error: Top altimetry undefined above the source' )
end

% *** identify the bottom segment below the source

IsegBotT = find( xBot( 1, 1:NbtyPts) <= xs( 1 ) );

if ( IsegBotT( end ) > 0 & IsegBotT( end ) < NbtyPts )
    IsegBot  = IsegBotT( end ) * ones( Nbeams, 1 );	% IsegBot MUST LIE IN [ 1, NbtyPts-1 ]
else
    disp( 'Fatal Error: Bottom bathymetry undefined below the source' )
end

% *** Trace the beams ***

ib = 1 : Nbeams;
I = 0;

for ISTEP = 1 : MxN - 1
    I  = I + 1;
    I0 = I;
    I1 = I + 1;
    
    [ xv( ib, :, I1 ), TrayV( ib, :, I1 ), pV( ib, :, I1 ), qV( ib, :, I1 ), ...
            tauV( ib, I1 ), Rfa( ib, I1 ), cV( ib, I1 ) ] = ...
        step( xv( ib, :, I0 ), TrayV( ib, :, I0 ), pV( ib, :, I0 ), qV( ib, :, I0 ), ...
              tauV( ib, I0  ), Rfa( ib, I0  ), cV( ib, I0  ), ...
              xTop( :, IsegTop( ib ) )', nTop( :, IsegTop( ib ) )', ...
              xBot( :, IsegBot( ib ) )', nBot( :, IsegBot( ib ) )', deltas, TopOpt );
    
    % *** New altimetry segment? ***
    
    ii = uint16( find( xv( ib, 1, I1 ) < xTop( 1, IsegTop( ib )     )' | ...
        xv( ib, 1, I1 ) > xTop( 1, IsegTop( ib ) + 1 )' ) );
    if ( isempty( ii ) == 0 )
        for iii = ii'
            IsegTopT = find( xTop( 1, : ) < xv( ib( iii ), 1, I1 ) );
            if ( isempty( IsegTopT ) == 1 )   % no altimetry point behind us
                IsegTop( ib( iii ) ) = 1;
            else
                IsegTop( ib( iii ) ) = min( IsegTopT( end ), NatiPts - 1);
            end
        end
    end
    
    % *** New bathymetry segment? ***
    ii = uint16( find( xv( ib, 1, I1 ) < xBot( 1, IsegBot( ib )     )' | ...
        xv( ib, 1, I1 ) > xBot( 1, IsegBot( ib ) + 1 )' ) );
    if ( isempty( ii ) == 0 )
        for iii = ii'
            IsegBotT = find( xBot( 1, : ) < xv( ib( iii ), 1, I1 ) );
            if ( isempty( IsegBotT ) == 1 )   % no bathymetry point behind us
                IsegBot( ib( iii ) ) = 1;
            else
                IsegBot( ib( iii ) ) = min( IsegBotT( end ), NbtyPts - 1 );
            end
        end
    end
    
    % *** Reflections? ***
    % Tests that ray at step i is inside, and ray at step i+1 is outside
    % to detect only a crossing from inside to outside
    
    DBegTop( ib, 1 ) = xv( ib, 1, I  ) - xTop( 1, IsegTop( ib ) )';  % vector pointing from top    to ray
    DBegTop( ib, 2 ) = xv( ib, 2, I  ) - xTop( 2, IsegTop( ib ) )';
    DistBegTop( ib ) = dot( DBegTop( ib, : )', nTop( :, IsegTop( ib ) ) );
    
    DEndTop( ib, 1 ) = xv( ib, 1, I1 ) - xTop( 1, IsegTop( ib ) )';  % vector pointing from top    to ray
    DEndTop( ib, 2 ) = xv( ib, 2, I1 ) - xTop( 2, IsegTop( ib ) )';
    DistEndTop( ib ) = dot( DEndTop( ib, : )', nTop( :, IsegTop( ib ) ) );
    
    DBegBot( ib, 1 ) = xv( ib, 1, I  ) - xBot( 1, IsegBot( ib ) )';  % vector pointing from bottom to ray
    DBegBot( ib, 2 ) = xv( ib, 2, I  ) - xBot( 2, IsegBot( ib ) )';  % vector pointing from bottom to ray
    DistBegBot( ib ) = dot( DBegBot( ib, : )' , nBot( :, IsegBot( ib ) ) );
    
    DEndBot( ib, 1 ) = xv( ib, 1, I1 ) - xBot( 1, IsegBot( ib ) )';  % vector pointing from bottom to ray
    DEndBot( ib, 2 ) = xv( ib, 2, I1 ) - xBot( 2, IsegBot( ib ) )';  % vector pointing from bottom to ray
    DistEndBot( ib ) = dot( DEndBot( ib, : )', nBot( :, IsegBot( ib ) ) );
    
    CrossTop = find( DistBegTop( ib ) < 0.0 & DistEndTop( ib ) >= 0.0 );
    CrossBot = find( DistBegBot( ib ) < 0.0 & DistEndBot( ib ) >= 0.0 );
    
    if     ( isempty( CrossTop ) == 0 )
        BC(1:1) = TopOpt(2:2);
        reflect( I1, ib( CrossTop ), IsegTop( ib ), BeamType, BC, cpT, rhoT, 'TOP', ...
            tTop( :, IsegTop( CrossTop )  )', nTop( :, IsegTop( CrossTop ) )', ...
            thetaTop, RTop, phiTop, NTopPts, zSSPV, cSSPV, czV )
    end
    
    if ( isempty( CrossBot ) == 0 )
        BC = BotOpt(1:1);
        reflect( I1, ib( CrossBot ), IsegBot( ib ), BeamType, BC, cpB, rhoB, 'BOT', ...
            tBot( :, IsegBot( CrossBot )  )', nBot( :, IsegBot( CrossBot ) )', ...
            thetaBot, RBot, phiBot, NBotPts, zSSPV, cSSPV, czV )
    end
    
    % *** Has the ray left the box, lost its energy, or exceeded storage limit? ***
    
    ikill = abs( xv( ib, 1, I1 ) ) > rBox | ...
                 xv( ib, 2, I1 )   > zBox | ...
           abs( Rfa( ib,    I1 ) ) < 0.02;
       
    if any( ikill ) > 0
        keep = find( ikill == 0 );
        kill = find( ikill == 1 );
        Nsteps( ib( kill ) ) = I1;
        ib = ib( keep );
        if length( ib ) == 0  % no more beams to trace
            return
        end
    end
    
    if ( any( I >= MxN - 2 ) )
        disp( 'Warning: Insufficient storage for ray trajectory' )
        Nsteps( ib ) = I;
        return
    end
    
end   % Next step