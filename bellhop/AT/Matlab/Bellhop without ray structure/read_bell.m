function  [ Isingl, Nimage, Ibwin, deltas, MxN, zBox, rBox, epmult, rLoop, RunType, BeamType ] = read_bell( fid, freq, depthB, depthT )

% read in the rest of the environmental file
% This is the part containing control info specific to Bellhop

global alpha Nbeams

Isingl = 0;
c0     = 1500.0;    % reference sound speed used to compute default step size
Ibwin  = 5;
rLoop  = 0;
Nimage = 3;

% *** Read run type ***

RunType = fgetl( fid );
RunType = RunType( 2:end ); % remove leading quote
if ( RunType( 2:2 ) == '''' ); RunType( 2:3 ) = '  '; end
disp( '    ' )

switch ( RunType(1:1) )
    case ( 'R' )
        disp( 'Ray trace run' )
    case ( 'E' )
        disp( 'Eigenray trace run' )
    case ( 'I' )
        disp( 'Incoherent TL calculation' )
    case ( 'S' )
        disp( 'Semi-coherent TL calculation' )
    case ( 'C' )
        disp( 'Coherent TL calculation' )
    case ( 'A' )
        disp( 'Arrivals calculation' )
    case ( 'a' )
        disp( 'Arrivals calculation' )
        RunType(1:1) = 'A';   % 'a' for binary mode is irrelevant in the Matlab version
    otherwise
        fclose all;
        error( 'Fatal Error: Unknown RunType selected' )
end

switch ( RunType(2:2) )
    case ( 'C' )
        disp( 'Cartesian beams' )
    case ( 'R' )
        disp( 'Ray centered beams' )
    case ( 'S' )
        disp( 'Simple gaussian beams' )
    case ( 'B' )
        disp( 'gaussian beam Bundles (grab)' )
    otherwise
        RunType(2:2) = 'G';
        disp( 'Geometric beams' )
end

Nbeams = fscanf( fid, '%i', 1 );
fprintf( '\nNumber of beams = %i \n', Nbeams )
fgetl( fid );
alpha = fscanf( fid, '%f', Nbeams );
if Nbeams > 2
    alpha = linspace( alpha( 1 ), alpha( 2 ), Nbeams )';
end
fgetl( fid );
% following is determined by Matlab storage limits
MxN    = floor( 5000000 / Nbeams );      % maximum number of allowed steps for a single ray

% *** Limits for tracing beams ***

deltas = fscanf( fid, '%f', 1 );
zBox   = fscanf( fid, '%f', 1 );
rBox   = fscanf( fid, '%f', 1 );

fprintf( '\nStep length,     deltas = %d \n', deltas )
fprintf( 'Maximum ray Depth, zBox = %d \n', zBox )
fprintf( 'Maximum ray range, rBox = %d \n', rBox )

% *** Automatic step size selection < 10 wavelengths

if ( deltas == 0.0 )
    
    lambdax10 = 10 * c0 / freq;   % 10 wavelengths
    lambdax66 = 66 * c0 / freq;   % 66 wavelengths
    Dby20  = ( depthB - depthT ) / 20.0;
    
    switch ( RunType(1:1) )
        case ( 'R' )
            deltas = min( lambdax66, Dby20 );
        case ( 'E' )
            deltas = min( lambdax66, Dby20 );
        case ( 'I' )
            deltas = min( lambdax66, Dby20 );
        case ( 'S' )
            deltas = min( lambdax66, Dby20 );
        case ( 'C' )
            deltas = min( lambdax10, Dby20 );
        case ( 'A' )
            deltas = min( lambdax10, Dby20 );
        case ( 'a' )
            deltas = min( lambdax10, Dby20 );
    end
    deltas = max( deltas, rBox / MxN );		% at most do MxN steps
    deltas = min( deltas, depthB - depthT );	% deltas < channel height
    fprintf( 'Default step length,     deltas = %d \n', deltas )
end

rBox = 1000.0 * rBox;

% ****** Beam characteristics ******

% note: curvature change can cause overflow in grazing case
% suppress by setting BeamType(2:2) = 'Z'

if ( findstr( RunType(2:2), 'GBS' ) ~= 0 )
    BeamType(1:2) = 'MS';
    rLoop = 1.0;
    epmult = 1.0;
    BeamType(3:3) = RunType(3:3);
    switch ( BeamType(3:3) );
        case ( 'S' )
            disp( 'Beam shift in effect' )
        otherwise
            disp( 'No beam shift in effect' )
    end
else
    BeamType(1:2)= fscanf( fid, '%s2', 1);
    epmult       = fscanf( fid, '%f', 1 );
    rLoop        = fscanf( fid, '%f', 1 );
    fprintf( '\n\nType of beam = %s', BeamType(1:1) )
    
    switch ( BeamType(2:2) )
        case ( 'D' )
            disp( 'Curvature doubling invoked' )
        case ( 'Z' )
            disp( 'Curvature zeroing invoked' )
        case ( 'S' )
            disp( 'Standard curvature condition' )
        otherwise
            fclose all;
            error( 'Fatal Error: Unknown curvature condition' )
    end
    
    fprintf( 'Epsilon multiplier %d \n', epmult )
    fprintf( 'Range for choosing beam width %d \n', rLoop )
    
    %  ****** Images, windows ******
    
    Nimage = fscanf( fid, '%i', 1 );
    Ibwin  = fscanf( fid, '%i', 1 );
    disp( '  ' )
    fprintf( 'Number of images, Nimage = %i \n', Nimage )
    fprintf( 'Beam windowing parameter,  Ibwin = %i \n', Ibwin )
    
end

fclose( fid );