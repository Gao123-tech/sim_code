% menumaster.m
% Run models in the acoustics toolbox from a gui menu
% michael b. porter October 23, 1999

clear all

envfil = '';
atifil = '';
btyfil = '';
sbpfil = '';

%caxisdat = [ -100 0 ];	% default TL limits for colorbar

choice = 0;

while choice ~= 22
  
  choice = menu( 'Options', ...
    'Select envfil', ...
    'Edit   envfil', ...
    'Edit   atifil', ...
    'Edit   btyfil', ...
    'Edit   field.flp', ...
    'Edit   fields.flp', ...
    'BELLHOP', ...
    'BOUNCE', ...
    'KRAKEN ', ...
    'KRAKENC', ...
    'SCOOTER', ...
    'Plotati ', ...
    'Plotbty ', ...
    'Plotmode', ...
    'Plotray ', ...
    'Plotrth', ...
    'Plotshd ', ...
    'Plotslic', ...
    'Axis limits', ...
    'Colorbar', ...
    'Receiver depth (for Plotslice)', ...
    'Quit' );
  
  switch choice
  case 1				% Select envfil
      d = dir( '*.env' );
      if isempty( d )
          warndlg( 'No envfil in this directory', 'Warning' );
      else
          filenames = {d.name};
          [ s, v ] = listdlg( 'PromptString', 'Select a file:',...
              'SelectionMode', 'single',...
              'ListString', filenames );
          envfil = char( filenames( s ) );
          root = envfil( 1:length( envfil ) - 4 );
          atifil = [ root '.ati' ];
          btyfil = [ root '.bty' ];
          trcfil = [ root '.trc' ];
          brcfil = [ root '.brc' ];
          sbpfil = [ root '.sbp' ];
      end
      
  case 2				% Edit envfil
    if ( isempty( envfil ) )
      warndlg( 'No envfil has been selected', 'Warning' );
    else
      eval( [ '!notepad ' envfil ] );
    end
    
  case 3				% Edit btyfil
    if ( isempty( atifil ) )
      warndlg( 'No atifil has been selected', 'Warning' );
    else
      eval( [ '!notepad ' atifil ] );
    end
    
  case 4				% Edit btyfil
    if ( isempty( btyfil ) )
      warndlg( 'No btyfil has been selected', 'Warning' );
    else
      eval( [ '!notepad ' btyfil ] );
    end
    
  case 5				% Edit field.flp
      eval( '!notepad field.flp' );
    
  case 6				% Edit fields.flp
      eval( '!notepad fields.flp' );
      
  case 7				% BELLHOP
    if ( isempty( envfil ) )
      warndlg( 'No envfil has been selected', 'Warning' );
    else
      if ( exist( atifil ) == 2 )
        copyfile( atifil, 'atifil' );	% need altimetry  in atifil
      end
      if ( exist( btyfil ) == 2 )
        copyfile( btyfil, 'btyfil' );	% need bathymetry in btyfil
      end
      if ( exist( trcfil ) == 2 )
        copyfile( trcfil, 'trcfil' );	% need refl. coef in trcfil
      end
      if ( exist( brcfil ) == 2 )
        copyfile( brcfil, 'brcfil' );	% need refl. coef in brcfil
      end
      if ( exist( sbpfil ) == 2 )
        copyfile( sbpfil, 'sbpfil' );	% need refl. coef in brcfil
      end
      eval( [ '! bellhop.exe < ' envfil ] );
    end
    
case 8				% BOUNCE
    if ( isempty( envfil ) )
      warndlg( 'No envfil has been selected', 'Warning' );
    else
      eval( [ '! bounce.exe < ' envfil ] );
    end
  
  case 9				% KRAKEN
    if ( isempty( envfil ) )
      warndlg( 'No envfil has been selected', 'Warning' );
    else
      if ( exist( btyfil ) == 2 )
        copyfile( btyfil, 'btyfil' );	% need bathymetry in btyfil
      end
      if ( exist( trcfil ) == 2 )
        copyfile( trcfil, 'trcfil' );	% need refl. coef in trcfil
      end
      if ( exist( brcfil ) == 2 )
        copyfile( brcfil, 'brcfil' );	% need refl. coef in brcfil
      end
      eval( [ '! kraken.exe < ' envfil ] );
      ! field.exe < field.flp
    end
    
  case 10				% KRAKENC
    if ( isempty( envfil ) )
      warndlg( 'No envfil has been selected', 'Warning' );
    else
      if ( exist( btyfil ) == 2 )
        copyfile( btyfil, 'btyfil' );	% need bathymetry in btyfil
      end
      if ( exist( trcfil ) == 2 )
        copyfile( trcfil, 'trcfil' );	% need refl. coef in trcfil
      end
      if ( exist( brcfil ) == 2 )
        copyfile( brcfil, 'brcfil' );	% need refl. coef in brcfil
    end
      eval( [ '! krakenc.exe < ' envfil ] );
      ! field.exe < field.flp
    end
    
  case 11				% SCOOTER
    if ( isempty( envfil ) )
      warndlg( 'No envfil has been selected', 'Warning' );
    else
      if ( exist( btyfil ) == 2 )
        copyfile( btyfil, 'btyfil' );	% need bathymetry in btyfil
      end
      if ( exist( trcfil ) == 2 )
        copyfile( trcfil, 'trcfil' );	% need refl. coef in trcfil
      end
      if ( exist( brcfil ) == 2 )
        copyfile( brcfil, 'brcfil' );	% need refl. coef in brcfil
      end
      eval( [ '! scooter.exe < ' envfil ] );
      ! fields.exe < fields.flp
    end
    
 case 12				% Plotati
    if ( isempty( atifil ) )
      warndlg( 'No atifil has been selected', 'Warning' );
    else
      plotati( atifil )
    end
    
 case 13				% Plotbty
    if ( isempty( btyfil ) )
      warndlg( 'No btyfil has been selected', 'Warning' );
    else
      plotbty( btyfil )
    end
    
  case 14				% Plotmode
    if ( exist( 'MODFIL0001' ) == 2 )
        plotMODE( 'MODFIL0001' )
    else
        warndlg( 'No mode file exists; you must run KRAKEN first', 'Warning' );
    end
    
  case 15				% Plotray
    if ( exist( 'RAYFIL' ) == 2 )
        plotray( 'RAYFIL' )
    else
        warndlg( 'No ray file exists; you must run BELLHOP first (with ray ouput selected)', 'Warning' );
    end
    
  case 16				% Plotrth
    if ( exist( 'BRCFIL' ) == 2 )
        plotrth( 'BRCFIL' )
    else
        warndlg( 'No bottom refl. coef. file exists; you must run BOUNCE first', 'Warning' );
    end
    
  case 17				% Plotshd
    if ( exist( 'SHDFIL' ) == 2 )
        plotshd( 'SHDFIL' )
        if exist( 'caxisdat' )
            caxis( caxisdat ); colorbar( 'horiz' )
        end
    else
        warndlg( 'No shdfil exists; you must run a model first', 'Warning' );
    end
    
  case 18				% Plotslic
    if ( exist( 'SHDFIL' ) ~= 2 )
        warndlg( 'No shdfil exists; you must run a model first', 'Warning' );
    else
        if ( ~exist( 'rdt' ) )
            warndlg( 'Receiver depth for slice has not been selected', 'Warning' );
        else
           plotslic( 'SHDFIL', rdt )
       end
    end
    
  case 19			% axis limits
    axlimdlg
  case 20			% Colorbar
    prompt = { 'Color axis' };
    if exist( 'caxisdat' )
       def = { num2str( round( caxisdat ) ) };
    else
       def = { num2str( [ 50 100 ] ) };
    end

    prtitle = 'Colorbar';
    lineNo = 1;
    answer = inputdlg( prompt, prtitle, lineNo, def );
    caxisdat = sscanf( char( answer( 1 ) ), '%f' );
    caxisdat = caxisdat';
    if isempty( caxisdat )
      caxisdat = [ -100 0 ];
    end
    caxis( caxisdat ); colorbar( 'horiz' )
    
  case 21			% Receiver depth
    prompt = { 'Receiver depth' };
    if exist( 'rdt' )
       def = { num2str( rdt ) };
    else
       rdt = 0;
       def = { num2str( rdt ) };
    end

    prtitle = 'Receiver depth';
    lineNo = 1;
    answer = inputdlg( prompt, prtitle, lineNo, def );
    rdt = sscanf( char( answer( 1 ) ), '%f' );
    if isempty( rdt )
      rdt = 0;
    end

  case 22			% Quit
    break
  end
end;	% next menu selection

%case 13			% Receiver positions
%    prompt={'Nsd', 'Source depths (m)', 'Nrd', 'Receiver depths (m)', 'Nrr', 'Receiver ranges (km)' };
%    def = { int2str( Nsd ), num2str( sd ), int2str( Nrd ), num2str( rd ), int2str( Nrr ), num2str( rr ) };
%    prtitle = 'Source/Receiver information';
%    lineNo = 1;
%    answer = inputdlg( prompt, prtitle, lineNo, def );
    
%    Nsd = sscanf( char( answer( 1 ) ), '%i' )';
%    sd  = sscanf( char( answer( 2 ) ), '%f' );
%    if ( Nsd > length( sd ) ) sd = linspace( sd( 1 ), sd( 2 ), Nsd ); end
    
%    Nrd = sscanf( char( answer( 3 ) ), '%i' );
%    rd  = sscanf( char( answer( 4 ) ), '%f' )';
%    if ( Nrd > length( rd ) ) rd = linspace( rd( 1 ), rd( 2 ), Nrd ); end
    
%    Nrr = sscanf( char( answer( 5 ) ), '%i' );
%    rr  = sscanf( char( answer( 6 ) ), '%f' )';
%    if ( Nrr > length( rr ) ) rr = linspace( rr( 1 ), rr( 2 ), Nrr ); end
%    writeenv;


%case 14			% Beam angles
%    prompt={'Nbeams', 'alpha1', 'alpha2' };
%    def = { int2str( Nbeams ), num2str( alpha( 1 ) ), num2str( alpha( 2 ) ) };
%    prtitle = 'Beam angles';
%    lineNo = 1;
%    answer = inputdlg( prompt, prtitle, lineNo, def );
    
%    Nbeams     = sscanf( char( answer( 1 ) ), '%i' );
%    alpha( 1 ) = sscanf( char( answer( 2 ) ), '%f' );
%    alpha( 2 ) = sscanf( char( answer( 3 ) ), '%f' ); 
%    writeenv  
