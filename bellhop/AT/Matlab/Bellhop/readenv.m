function  [ pltitle, freq, Bdry, fid ] = readenv( envfil )
% read in the environmental file
   
   global SSP NMedia N depth HV sigma cInsT rhoInsT cInsB rhoInsB NFirstAcoustic NLastAcoustic
   global alphaR betaR rhoR alphaI betaI
   
   alphaR = 1500;   % defaults
   betaR  = 0;
   rhoR   = 1;
   alphaI = 0;
   betaI  = 0;
   
   NFirstAcoustic = 0;
   
   Bdry.Top.cp    = 0.0;
   Bdry.Top.cs    = 0.0;
   Bdry.Top.rho   = 0.0;
   Bdry.Bot.cp    = 2000.0;
   Bdry.Bot.cs    = 0.0;
   Bdry.Bot.rho   = 2.0;
   
   fid = fopen( envfil, 'r' );
   if ( fid == -1 )
      error( 'Unable to open environmental file %s', envfil );
   end
   
   pltitle = fgetl( fid );
   disp( pltitle )
   
   freq     = fscanf( fid, '%f', 1 );
   fprintf( 'Frequency = %d \n', freq )
   fgetl( fid );
   
   NMedia   = fscanf( fid, '%i', 1 );
   fprintf( 'Number of media = %i \n\n', NMedia )
   fgetl( fid );
   
   Bdry.Top.Opt   = fgetl( fid );
   Bdry.Top.Opt   = Bdry.Top.Opt( 2: end );  % remove quote
   
   SSPType(   1:1) = Bdry.Top.Opt(1:1);
   BCType(    1:1) = Bdry.Top.Opt(2:2);
   AttenUnit( 1:1) = Bdry.Top.Opt(3:3);
   
   %     *** SSP approximation options ***
   
   switch ( SSPType )
      case ( 'N' )
         disp( '    N2-LINEAR approximation to SSP' )
      case ( 'C' )
         disp( '    C-LINEAR approximation to SSP' )
      case ( 'S' )
         disp( '    SPLINE approximation to SSP' )
      case ( 'A' )
         disp( '    ANALYTIC SSP option' )
      otherwise
         fclose all;
         error( 'Fatal error: Unknown option for SSP approximation' )
   end
   
   %     *** Attenuation options ***
   
   switch ( AttenUnit )
      case ( 'N' )
         disp( '    Attenuation units: nepers/m' )
      case ( 'F' )
         disp( '    Attenuation units: dB/mkHz' )
      case ( 'M' ) 
         disp( '    Attenuation units: dB/m' )
      case ( 'W' )
         disp( '    Attenuation units: dB/wavelength' )
      case ( 'Q' )
         disp( '    Attenuation units: Q' )
      otherwise
         fclose all;
         error( 'Fatal error: Unknown attenuation units' )
   end
   
   %     *** optional addition of volume attenuation using standard formulas
   
   if ( length( Bdry.Top.Opt ) >= 4 )
      switch ( Bdry.Top.Opt(4:4) )
         case ( 'T' )
            disp( '    THORP attenuation added' )
      end
   end
   
   if ( length( Bdry.Top.Opt ) >= 5 )
      switch ( Bdry.Top.Opt(5:5) )
         case ( '*' )
            disp( '    Development options enabled' )
      end
   end
   
   [ Bdry.Top.cp, Bdry.Top.cs, Bdry.Top.rho, BumDen, eta, xi ] = topbot( fid, freq, BCType, AttenUnit );
   
   % main loop to readin in SSP
   fprintf( '\n     Z          AlphaR     BetaR      Rho       AlphaI     BetaI \n' );
   SSP.z   = [];
   SSP.c   = [];
   SSP.cs  = [];
   SSP.rho = [];
   for medium = 1 : NMedia
      if ( medium == 1 )
         Loc( medium ) = 0;
      else
         Loc( medium ) = Loc( medium - 1 ) + SSP.Npts( medium - 1 );
      end
      ILoc = Loc( medium );
      
      % number of points in finite-difference grid
      N(     medium )  = fscanf( fid, '%i', 1 );
      sigma( medium )  = fscanf( fid, '%f', 1 );
      depth( medium+1) = fscanf( fid, '%f', 1 );
      
      if ( N( medium ) == 0 ) % calculate mesh automatically
         % choose a reference sound speed
         C = alphaR;
         if ( betaR > 0.0 ); C = betaR; end % shear?
         
         deltaz = 0.1 * C / freq;     % tenth of a wavelength default sampling
         N( medium ) = round( ( depth( medium + 1 ) - depth( medium ) ) / deltaz );
         N( medium ) = max( N( medium ), 10 );     % require a minimum of 10 points
      end
      fprintf( '    ( Number of pts = %i  Roughness = %6.2f  Depth = %8.2f ) \n', N( medium ), sigma( medium ), depth( medium+1 ) );
      fgetl( fid );
      
      % read in the SSP
      
      clear cofz;
      for ii = 1:999
         ztmp   = fscanf( fid, '%f', 1 );
         alphaRtemp = fscanf( fid, '%f', 1 );
         betaRtemp  = fscanf( fid, '%f', 1 );
         rhoRtemp   = fscanf( fid, '%f', 1 );
         alphaItemp = fscanf( fid, '%f', 1 );
         betaItemp  = fscanf( fid, '%f', 1 );
         fgetl( fid );
         % if values read in copy over, otherwise use defaults
         if (~isempty( alphaRtemp ) ); alphaR = alphaRtemp; end
         if (~isempty( betaRtemp  ) ); betaR  = betaRtemp;  end
         if (~isempty( rhoRtemp   ) ); rhoR   = rhoRtemp;   end
         if (~isempty( alphaItemp ) ); alphaI = alphaItemp; end
         if (~isempty( betaItemp  ) ); betaI  = betaItemp;  end
         
         fprintf( '%8.2f    %8.2f    %8.2f    %8.2f    %8.4f    %8.4f \n', ztmp, alphaR, betaR, rhoR, alphaI, betaI )
         
         cp = crci( alphaR, alphaI, freq, AttenUnit );
         cs = crci( betaR,  betaI,  freq, AttenUnit );
         SSP.z   = [ SSP.z;   ztmp ];   % add in to existing vector
         SSP.c   = [ SSP.c;   cp   ];
         SSP.cs  = [ SSP.cs;  cs  ];
         SSP.rho = [ SSP.rho; rhoR ];
         if ( ztmp == depth( medium+1 ) ); break; end
      end
      
      % keep track of first and last acoustic medium
      if ( ~any( cs ) )   % shear anwwhere? 
         if ( NFirstAcoustic == 0 )
            NFirstAcoustic = medium;
         end
         NLastAcoustic  = medium;
      end
      
      % stuff for Bellhop
      if ( medium == 1 )
         HV       = diff( SSP.z ); % layer thicknesses
         SSP.cz   = diff( SSP.c ) ./ HV; % gradient of ssp (centered-difference approximation)
      end
      SSP.Npts( medium ) = ii;
      if ( medium == 1 ); depth( 1 ) = SSP.z( 1 ); end
   end  
   
   % lower halfspace
   Bdry.Bot.Opt = fgetl( fid );
   Bdry.Bot.Opt = Bdry.Bot.Opt( 2:end );   % remove quote
   BCType       = Bdry.Bot.Opt( 1: 1 );
   [ Bdry.Bot.cp, Bdry.Bot.cs, Bdry.Bot.rho, BumDen, eta, xi ] = topbot( fid, freq, BCType, AttenUnit );
   
   Bdry.Top.depth = depth( 1 );
   Bdry.Bot.depth = depth( NMedia + 1 );

   % Get rho, c just INSide the boundary (used later for reflection
   % coefficients)
   I = NFirstAcoustic;
   rhoInsT = SSP.rho( I );
   cInsT   = SSP.c(   I );

   I = Loc( NLastAcoustic ) + SSP.Npts( NLastAcoustic );
   rhoInsB = SSP.rho( I );
   cInsB   = SSP.c(   I );
end
   