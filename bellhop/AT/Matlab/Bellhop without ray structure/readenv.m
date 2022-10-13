function  [ pltitle, freq, ...
      TopOpt, depthT, cpT, csT, rhoT, ...
      BotOpt, depthB, cpB, csB, rhoB, fid ] = readenv( envfil )
   
   % read in the environmental file
   
   global zSSPV cSSPV cSSPVs rhoV czV NMedia N depth HV NSSPPts sigma
   global alphaR betaR rhoR alphaI betaI
   
   alphaR = 1500;   % defaults
   betaR  = 0;
   rhoR   = 1;
   alphaI = 0;
   betaI  = 0;
   
   cpT    = 0.0;
   csT    = 0.0;
   rhoT   = 0.0;
   cpB    = 2000.0;
   csB    = 0.0;
   rhoB   = 2.0;
   
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
   
   TopOpt   = fgetl( fid );
   TopOpt   = TopOpt( 2: end );  % remove quote
   
   SSPType(   1:1) = TopOpt(1:1);
   BCType(    1:1) = TopOpt(2:2);
   AttenUnit( 1:1) = TopOpt(3:3);
   
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
   
   if ( length( TopOpt ) >= 4 )
      switch ( TopOpt(4:4) )
         case ( 'T' )
            disp( '    THORP attenuation added' )
      end
   end
   
   if ( length( TopOpt ) >= 5 )
      switch ( TopOpt(5:5) )
         case ( '*' )
            disp( '    Development options enabled' )
      end
   end
   
   [ cpT, csT, rhoT, BumDen, eta, xi ] = topbot( fid, freq, BCType, AttenUnit );
   
   % main loop to readin in SSP
   fprintf( '\n     Z          AlphaR     BetaR      Rho       AlphaI     BetaI \n' );
   zSSPV  = [];
   cSSPV  = [];
   cSSPVs = [];
   for medium = 1 : NMedia
      if ( medium == 1 )
         Loc( medium ) = 0;
      else
         Loc( medium ) = Loc( medium - 1 ) + NSSPPts( medium - 1 );
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
         
         cp = CRCI( alphaR, alphaI, freq, AttenUnit );
         cs = CRCI( betaR,  betaI,  freq, AttenUnit );
         zSSPV  = [ zSSPV; ztmp ];   % add in to existing vector
         cSSPV  = [ cSSPV; cp   ];
         cSSPVs = [ cSSPVs; cs  ];
         rhoV   = [ rhoV;  rhoR ];
         if ( ztmp == depth( medium+1 ) ) break; end
      end 
      
      % stuff for Bellhop
      if ( medium == 1 )
         HV    = diff( zSSPV ); % layer thicknesses
         czV   = diff( cSSPV ) ./ HV; % gradient of ssp (centered-difference approximation)
      end
      NSSPPts( medium ) = ii;
      if ( medium == 1 ); depth( 1 ) = zSSPV( 1 ); end
   end  
   
   % lower halfspace
   BotOpt = fgetl( fid );
   BotOpt = BotOpt( 2:end );   % remove quote
   BCType = BotOpt( 1: 1 );
   [ cpB, csB, rhoB, BumDen, eta, xi ] = topbot( fid, freq, BCType, AttenUnit );
   
   depthT = depth( 1 );
   depthB = depth( NMedia + 1 );
