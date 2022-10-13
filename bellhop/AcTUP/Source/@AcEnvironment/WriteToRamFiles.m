function WriteToRamFiles(Env, Freq, RMax, DeltaR,  Zs, RamIpFile, Title, RamParams)
% WriteToRamFiles  Writes run parameters to standard input file formats for RAM*
%
% USAGE: WriteToRamFiles(Env, Freq, RMax, DeltaR,  Zs, EnvFile, FlpFile, Title, RamParams)
% 
% Note Variations in datum for z in each of the {z, x} lists where x is an acoustic parameter 
%
%    RAM, RAMS        -> all z referenced from air/water surface
%                        (flat)
%    RAMGeo, RAMSurf  -> bottom z referenced from water/bottom surface 
%                        (bathymetry following)
%
% Revision 0.0    03 06 2003 - ALM
%                 - RAMGeo ONLY
%
% Revision 1.0    04 07 2003 - ALM
%                 - expanded to handle RAM, RAMS and RAMSurf
%
% Revision 1.1    12 11 2003 - ALM
%                 - Fixed bug in 1.0 ... -1 -1 delimiter appearing after surface profile even if no surface profile exists

% GENERAL RUN PARAMETERS - GET
% ============================
% Surface Profile Data 
% --------------------
% do we have a file or do we have a flat pressure release surface
switch RamParams.Species
   case 'RAMSurf'
      if RamParams.UseSurfFile
         SurfData = ReadBathymetryFile(RamParams.SurfFName); % same format as bathymetry file!!
      else
         SurfData = [0, RMax; 0, 0]; % 2 x N
      end  
end
% Bathymetry Data 
% ---------------
% do we have a file or do we have a flat bottom specified by the
% environment object ? ... the bathymetry data in the file (if it exists)
% should have depths within the range specified by the water layer of the
% environment object
if RamParams.UseBathFile
   BathData = ReadBathymetryFile(RamParams.BathFName); 
   % we dont need to check whether the bathymetry data is consistent with
   % the environ spec (as for other prop models) since RAM maintains last
   % specified profile value to zmax for all layers ... (the bathymetry and
   % sediment/sediment surfaces merely specify at which depth to switch from 
   % one profile table to the next  
else
   EnvWaterDepth = GetWaterDepth(Env);
   BathData = [0, RMax; EnvWaterDepth, EnvWaterDepth]; % 2 x N
end  
% Maximum Depths and Thicknesses for Various Boundaries
% Maximum Water Depth ...
MaxWaterDepth = max(BathData(2,:));
% Maximum Depth for Plot -> zmplt
   % zmplt is only depth of the computational grid over which results are
   % plotted ... in this case we default to the entire environment above the
   % attenuation layer
[zsub, halfspace_flag] = GetSubstrateThickness(Env);
if halfspace_flag
   % then zsub only deals with layers between the water and the top of the halfspace
   zsub = zsub + RamParams.LastLayerDz;
end
MaxSubstrateDepth = MaxWaterDepth + zsub; 
if RamParams.zmplt < 0  % automatically define plot depth range
   % define limit as max depth + substrate thickness (not inclduing
   % attenuation layer)
   RamParams.zmplt = MaxSubstrateDepth; 
end
% Maximum Depth for PE calc ->  zmax 
   % zmax is the z-limit for the PE calc from top of the water column to the
   % bottom of the last substrate layer (including the attentuation layer if,
   % as recommended, this is included)
zmax  = MaxSubstrateDepth + RamParams.AttenLayerDz;
%
% GENERAL RUN PARAMETERS - WRITE
% ==============================
fprintf(RamIpFile, '%s\n', Title);
fprintf(RamIpFile, '%f\t%f\t%f\t\t%s\n'  , Freq, Zs, RamParams.zr,                                 'f, zs, zr');
fprintf(RamIpFile, '%f\t%f\t%d\t\t%s\n'  , RMax, DeltaR, RamParams.ndr,                            'rmax, dr, ndr');
fprintf(RamIpFile, '%f\t%f\t%d\t%f\t%s\n', zmax, RamParams.dz, RamParams.ndz, RamParams.zmplt,     'zmax dz, ndz, zmplot');
switch RamParams.Species
   case {'RAM', 'RAMGeo', 'RAMSurf'}
      fprintf(RamIpFile, '%f\t%d\t%d\t%f\t%s\n', RamParams.c0, RamParams.np, RamParams.ns, RamParams.rs, 'c0, np, ns, rsc');
   case 'RAMS'
      fprintf(RamIpFile, '%f\t%d\t%d\t%f\t%s\n', RamParams.c0, RamParams.np, RamParams.irot, RamParams.theta, 'c0, np, irot, theta');
end
% surface
switch RamParams.Species
   case 'RAMSurf'
      fprintf(RamIpFile, '%f\t%f\t\t\t%s\n', SurfData(1,1), SurfData(2,1), 'free surface data {r,z} in [m, m]');
      fprintf(RamIpFile, '-1\t-1\n');
end

% bathymetry
fprintf(RamIpFile, '%f\t%f\t\t\t%s\n', BathData(1,1), BathData(2,1), 'bathymetry data {r,z} in [m, m]');
for col = 2:size(BathData, 2) % should be N x 2
   fprintf(RamIpFile, '%f\t%f\n', BathData(1, col), BathData(2, col));
end
fprintf(RamIpFile, '-1\t-1\n');

% PROFILE DATA IN RANGE SLICES
% ============================
% RAM* can handle range dependent profiles ... in future we may do this
% through AcT_UI by using arrays of environments and loop through the data
% ... at this point however we just have a single range slice

for islice = 1:1

   % GET
   % ===
   % Acoustic Parameter Profiles for Water and Substrate 
   % (handled seperately by RAM IP file)
   % build substrate profiles for cb, rhob and atten (since these may consist
   % of multiple layers unlike the water column which is a single layer in EnvObj)
   % - note: 1. that in the property vectors for each AcLayer the last depth is the thickness of the layer ... 
   %         2. conversely in RAM, properties are extrapolated (zero-gradient ... i.e hold last value) to 
   %            upper and lower limits of depth range by the RAM routines
   %         3. 1st layer is always water so first substrate layer is @ index 2
   %         4. halfspaces are homogenous
   %
   nlayers = GetNumLayers(Env);
   zbuffer = 0;
   z     = [];
   cbp   = [];
   rhob  = [];
   attnp = [];
   dz   = RamParams.dz;
   % water layer first ...
   layer = GetLayer(Env, 1);
   zw = GetProfile(layer, 'Z');
   cw = GetProfile(layer, 'Cp');
   % now the sediments
   % is there more than one sediment layer?
   if nlayers > 2
      % 1st sediment layer - can't be a half space because there is a layer
      % after this one
      layer   = GetLayer(Env, 2);
      z       = GetProfile(layer, 'Z');
      cbp     = GetProfile(layer, 'Cp') ;
      rhob    = GetProfile(layer, 'Rho');
      attnp   = GetProfile(layer, 'Ap') ;
      % further layers up until and including 2nd last
      for ii = 3:nlayers-1
         layer = GetLayer(Env, ii);
         z       = [z     , z(end) + GetProfile(layer, 'Z')];
         cbp     = [cbp   , GetProfile(layer, 'Cp') ];
         rhob    = [rhob  , GetProfile(layer, 'Rho')];
         attnp   = [attnp , GetProfile(layer, 'Ap') ];
      end
   end
   
   % last sediment layer - 
   % - maybe a halfspace
   % - maybe also be the only sediment layer (i.e. the first)
   layer = GetLayer(Env, nlayers); 
   Z     = GetProfile(layer, 'Z');
   Cp    = GetProfile(layer, 'Cp');
   Rho   = GetProfile(layer, 'Rho');
   Ap    = GetProfile(layer, 'Ap');   
   if IsHalfSpace(layer)
      % reduce homogenous halfspace to finite extent (need only two points)
      % half space may be first (substrate) layer ... 
      if isempty(z)
         refdepth = 0;      % 1st sediment layer
      else
         refdepth = z(end);
      end
      layer_top = refdepth + dz;
      layer_bas = refdepth + RamParams.LastLayerDz; 
      z       = [z     , layer_top, layer_bas ];   
      cbp     = [cbp   , Cp(1)    , Cp(end)   ];
      rhob    = [rhob  , Rho(1)   , Rho(end)  ];
      attnp   = [attnp , Ap(1)    , Ap(end)   ];
   else
      if nlayer == 2  % 1st sediment layer
         z       = GetProfile(layer, 'Z');
         cbp     = GetProfile(layer, 'Cp') ;
         rhob    = GetProfile(layer, 'Rho');
         attnp   = GetProfile(layer, 'Ap') ;
      else            % join onto previous layers
         z       = [z    , z(end) + layer.Z];
         cbp     = [cbp  , Cp ];
         rhob    = [rhob , Rho];
         attnp   = [attnp, Ap ];
      end
   end   
   % now for the attenuation layer (if its there)
   if RamParams.AttenLayerDz > dz 
      % note that as with any data in profile specs in Ram we dont have to
      % specify last point since ram will hold last value to maximum depth for
      % sediment (and in fact for water column as well) ... we just do it here
      % to make the run spec file explicit
      %
      % only need to specify bottom of the layer for RAM interpolation to
      % work (no discontinuties ... either constant or linear gradient) ...
      % actually we dont need to explicitly specify bottom points for those
      % parameters that stay constant ... but just for ease of interpretation
      layer_bas = z(end) + RamParams.AttenLayerDz; 
      z     = [z     , layer_bas ];   
      cbp   = [cbp   , cbp(end)  ];
      % hold these values from previous layer
      rhob  = [rhob  , rhob(end) ];
      attnp = [attnp, RamParams.AttenLayerAttenPMax];
   end
   % convert density from kg/m³ to g/cm³ -> ÷10³
   rhob = rhob.*1e-3;
   
   % WRITE
   % =====
   fprintf(RamIpFile, '%f\t%f\t\t\t%s\n', zw(1), cw(1), 'sound speed profile in water {z,cw} [m, m/s]');
   for col = 2:size(zw, 2)  %should be a row vector
      fprintf(RamIpFile, '%f\t%f\n', zw(col), cw(col));
   end
   fprintf(RamIpFile, '-1\t-1\n');
   %
   fprintf(RamIpFile, '%f\t%f\t\t\t%s\n', z(1), cbp(1), 'compressive sound speed profile in substrate {z,cbp} [m, m/s]');
   for col = 2:size(z, 2)  %should be a row vector
      fprintf(RamIpFile, '%f\t%f\n', z(col), cbp(col));
   end
   fprintf(RamIpFile, '-1\t-1\n');
   %
   fprintf(RamIpFile, '%f\t%f\t\t\t%s\n', z(1), rhob(1), 'density profile in substrate {z,rhob} [m, g/cm³]');
   for col = 2:size(z, 2)  %should be a row vector
      fprintf(RamIpFile, '%f\t%f\n', z(col), rhob(col));
   end
   fprintf(RamIpFile, '-1\t-1\n');
   %
   fprintf(RamIpFile, '%f\t%f\t\t\t%s\n', z(1), attnp(1), 'compressive attenuation profile in substrate {z,attnp} [m, dB/lambda]');
   for col = 2:size(z, 2)  %should be a row vector
      fprintf(RamIpFile, '%f\t%f\n', z(col), attnp(col));
   end
   fprintf(RamIpFile, '-1\t-1\n');
   
end % Next Range Slice

% =========================================================================