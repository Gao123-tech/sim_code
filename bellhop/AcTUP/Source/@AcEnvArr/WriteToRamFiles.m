function WriteToRamFiles(EnvArrObj, RunDef, Freq, RamIpFile)
% WriteToRamFiles  Writes run parameters to standard input file formats for RAM*
%
% USAGE:  WriteToRamFiles(EnvArrObj, RunDef, Freq, RamIpFile)
% 
% Note Variations in datum for z in each of the {z, x} lists where x is an acoustic parameter 
%
%    RAM, RAMS,                -> all z referenced from air/water surface
%                                 (flat)
%    RAMGeo, RAMSurf, RAMSGeo  -> bottom z referenced from water/bottom surface 
%                                 (bathymetry following)
%
% Revision 0.0    03 06 2003 - ALM
%                 - RAMGeo ONLY
%
% Revision 1.0    04 07 2003 - ALM
%                 - expanded to handle RAM, RAMS and RAMSurf
%
% Revision 1.1    12 11 2003 - ALM
%                 - Fixed bug in 1.0 ... -1 -1 delimiter appearing after surface profile even if no surface profile exists
% Revision 1.2    05 08 2004 - AJD
%                 - Moved this function from the AcEnvironment class to the AcEnvArr class and 
%                   added support for multiple, range dependent, environments
% Revision 1.3    14 10 2004 - ALM
%                 - Add ndz*dz depth shift to zmplt parameter 
%                   motivation: RAMGeo appears to not return TL data right up to the last grid point
%                               instead it stops @ zmplot - dz*ndz (decimation factor * dz back from max)
%                   This appears to be because of weird truncation strategy in RAMGeo source code.
%                   Looks like [integer]= f1/f2 -0.5 has been implemented instead of a +0.5 often used to force a round op
%                   instead of a truncation - the incorrect sign means that there is an additional (unwanted ?) truncation
%                   
% Revision 1.4    15 10 2004 - ALM
%                 - Add RAMSGeo 
%
% Revision 2.0    03 12 2004 - ALM
%                 - Uses expanded RunDef struct (not yet class) 
%                 - Simplified paramter set not backward compatible
%
% Revision 2.1    25 02 2005 - ALM
%                 - Mod to use dimensionless definitions for dz and dr with sensible rounding off of absolute values
%
% Revision 2.2    08-10 03 2005 - ALM
%                 - Mod to normalise half-space replacement layer thickness
%                 - additional ndz*dz shift to zmplt because looks like RAMGeo grid is shifted in depth by this value
%                 - add additional rounding off to absolute values of parameters derived from nondimensional numbers 
%                   (response to suspected cause of RAMGeo crashes - unsure ??)
%
% Revision 2.21   08 04 2005 - ALM
%                 - Fixed bug in shear velocity and attenuation read/construction for last layer
%
% Revision 2.3    21 07 2006 - ALM
%                 - Added minimum shear velocity condition for substrate(user defined value)
%                   This avoids manual adjustment to environment ... RAMS* appears to fail if cs is too low (e.g. cs=0 for all z)
%                   Not sure what happens when isolated points or regions have low cs <¿¿¿shrug???> 
%
%                 - Cleaned up code??
%                   Found extracted vectors from "last sediment layer" not being used consistently and conditionally reextracted 
%                   (identically) and also direct ref to layer.z
%                   Was this intentional ??? ~ can't see why - anyway it cleaned up - if fails can retrieve old code from last
%                   archived version (v2.21) (fingers crossed)
%
% GENERAL RUN PARAMETERS - GET
%

%Interpolate the environments
InterpObj = Interpolate(EnvArrObj);
EnvArr    = InterpObj.EnvArr;
RangeVec  = InterpObj.RangeVec;
NEnv      = length(RangeVec);
    
 
% ============================
% Surface Profile Data 
% --------------------
% do we have a file or do we have a flat pressure release surface
switch RunDef.RAM.Species
   case 'RAMSurf'
      if RunDef.RAM.UseSurfFile
         SurfData = ReadBathymetryFile(RunDef.RAM.SurfFName); % same format as bathymetry file!!
      else
         SurfData = [0, RunDef.RMax; 0, 0]; % 2 x N
      end  
end
% Bathymetry Data 
% ---------------
% do we have a file or do we have a flat bottom specified by the
% environment object ? ... the bathymetry data in the file (if it exists)
% should have depths within the range specified by the water layer of the
% environment object
if RunDef.UseBathFile
    BathData = ReadBathymetryFile(RunDef.BathFName); 
    % we dont need to check whether the bathymetry data is consistent with
    % the environ spec (as for other prop models) since RAM maintains last
    % specified profile value to zmax for all layers ... (the bathymetry and
    % sediment/sediment surfaces merely specify at which depth to switch from 
    % one profile table to the next  
else
    if NEnv == 1
        EnvWaterDepth = GetWaterDepth(EnvArr{1});
        BathData = [0, RunDef.RMax; EnvWaterDepth, EnvWaterDepth]; % 2 x N
    else
        BathData = zeros(2, NEnv);
        for iEnv = 1:NEnv
            EnvWaterDepth = GetWaterDepth(EnvArr{iEnv });
            BathData(:, iEnv ) = [RangeVec(iEnv ); EnvWaterDepth];
        end
        if RangeVec(end) < RunDef.RMax
            BathData = [BathData, [RunDef.RMax; EnvWaterDepth]];
        end
    end
end  
% Maximum Depths and Thicknesses for Various Boundaries
% Maximum Water Depth ...
MaxWaterDepth = max(BathData(2,:));

%Derived Parameters
%==================
lambda = RunDef.RAM.c0        / Freq    ;
% if dz < 1m round dz down to either [1/10, 1/5, 1/4 or 1/2] m  ... or mulitples of 10^-n of these numbers
%                                  = [1     2    2.5 or 5  ] x 0.1m  "   " ...
% if dz > 1m round dz down to either [1     2    2.5    5  ] m  ... or mulitples of 10^+n of these numbers
dz     = RunDef.RAM.dz_lambda * lambda          ;
dz     = fix2x10pN(dz, [1 2 2.5 5 ])            ;
% z grid decimation factor ?
ndz    = max(1, floor(RunDef.RAM.dzgridmin/dz)) ;
% similar for dr and assoc. grid decimation
dr     = RunDef.RAM.dr_dz     * dz              ;
dr     = fix2x10pN(dr, [1 2 2.5 5 ])            ;
ndr    = max(1, floor(RunDef.RAM.drgridmin/dr)) ;

% attenuation layer (round up to nearest dz)
AttenLayerDz = ceil(RunDef.RAM.AttenLayerDz_lambda * lambda / dz) .* dz      ;

MaxSubstrateDepth = 0;
for iEnv = 1:NEnv
    [zsub, halfspace_flag] = GetSubstrateThickness(EnvArr{iEnv });
    if halfspace_flag       
        % then zsub only deals with layers between the water and the top of the halfspace
        % Revision 2.2 ... change LastLayer increment to use lambda-normalised value
        % zsub   = zsub + RunDef.RAM.LastLayerDz;
        LastLayer   = GetLayer(EnvArr{iEnv}, 'last');
        cpmax       = GetMaxSoundSpeed(LastLayer);
        LastLayerDz = RunDef.RAM.LastLayerDz_lambda * cpmax/Freq;
        LastLayerDz = ceil(LastLayerDz /dz).*dz;
        zsub        = zsub + LastLayerDz;
    end
    SubstrateDepth = MaxWaterDepth + zsub; 
    if SubstrateDepth > MaxSubstrateDepth
        MaxSubstrateDepth = SubstrateDepth;
    end
end

% Maximum Depth for Plot -> zmplt
   % zmplt is only depth of the computational grid over which results are
   % plotted ... in this case we default to the entire environment above the
   % attenuation layer
if RunDef.RAM.zmplt < 0  % automatically define plot depth range
   % define limit as max depth + substrate thickness (not inclduing
   % attenuation layer)
   RunDef.RAM.zmplt = MaxSubstrateDepth;
end
% Revision 1.3 - extend zmplot to next (plot) depth increment
% RunDef.RAM.zmplt = RunDef.RAM.zmplt + ndz * dz;
% Revision 2.2 - extend zmplot to next (plot) depth increment 
RunDef.RAM.zmplt = RunDef.RAM.zmplt + 2 * ndz * dz;

%
% Maximum Depth for PE calc ->  zmax 
   % zmax is the z-limit for the PE calc from top of the water column to the
   % bottom of the last substrate layer (including the attentuation layer if,
   % as recommended, this is included)
zmax  = MaxSubstrateDepth + AttenLayerDz;

%
% GENERAL RUN PARAMETERS - WRITE
% ==============================

fprintf(RamIpFile, '%s\n', RunDef.Title);
fprintf(RamIpFile, '%f\t%f\t%f\t\t%s\n'  , Freq       , RunDef.Zs    , RunDef.RAM.zr ,                  'f, zs, zr');
fprintf(RamIpFile, '%f\t%f\t%d\t\t%s\n'  , RunDef.RMax, dr           , ndr           ,                  'rmax, dr, ndr');
fprintf(RamIpFile, '%f\t%f\t%d\t%f\t%s\n', zmax       , dz           , ndz           , RunDef.RAM.zmplt,'zmax dz, ndz, zmplot');
switch RunDef.RAM.Species
   case {'RAM', 'RAMGeo', 'RAMSurf'}
      fprintf(RamIpFile, '%f\t%d\t%d\t%f\t%s\n', RunDef.RAM.c0, RunDef.RAM.np, RunDef.RAM.ns  , RunDef.RAM.rs   , 'c0, np, ns, rsc');
   case {'RAMS', 'RAMSGeo'}
      fprintf(RamIpFile, '%f\t%d\t%d\t%f\t%s\n', RunDef.RAM.c0, RunDef.RAM.np, RunDef.RAM.irot, RunDef.RAM.theta, 'c0, np, irot, theta');
end
% surface
switch RunDef.RAM.Species
   case 'RAMSurf'
      fprintf(RamIpFile, '%f\t%f\t\t\t%s\n'   , SurfData(1,1), SurfData(2,1), 'free surface data {r,z} in [m, m]');
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

for iEnv = 1:NEnv
    Env = EnvArr{iEnv };
    ProfileRange = RangeVec(iEnv );

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
   %         4. halfspaces are homogeneous
   %
   nlayers = GetNumLayers(Env);
  % zbuffer = 0;   % appears to no tbe used
   z       = [];
   cbp     = [];
   cbs     = [];
   rhob    = [];
   attnp   = [];
   attns   = [];
   % water layer first ...
   layer   = GetLayer(Env, 1);
   zw      = GetProfile(layer, 'Z');
   cw      = GetProfile(layer, 'Cp');
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
      switch RunDef.RAM.Species
         case {'RAMS', 'RAMSGeo'}
            cbs     = GetProfile(layer, 'Cs') ;
            attns   = GetProfile(layer, 'As') ;
      end
      % further layers up until and including 2nd last
      for ii = 3:nlayers-1
         layer = GetLayer(Env, ii);
         z       = [z     , z(end) + GetProfile(layer, 'Z')];
         cbp     = [cbp   , GetProfile(layer, 'Cp') ];
         rhob    = [rhob  , GetProfile(layer, 'Rho')];
         attnp   = [attnp , GetProfile(layer, 'Ap') ];
         switch RunDef.RAM.Species
            case {'RAMS', 'RAMSGeo'}
               cbs     = [cbs   , GetProfile(layer, 'Cs') ];
               attns   = [attns , GetProfile(layer, 'As') ];
         end
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
   switch RunDef.RAM.Species
      case {'RAMS', 'RAMSGeo'}
         Cs   = GetProfile(layer, 'Cs') ;
         As   = GetProfile(layer, 'As') ;
         % MOD v2.3 - Enforce (user-specified) minimum cs value to avoid crash 
         Cs   = max(Cs, RunDef.RAM.CsMin.*ones(size(Cs)));         
   end
   if IsHalfSpace(layer)
      % reduce homogenous halfspace to finite extent (need only two points)
      % half space may be first (substrate) layer ... 
      if isempty(z)
         refdepth = 0;      % 1st sediment layer
      else
         refdepth = z(end);
      end
      layer_top = refdepth + dz;
      layer_bas = refdepth + LastLayerDz; 
      z       = [z     , layer_top, layer_bas ];   
      cbp     = [cbp   , Cp(1)    , Cp(end)   ];
      rhob    = [rhob  , Rho(1)   , Rho(end)  ];
      attnp   = [attnp , Ap(1)    , Ap(end)   ];
      switch RunDef.RAM.Species
         case {'RAMS', 'RAMSGeo'}
            cbs     = [cbs   , Cs(1)    , Cs(end)   ];
            attns   = [attns , As(1)    , As(end)   ];
      end
   else
      if nlayers == 2  % 1st sediment layer
         % MOD v2.3 - there had been reextracted
         z       = Z  ;
         cbp     = Cp ;
         rhob    = Rho;
         attnp   = Ap ;
         switch RunDef.RAM.Species
            case {'RAMS', 'RAMSGeo'}
               cbs     = Cs;
               attns   = As;
         end
      else            % join onto previous layers
         % MOD v2.3 - i think layer.z was referenced here instead of Z - don't know why 
         z       = [z    , z(end) + Z];
         cbp     = [cbp  , Cp ];
         rhob    = [rhob , Rho];
         attnp   = [attnp, Ap ];
         switch RunDef.RAM.Species
            case {'RAMS', 'RAMSGeo'}
               cbs     = [cbs   , Cs];
               attns   = [attns , As];
         end
      end
   end   
   % now for the attenuation layer (if its there)
   if AttenLayerDz > dz 
      % note that as with any data in profile specs in Ram we dont have to
      % specify last point since ram will hold last value to maximum depth for
      % sediment (and in fact for water column as well) ... we just do it here
      % to make the run spec file explicit
      %
      % only need to specify bottom of the layer for RAM interpolation to
      % work (no discontinuties ... either constant or linear gradient) ...
      % actually we dont need to explicitly specify bottom points for those
      % parameters that stay constant ... but just for ease of interpretation
      %
      % RAMS* will use AttenPMax value for shear attenuation as well 
      %
      layer_bas = z(end) + AttenLayerDz; 
      z         = [z     , layer_bas ];   
      % hold these values from previous layer
      cbp   = [cbp   , cbp(end)  ];      
      rhob  = [rhob  , rhob(end) ];
      % increase attenuation to maximum value  
      attnp = [attnp, RunDef.RAM.AttenLayerAttenPMax];
      switch RunDef.RAM.Species
         case {'RAMS', 'RAMSGeo'}
            % hold from previous layer
            cbs     = [cbs   , cbs(end) ];
            % increase attenuation to maximum value 
            attns   = [attns, RunDef.RAM.AttenLayerAttenPMax];
      end
   end
   % convert density from kg/m³ to g/cm³ -> ÷10³
   rhob = rhob.*1e-3;
   
   % WRITE
   % =====
   if iEnv > 1
       %Range of profile only written for second and subsequenmt profiles
       fprintf(RamIpFile, '%f\t\t\t\t%s\n', ProfileRange, 'Profile range (m)');
   end
       
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
   switch RunDef.RAM.Species
      case {'RAMS', 'RAMSGeo'}
         fprintf(RamIpFile, '%f\t%f\t\t\t%s\n', z(1), cbs(1), 'shear sound speed profile in substrate {z,cbs} [m, m/s]');
         for col = 2:size(z, 2)  %should be a row vector
            fprintf(RamIpFile, '%f\t%f\n', z(col), cbs(col));
         end
         fprintf(RamIpFile, '-1\t-1\n');
   end         
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
   %
   switch RunDef.RAM.Species
      case {'RAMS', 'RAMSGeo'}
         fprintf(RamIpFile, '%f\t%f\t\t\t%s\n', z(1), attns(1), 'shear attenuation profile in substrate {z,attns} [m, dB/lambda]');
         for col = 2:size(z, 2)  %should be a row vector
            fprintf(RamIpFile, '%f\t%f\n', z(col), attns(col));
         end
         fprintf(RamIpFile, '-1\t-1\n');
   end
   %
end % Next Range Slice

% =========================================================================