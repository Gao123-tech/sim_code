function WriteToScooterFile(Env, RunDef, f, fid)
%WriteToScootFiles          Writes configuration file for Scooter to open target (ascii) file Environment File (fid)
%
% USAGE                      WriteToScooterFile(Env, RunDef, f, fid)
%
% INPUT                 Env     =  Environemtn Object
%                       RunDef  =  RunDEfinition object
%                       f       =  frequency in Hz
%                       fid     =  open (write) enviornment ascii file ID
%
%
% Revision 1.0     13 July       2006  ... ALM
%                  >> SEPARATION FROM WriteToScooterFiles v 0.2 (note plural!)  
%                  - formal seperation of Scooter and Fields options
%                  - Whole structures now passed (although Env is passed separately in anticipation of 
%                    possible split between Propagation and Environment structures
%                                                 

%Minimum horizontal phase speed (m/s)
%For scooter Cpmin is related to the range sample interval by RunDef.dR = Cpmin / f
Cpmin = f*RunDef.dR;

MinSpeed = GetMinSoundSpeed(Env);

if Cpmin > 0.9*MinSpeed
   Cpmin = 0.9*MinSpeed;
end


%Maximum horizontal phase speed (m/s).
%Defines minimum grazing angle - use a very large value to include rays near normal incidence
Cpmax = 1e9;  

%Maximum range passed to Scooter - this has little to do with the receiver range but instead controls
%the wavenumber spacing.  Here we define it in terms of the number of wavenumber samples, which is
% specified by NK:
NK = 2000;
RMaxScoot = NK*RunDef.dR;
if RMaxScoot < 2*RunDef.RMax
   RMaxScoot = 2*RunDef.RMax;
end

%Number of source depths
Nsd = length(RunDef.Zs);  
%Receiver depths
Nrd = length(RunDef.Zr);

%Linear interpolation of sound speed profile, vacuum top layer, attenuation in dB/wavelength
%(see kraken.hlp for more info)
OptionStr = '''NVW''';  
NLayers   = length(Env.LayerArr);   
         
         
%TitleStr = ['''' Title ', ' num2str(f) 'Hz'''];
TitleStr = ['''' RunDef.Title ''''];
         
fprintf(fid, '%s\n', TitleStr);
fprintf(fid, '%f\n', f);
fprintf(fid, '%d\n', NLayers-1);  %Bottom half-space isn't counted so subtract 1
fprintf(fid, '%s\n', OptionStr);

%Output the layers in order of increasing depth - this is the order in which they are
%stored in the array
ZTop = 0;
for ILayer = 1:NLayers
   WriteToEnvFile(Env.LayerArr{ILayer}, f, Cpmax, fid, ZTop);
   ZTop = ZTop + GetLayerThickness(Env.LayerArr{ILayer});
end

fprintf(fid, '%f  %f\n', Cpmin, Cpmax);        
fprintf(fid, '%f\n', RMaxScoot/1000);        

fprintf(fid, '%d\n%f\n', Nsd, RunDef.Zs(1:Nsd)); 
fprintf(fid, '%d\n', Nrd);
fprintf(fid, '%f  ', RunDef.Zr(1:Nrd));
fprintf(fid, '\n');