function WriteToBounceFiles(Obj, Freq, EnvFile, Title, UsePltrth)
%WriteToBounceFiles(Obj, Freq, RMin, RMax, DeltaR,  EnvFile, PlpFile)
%
%Outputs acoustic layer data to an environment file used by bounce to compute bottom reflection coefft
%Note that the top layer is assumed to be the water column and is ignored
%
%Freq - frequency (Hz)
%RMin - minimum receiver range (m)
%RMax - maximum receiver range (m)
%DeltaR - receiver range step (m)
%EnvFile - pointer to environment file used by Bounce


%Minimum horizontal phase speed (m/s)
%Set to minimum p-wave in problem to get horizontal incidence
Cpmin = GetMinSoundSpeed(Obj);

%Maximum horizontal phase speed (m/s).
Cpmax = 1e9;  %Large value to get near vertical incidence  

NPts = 2000;  %Approx number of angles at which to calculate refln coefft

RMax = NPts * Cpmin / Freq;  %This "reverse engineers" the calculation in bounce to get the required number of points

%Linear interpolation of sound speed profile, acoustic halfspace (water column), attenuation in dB/wavelength
%(see kraken.hlp for more info)
OptionStr = '''NAW''';  
NLayers = length(Obj.LayerArr); 

WaterLayer = GetWaterColumn(Obj);
SoundSpeedAtBottom = GetBottomSoundSpeed(WaterLayer);
DensityAtBottom = GetBottomDensity(WaterLayer);


         
%TitleStr = ['''' Title ', ' num2str(Freq) 'Hz'''];
TitleStr = ['''' Title ''''];
         
fprintf(EnvFile, '%s\n', TitleStr);
fprintf(EnvFile, '%f\n', Freq);
fprintf(EnvFile, '%d\n', NLayers-2);  %Water column and bottom half-space aren't counted so subtract 2
fprintf(EnvFile, '%s\n', OptionStr);
fprintf(EnvFile, '0.0 %f 0.0 %f 0.0 0.0\n', SoundSpeedAtBottom, DensityAtBottom/1000);  %Top half-space properties (water column)

%Output the layers in order of increasing depth - this is  the order in which they are
%stored in the array
ZTop = 0;
for ILayer = 2:NLayers
   WriteToEnvFile(Obj.LayerArr{ILayer}, Freq, Cpmax, EnvFile, ZTop);
   ZTop = ZTop + GetLayerThickness(Obj.LayerArr{ILayer});
end

fprintf(EnvFile, '%f  %f\n', Cpmin, Cpmax);        
fprintf(EnvFile, '%f\n', RMax/1000);        
fprintf(EnvFile, '\n');

if UsePltrth

    %Write info to plot definition file - this is always the same file name and is in the current directory
    PlpFile = fopen('plotrth.plp', 'wt');
    PlpOpt = 'LDF';  %Linear scale, phase in degrees, bellhop format

    WaterLayer = GetWaterColumn(Obj);
    SoundSpeedAtBottom = GetBottomSoundSpeed(WaterLayer);

    ThetaAxStr = '0 90 10 20';
    RAxStr = '0 1 0.1 20';

    fprintf(PlpFile, '%s\n', PlpOpt);
    fprintf(PlpFile, '%f\n', SoundSpeedAtBottom);
    fprintf(PlpFile, '%s\n', ThetaAxStr);
    fprintf(PlpFile, '%s\n', RAxStr);

    fclose(PlpFile);
end


