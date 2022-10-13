function WriteToBellhopFiles(Obj, Freq, RMin, RMax, DeltaR,  ZRx,  ZSource, EnvFile, BellhopParams, Title)
%WriteToScootFiles(Obj, Freq, RMax, DeltaR,  ZRxMin, ZRxMax, DeltaZ, ZSource, EnvFile, FieldsFile)
%
%Outputs acoustic layer data to an environment file and a .flp for use by Scooter and Fields
%
%Freq - frequency (Hz)
%RMin - minimum receiver range (m)
%RMax - maximum receiver range (m)
%DeltaR - receiver range step (m)
%ZRx - vector of receiver depths (m)
%ZSource - source depth (m)
%EnvFile - pointer to environment file used by Kraken(c)
%FlpFile - pointer to .flp file used by Field
%
% Revision 0                           ... AJD 
% Revision 0.1x    06 December   2004  ... ???
% Revision 0.2     22 June       2006  ... ALM
%                  >> FIX FOR Mike Porter's AT Release 2006
%                  - Change bathymetry switch charater from 'B' to '*' 
%                    Note that this is the correct switch character for both 2006 and earlier version of Bellhop 
%                    [see both 2002 and 2005 .hlp files !! - note these are ascii files not windows help files]
%                    ... BUT for some reason (haven't checked source code) 'B' works as well as '*' in the earlier version 
%                    ... NOT SO FOR 2006 !! - runs but ignores bathymetry
%                  - Also change first line of help text block from WriteToScootFiles to WriteToBellhopFiles


%Minimum horizontal phase speed (m/s) - uses the lowest shear or sound speed in the problem
Cpmin = GetMinSoundSpeed(Obj);

LambdaMin = Cpmin/Freq;

%Compute the maximum horizontal phase speed - from the maximum grazing angle at the source and the maximum sound speed in the
%water column
WaterLayer = GetWaterColumn(Obj);
CAcMax = GetMaxSoundSpeed(WaterLayer);

ThetaGMax = max(abs([BellhopParams.StartAngle BellhopParams.EndAngle]));
if ThetaGMax > 0.999*pi/2
    ThetaGMax = 0.999*pi/2;
end

%Maximum horizontal phase speed (m/s).
Cpmax = CAcMax/cos(ThetaGMax);  


Nsd = length(ZSource);  %Number of source depths

%Receiver depths
Nrd = length(ZRx);

if RMax == RMin
    NR = 1;
else
    NR = round((RMax - RMin)/DeltaR) + 1;  %Number of ranges
end

if BellhopParams.UseBathyFile
    BottomOpt = 'F*';
else
    BottomOpt = 'F';    
end

SigmaBot = 0;          %Bottom roughness (ignored)


%N2-linear interpolation of sound speed profile, vacuum top layer, attenuation in dB/wavelength, Thorpe volume attenuation formula
%(see bellhop.hlp for more info)
%OptionStr = '''NVT''';  %Worked with earlier version of Acoustic toolbox
OptionStr = '''NVWT''';  
NLayers = 1;   
         
         
%TitleStr = ['''' Title ', ' num2str(Freq) 'Hz'''];
TitleStr = ['''' Title ''''];

RunType = BellhopParams.RunType;

RunStr = ['''' RunType ''''];

StepSize = BellhopParams.StepSize * LambdaMin;

RBox = 1.01 * RMax;
ZBox = 1.01 * GetWaterDepth(Obj);
         
fprintf(EnvFile, '%s\n', TitleStr);
fprintf(EnvFile, '%f\n', Freq);
fprintf(EnvFile, '%d\n', 1);  %Bellhop only uses the water column layer
fprintf(EnvFile, '%s\n', OptionStr);

%Output the water column layer only.  The other layers are handled by way of the bottom reflection
%coefficient file which is computed by Bounce
WriteToEnvFile(WaterLayer, Freq, Cpmax, EnvFile, 0);


fprintf(EnvFile, '%s  %f\n', BottomOpt, SigmaBot);        

fprintf(EnvFile, '%d\n', Nsd);  %f\n', Nsd, ZSource(1:Nsd)); %Source depths
fprintf(EnvFile, '%f  ', ZSource(1:Nsd));
fprintf(EnvFile, '\n');

fprintf(EnvFile, '%d\n', Nrd);  %Receiver depths
fprintf(EnvFile, '%f  ', ZRx(1:Nrd));
fprintf(EnvFile, '\n');

fprintf(EnvFile, '%d\n%f %f /\n', NR, RMin/1000, RMax/1000);  %Receiver ranges

fprintf(EnvFile, '%s\n', RunStr);

fprintf(EnvFile, '%d\n%f %f /\n', BellhopParams.NBeams, BellhopParams.StartAngle, BellhopParams.EndAngle);

fprintf(EnvFile, '%f %f %f\n', StepSize, ZBox, RBox/1000);

