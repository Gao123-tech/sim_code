function WriteToKrakenFiles(Obj, Freq, RMin, RMax, DeltaR,  ZRx,  ZSource, EnvFile, FieldFile, Title, ExhaustiveSearch)
%WriteToScootFiles(Obj, Freq, RMax, DeltaR,  ZRxMin, ZRxMax, DeltaZ, ZSource, EnvFile, FieldsFile, Title)
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


%Minimum horizontal phase speed (m/s)
%Set this to zero to get Kraken to compute a sensible value automatically
%Set to minimum p-wave in problem to avoid computing interface modes
Cpmin = 0.9*GetMinSoundSpeed(Obj);
%Cpmin = 0;

%Minimum angle of incidence to include (rad).  
%If this is less than the critical angle for trapping rays Kraken will automatically increase it to 
%include only trapped modes, whereas Krakenc will attempt to compute leaky modes
ThetaMin = 20*pi/180;
%Maximum horizontal phase speed (m/s).
Cpmax = GetMaxSoundSpeed(Obj)/sin(ThetaMin);  


Nsd = length(ZSource);  %Number of source depths

%Receiver depths
Nrd = length(ZRx);

%Linear interpolation of sound speed profile, vacuum top layer, attenuation in dB/wavelength
%(see kraken.hlp for more info)
if ExhaustiveSearch
   OptionStr = '''NVW .''';
   EnvRMax = 0;
else
   OptionStr = '''NVW''';
   EnvRMax = RMax;
end

NLayers = length(Obj.LayerArr);   
         
         
%TitleStr = ['''' Title ', ' num2str(Freq) 'Hz'''];
TitleStr = ['''' Title ''''];
         
fprintf(EnvFile, '%s\n', TitleStr);
fprintf(EnvFile, '%f\n', Freq);
fprintf(EnvFile, '%d\n', NLayers-1);  %Bottom half-space isn't counted so subtract 1
fprintf(EnvFile, '%s\n', OptionStr);

%Output the layers in order of increasing depth - this is the order in which they are
%stored in the array
ZTop = 0;
for ILayer = 1:NLayers
   WriteToEnvFile(Obj.LayerArr{ILayer}, Freq, Cpmax, EnvFile, ZTop);
   ZTop = ZTop + GetLayerThickness(Obj.LayerArr{ILayer});
end

fprintf(EnvFile, '%f  %f\n', Cpmin, Cpmax);        
fprintf(EnvFile, '%f\n', EnvRMax/1000);        

fprintf(EnvFile, '%d\n%f\n', Nsd, ZSource(1:Nsd)); 
fprintf(EnvFile, '%d\n', Nrd);
fprintf(EnvFile, '%f  ', ZRx(1:Nrd));
fprintf(EnvFile, '\n');

%Now output the required information to the .flp file
   %Options for processing Kraken output with Field
   FieldTitle = '/,';
   FieldOpt = '''RA''';  %R=point source, A = adiabatic modes (see field.hlp)
   
   NModes = 9999;        %Very large to use all computed modes
   
   NProf = 1;				 %NProf = 1, RProf = 0 for range independent problems
   RProf = 0.0;
   
   if RMax == RMin
       NRange = 1;
   else
       NRange = round((RMax - RMin)/DeltaR) + 1;  %Number of ranges
   end
      
   RxDispVec = zeros(1,Nrd);
      
   fprintf(FieldFile, '%s\n', FieldTitle);
   fprintf(FieldFile, '%s\n', FieldOpt);
   fprintf(FieldFile, '%d\n', NModes);
   fprintf(FieldFile, '%d\n%f\n', NProf, RProf);
   fprintf(FieldFile, '%d\n', NRange);
   fprintf(FieldFile, '%f  %f /\n', RMin/1000, RMax/1000);
   fprintf(FieldFile, '%d\n', Nsd);
   fprintf(FieldFile, '%f\n', ZSource);
   fprintf(FieldFile, '%d\n', Nrd);
   fprintf(FieldFile, '%f  ', ZRx);
   fprintf(FieldFile, '\n');
   fprintf(FieldFile, '%d\n', Nrd);
   fprintf(FieldFile, '%f  ', RxDispVec);
   fprintf(FieldFile, '\n');
