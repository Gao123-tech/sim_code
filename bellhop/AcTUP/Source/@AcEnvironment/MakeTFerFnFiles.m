function EnvObj = MakeTFerFnFiles(EnvObj, Source, VesselInfo, RMin, RMax,  ZRxMin, ZRxMax, MinBottomBounce, AllowRebuild, PropgnCode)
%
%Makes the files required to calculate the transfer functions for the specified source and 
%over the specified range and receiver depth intervals.  
%
%Run this function for each source before calling GetTFerFn
%
%This is done by running the Scooter fast-field program for a number of frequencies and building a 3D interpolation
%table
%
%Source - Source object
%RMin - minimum horizontal receiver range (m)
%RMax - maximum horizontal receiver range (m)
%ZRxMin - minimum receiver depth (m)
%ZRxMin - maximum receiver depth (m)
%MinBottombounce - minimum number of bottom bounces to use when calculating the required length of the impulse response
%AllowRebuild - 1 to allow rebuilding of transfer function files, 0 to prevent rebuilding
%PropgnCode (optional) - determines acoustic propagation code to be run:
%     'Scooter' (default)  fast-field program
%     'Kraken'    Kraken normal mode code
%     'Krakenc'   Complex arithmetic normal mode code

%Transfer function is set to zero at frequencies for which the source spectrum is smaller than this fraction of the maximum
%spectral level
SpectrumThreshold = 1e-6;  
if (nargin < 9) | ((nargin >= 9) & isempty(PropgnCode))
   PropgnCode = 'Scooter';
end

CwMax = GetMaxSoundSpeed(GetWaterColumn(EnvObj)); %Max sound speed in water column
DirInfo = GlblGetDirectoryInfo;

[Xs, Ys, Zs] = GetPosition(Source);  %Source position in vessel coords
ZSource = Zs + VesselInfo.VesselDepth;  %Source depth relative to water surface

%Estimate the maximum impulse response length - this is taken as the maximum delay of the multipath
%with the specified number of bottom interactions at minimum range.
Zw = GetWaterDepth(EnvObj);
CMin = GetMinSoundSpeed(EnvObj);
CMax = GetMaxSoundSpeed(EnvObj);

DeltaZs = ZRxMax + ZSource + 2* MinBottomBounce * Zw;

ThetaBot = atan(RMax / DeltaZs);
ThetaCrit = asin(CwMax / CMax);

if ThetaBot > ThetaCrit
   ThetaCalc = ThetaCrit;
else
   ThetaCalc = ThetaBot;
end

RSlant = ZRxMax/sin(ThetaCalc);

TorMax = RSlant / CMin;
TorMin = RMin / CMax;
MaxImpulseLength = (TorMax - TorMin)*1.1;

FVec = CalcFreqVec(Source, MaxImpulseLength);

FMin = FVec(1);
FMax = FVec(end);
dF = FVec(2) - FVec(1);

SourceName = GetLabel(Source);

%Check whether we have already generated files appropriate for this data set
MatchIndex = lFindMatchingTFFn(EnvObj, dF, FMin, FMax, RMin, RMax, ZRxMin, ZRxMax, ZSource);

if ~isempty(MatchIndex)
   %We've already run the propagation code for a transfer fn that will do the job
   %All we have to do is append the current source name to the list of source names
   Data = EnvObj.TFFnData{MatchIndex};
   NextName = length(Data.SourceName) + 1;
   Data.SourceName{NextName} = SourceName;
   EnvObj.TFFnData{MatchIndex} = Data;
else
   %Need to run the propagation code - first save the parameters

	BaseName = LibStrToValidFName([EnvObj.Name '_' SourceName]);
   FName = [DirInfo.AcWorkspace BaseName];
   
   Data.SourceName{1} = SourceName;
   Data.dF = dF;
   Data.FMin = FMin;
   Data.FMax = FMax;
   Data.RMin = RMin;
   Data.RMax = RMax;
   Data.ZRxMin = ZRxMin;
   Data.ZRxMax = ZRxMax;
   Data.ZSource = ZSource;
   Data.FileName = FName;
   
   INext = length(EnvObj.TFFnData) + 1;
   EnvObj.TFFnData{INext} = Data;
   if AllowRebuild
	   DeltaR = CwMax / (2.1*FMax);  %Range spacing is slightly less than half wavelength at highest frequency
		if DeltaR > RMax / 100;
   		    DeltaR = RMax/100;
		end
      
        if (RMax - RMin) < 4*DeltaR
           RMin = RMin - DeltaR;
           if RMin < 0;
              RMin = 0;
           end
           RMax = RMin + 5*DeltaR;
        end
        %RMin = 100;  %**************************Debug only
        %RMax = RMax*1.5;
      
        DeltaZ = CwMax / (2.1*FMax);	%Depth spacing is slightly less than half wavelength at highest frequency

		NZ = ceil((ZRxMax - ZRxMin) / DeltaZ) + 1;
		ZRxVec = ZRxMin + [0:NZ-1]*DeltaZ;
        
        %Compute the power spectral density of the source signal so that we can determine
        %which frequencies have significant energy and therefore need to be computed
        [Spectrum, SpecFreq] = GetSpectrum(Source, 1);
        MaxSpec = max(Spectrum);
        
        InterpSpec = interp1(SpecFreq, Spectrum, FVec);
        
		%Run the propagation code for each frequency
   	    AppendFile = 0;
		for IFreq = 1:length(FVec)
            if (InterpSpec(IFreq) / MaxSpec) > SpectrumThreshold
       		    Freq = FVec(IFreq);
		        disp(' ');
   		        disp(['Frequency = ' num2str(Freq)]);
                [Status, ErrStr] = RunPropgnCode(EnvObj, Freq, RMin, RMax, DeltaR,  ZRxVec, ZSource, FName, ...
                        AppendFile, PropgnCode, '', 0, [], 1);
	   	        if ~Status
   	   	            error(['Error running propagation code: ' ErrStr]);
	   	        end
		        AppendFile = 1;  
            end
        end
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function MatchIndex = lFindMatchingTFFn(EnvObj, dF, FMin, FMax, RMin, RMax, ZRxMin, ZRxMax, ZSource)
%Looks for a transfer function run that meets the requirements of the current run
%Returns index to run data, or [] if not found.
MatchIndex = [];
Done = 0;
Index = 1;
while ~Done
   if Index > length(EnvObj.TFFnData)
      Done = 1;
   else
      Data = EnvObj.TFFnData{Index};
      if (dF >= Data.dF) & ...
         	(FMin >= Data.FMin) & ...
         	(FMax <= Data.FMax) & ...
		   	(RMin >= Data.RMin) & ...
   			(RMax <= Data.RMax) & ...
         	(ZRxMin >= Data.ZRxMin) & ...
            (ZSource == Data.ZSource) & ...
            exist(Data.FileName, 'file')
         
      	MatchIndex = Index;
         Done = 1;
      else
         Index = Index+1;
      end
   end
end

         