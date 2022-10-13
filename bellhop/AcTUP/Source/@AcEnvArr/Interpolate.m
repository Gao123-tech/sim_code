function InterpObj = Interpolate(EnvArrObj);
%This function horizontally interpolates between the environments in an AcEnvArr object to produce a new AcEnvArr object
%with environments specified at smaller range steps.
%
%The interpolation strategy is as follows:
%1. Thicknesses of layers are linearly interpolated.  This assumes all environments have the same number of layers.
% Where this isn't the case the interpolated environments will have a number of layers equal to the lesser of the number
% of layers in the two adjacent environments.  Layers are matched in order from the top layer down.  This means
% discontinuous intermediate layers aren't supported.
%
%2. Acoustic properties within a layer are linearly interpolated between the points on the bounding profiles that are closest
% in depth from the top of the layer.  The bounding profile with the greatest thickness is used to define the number
% of points (in depth) of the interpolated profiles.
%
%  A J Duncan, 6/8/2004

% Find out how many environments there are and interpolate them if required
NEnv = GetNumberOfElements(EnvArrObj);
EnvArr = EnvArrObj.EnvArr;
RangeVec = EnvArrObj.RangeVec;

if (NEnv <= 1) | (EnvArrObj.dRInterp <= 0)
    %Don't interpolate
    InterpObj = EnvArrObj;
else
    RMax = EnvArrObj.RangeVec(end);
    RInterp = 0:EnvArrObj.dRInterp:RMax;  %Desired output ranges
    P.EnvArr = [];   %This will be a cell array containing interpolated environemtns
    NInterp = 0;
    
    %Deal with ranges less than minimum - make these same as first environment
    ISmall = find(RInterp <= RangeVec(1));
    for ISub = 1:length(ISmall)
        NInterp = NInterp + 1;
        P.EnvArr{NInterp} = EnvArr{1};
    end
    
    %Deal with intermediate ranges - linearly interpolate between adjacent environments
    for ISub = 1:NEnv - 1
        IThis = find((RInterp > RangeVec(ISub)) & (RInterp <= RangeVec(ISub+1)));
        %Call the AcEnvironment interpolation method to do the actual interpolation
        InterpEnv = Interpolate(EnvArr{ISub}, EnvArr{ISub+1}, RangeVec(ISub), RangeVec(ISub+1), RInterp(IThis));
        
        %Load results
        NThis = length(InterpEnv);
        for IThis = 1:NThis
            NInterp = NInterp + 1;
            P.EnvArr{NInterp} = InterpEnv{IThis};
        end
    end
    
    %Deal with ranges less than minimum - make these same as last environment
    IBig = find(RInterp > RangeVec(end));
    for ISub = 1:length(IBig)
        NInterp = NInterp + 1;
        P.EnvArr{NInterp} = EnvArr{end};
    end

    %Build the required AcEnvArr object
    P.RangeVec = RInterp;
    P.dRInterp  = 0;  %Prevent it the interpolate array from being re-interpolated
    P.Name = [EnvArrObj.Name ' - interpolated'];
    DirInfo = GetDirInfo(EnvArrObj);
    
    InterpObj = AcEnvArr(P, DirInfo);
end
