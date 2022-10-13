function Obj = AcEnvArr(EnvArrData, DirInfo)
%Constructor for AcEnvArr class - an array of environment objects corresponding to different ranges.
%EnvArrData is a structure containing the parameters for the environment array.  Fields are:
%  Name         =  string specifying name of environment array
%  EnvArr     =  cell array of AcEnvironment objects, one for each range
%  RangeVec   =  vector of horizontal ranges (m), one for each element of EnvArr
%  dRInterp   =  Specified environments will be interpolated to this range step (0 for no interpolation)
%
%Alec J Duncan, 
%Centre for Marine Science and Technology, 
%Curtin University of Technology,
%Kent Street, Bentley, Western Australia
%a.duncan@cmst.curtin.edu.au
%
%August 2004


if nargin==0
    Obj.Name = '';
    Obj.EnvArr = [];
    Obj.RangeVec = [];
    Obj.dRInterp = [];
    
    
    Obj = class(Obj, 'AcEnvArr');
    
elseif isa(EnvArrData, 'AcEnvArr')
    Obj = EnvArrData;
else
    if ischar(EnvArrData)
        if strcmpi(EnvArrData, 'Default')
            Obj.Name = 'Default environment array';
            Obj.EnvArr{1} = AcEnvironment('Default', DirInfo);
            Obj.RangeVec = 0;
            Obj.dRInterp = 0;
            
            Obj = class(Obj, 'AcEnvArr');
        else
            error(['Unknown parameter: ', EnvArrData]);
        end
    else
        Obj.Name = EnvArrData.Name;
        
        Obj.EnvArr = EnvArrData.EnvArr;
        Obj.RangeVec = EnvArrData.RangeVec;
        Obj.dRInterp = EnvArrData.dRInterp;
        
        Obj = class(Obj, 'AcEnvArr');
    end
    
end
