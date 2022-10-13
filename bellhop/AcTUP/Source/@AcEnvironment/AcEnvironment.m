function Obj = AcEnvironment(EnvData, DirInfo)
%Constructor for acoustic environment class.
%EnvData is a structure containing the parameters for the environment.  Fields are:
%  Name         =  string specifying name of environment
%  LayerArr     =  cell array of AcLayer objects, one for each acoustic layer (including bottom halfspace)
%
%Alec J Duncan, 
%Centre for Marine Science and Technology, 
%Curtin University of Technology,
%Kent Street, Bentley, Western Australia
%a.duncan@cmst.curtin.edu.au
%
%May 2002


if nargin==0
   Obj.Name = '';

   Obj.Gravity = [];
   Obj.PVapour = [];
   Obj.PAtm  = [];
   Obj.LayerArr = [];
   Obj.TFFnData = [];
   Obj.DirInfo = [];
   
   Obj = class(Obj, 'AcEnvironment');
   
elseif isa(EnvData, 'AcEnvironment')
   Obj = EnvData;
else
   if ischar(EnvData)
       if strcmpi(EnvData, 'Default')
            EnvData = [];
            %Set up some sensible defaults
            EnvData.Name = 'Default environment';

            L.Name = 'Water column';
            L.ZTop = 0;
            L.IsHalfSpace = 0;
            L.RMSRough = 0;
            L.Z = [0 150];
            L.Cp = 1500;
            L.Cs = 0;
            L.Rho = 1024;
            L.Ap = 0;
            L.As = 0;
            EnvData.LayerArr{1} = AcLayer(L);
      
            L.Name = 'Bottom halfspace';
            L.ZTop = 150;
            L.IsHalfSpace = 1;
            L.RMSRough = 0;
            L.Z = 0;
            L.Cp = 1749;
            L.Cs = 0;
            L.Rho = 1941;
            L.Ap = 0;
            L.As = 0;
            EnvData.LayerArr{2} = AcLayer(L);
        end
    end
   
      
   Obj.Name = EnvData.Name;

   %Dummy parameters for compatibility with old versions
   if isfield(EnvData, 'Gravity');
       Obj.Gravity = EnvData.Gravity;
   else
       Obj.Gravity = [];
   end
   
   if isfield(EnvData, 'PVapour');
       Obj.PVapour = EnvData.PVapour;
   else
       Obj.PVapour = [];
   end
   if isfield(EnvData, 'PAtm');
       Obj.PAtm = EnvData.PAtm;
   else
       Obj.PAtm = [];
   end
  
   %Layer data
   Obj.LayerArr = EnvData.LayerArr;
   %This field is used by class methods to maintain a database of conditions for which transfer function data
   %has been generated
	Obj.TFFnData = [];  
    Obj.DirInfo = DirInfo;
   Obj = class(Obj, 'AcEnvironment');
end
